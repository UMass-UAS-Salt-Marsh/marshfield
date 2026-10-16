#' Try to lock a site for editing
#'
#' Locks are per-site, so several people can assess different sites at once. The lock is a
#' `filelock` lock on `gcp_review.lock`; a sidecar file records who holds it, so the message can
#' name them.
#'
#' There is exactly one lock file per site, and a refusal is always taken at face value. An
#' earlier version treated "refused but no sidecar" as a lock wedged by the file server and
#' quietly moved to a second lock file -- which defeated the lock entirely, because two sessions
#' could each end up holding a different file and neither would see the other. A lock with a
#' bypass isn't a lock. The sidecar may be missing for a moment after a session takes the lock,
#' or if its write failed, and that must never be read as "nobody is there".
#'
#' @param site_path Path to the site
#' @returns List of lock (a filelock lock, or NULL if someone else holds it), info (list of
#'   user, host, time from the sidecar, or NULL), and file (the lock path)
#' @importFrom filelock lock unlock
#' @keywords internal
#' @noRd


gcp_lock <- function(site_path) {

   f <- file.path(site_path, 'gcp_review.lock')
   lk <- tryCatch(filelock::lock(f, timeout = 0), error = function(e) NULL)

   if(is.null(lk))                                                 # someone holds it, full stop
      return(list(lock = NULL, info = gcp_lock_info(f), file = f))

   gcp_write_lock_info(f)
   list(lock = lk, info = NULL, file = f)
}



#' Read the lock sidecar
#'
#' @param file Lock file path
#' @returns List of user, host, pid, time, or NULL if there's no readable sidecar
#' @importFrom utils read.table
#' @keywords internal
#' @noRd


gcp_lock_info <- function(file) {

   f <- paste0(file, '.info')
   if(!file.exists(f))
      return(NULL)
   x <- tryCatch(read.table(f, sep = '\t', header = TRUE, quote = '', comment.char = '',
                            colClasses = 'character'),
                 error = function(e) NULL)
   if(is.null(x) || nrow(x) == 0)
      return(NULL)
   as.list(x[1, ])
}



#' Write the lock sidecar
#'
#' @param file Lock file path
#' @returns TRUE if successful
#' @importFrom utils write.table
#' @keywords internal
#' @noRd


gcp_write_lock_info <- function(file) {

   x <- data.frame(user = Sys.info()[['user']], host = Sys.info()[['nodename']],
                   pid = Sys.getpid(), time = format(Sys.time(), '%Y-%m-%d %H:%M:%S'))
   tryCatch({
      write.table(x, paste0(file, '.info'), sep = '\t', row.names = FALSE, quote = FALSE)
      TRUE
   }, error = function(e) FALSE, warning = function(w) FALSE)
}



#' Release a lock and remove its sidecar
#'
#' @param lk List from `gcp_lock`
#' @returns Nothing
#' @keywords internal
#' @noRd


gcp_unlock <- function(lk) {

   if(is.null(lk) || is.null(lk$lock))
      return(invisible())
   tryCatch(filelock::unlock(lk$lock), error = function(e) NULL)
   suppressWarnings(file.remove(paste0(lk$file, '.info')))
   invisible()
}



#' Describe who holds a lock
#'
#' @param info List from `gcp_lock_info`, or NULL
#' @returns A sentence naming the holder and how long they've had it
#' @keywords internal
#' @noRd


gcp_lock_who <- function(info) {

   if(is.null(info))
      return('Site locked by another user; viewing only')

   age <- tryCatch({
      m <- as.numeric(difftime(Sys.time(), as.POSIXct(info$time), units = 'mins'))
      if(is.na(m) | m < 0) '' else
         if(m < 60) paste0(' (', round(m), ' min ago)') else
            paste0(' (', round(m / 60, 1), ' hours ago)')
   }, error = function(e) '')

   paste0('Site locked by ', info$user, ' on ', info$host, ', since ', info$time, age,
          '; viewing only')
}



#' Force a lock open
#'
#' For the case where someone left the app open and went home (or skiing).
#'
#' First simply tries to take the lock: if the holder's session has ended, the operating system
#' has already released it and we get it straight away. If that fails, the holder is alive, and
#' the only thing that can free the lock is the holder itself -- so the caller leaves a claim
#' (`gcp_claim`) and waits. Deleting the lock file or the sidecar would not help: the lock is
#' held on the open file handle, not on the directory entry, and dropping the sidecar would only
#' destroy the record of who to go and ask.
#'
#' @param site_path Path to the site
#' @returns List from `gcp_lock`, with `forced` TRUE if we now hold the lock
#' @keywords internal
#' @noRd


gcp_force_unlock <- function(site_path) {

   lk <- gcp_lock(site_path)
   lk$forced <- !is.null(lk$lock)
   lk
}



#' Claim a lock held by a running session
#'
#' Writes a claim file asking whoever holds the lock to give it up. A running `gcp` polls for
#' this (see `gcp_check_claim`) and yields, so a session left open over the weekend doesn't keep
#' a site locked for everyone else.
#'
#' @param site_path Path to the site
#' @returns TRUE if the claim was written
#' @importFrom utils write.table
#' @keywords internal
#' @noRd


gcp_claim <- function(site_path) {

   x <- data.frame(user = Sys.info()[['user']], host = Sys.info()[['nodename']],
                   pid = Sys.getpid(), time = format(Sys.time(), '%Y-%m-%d %H:%M:%S'))
   tryCatch({
      write.table(x, file.path(site_path, 'gcp_review.lock.claim'), sep = '\t',
                  row.names = FALSE, quote = FALSE)
      TRUE
   }, error = function(e) FALSE, warning = function(w) FALSE)
}



#' Has someone else claimed the lock we hold?
#'
#' @param site_path Path to the site
#' @returns List of user, host, pid, time if another session has claimed it, else NULL
#' @importFrom utils read.table
#' @keywords internal
#' @noRd


gcp_check_claim <- function(site_path) {

   f <- file.path(site_path, 'gcp_review.lock.claim')
   if(!file.exists(f))
      return(NULL)

   x <- tryCatch(read.table(f, sep = '\t', header = TRUE, quote = '', comment.char = '',
                            colClasses = 'character'),
                 error = function(e) NULL)
   if(is.null(x) || nrow(x) == 0)
      return(NULL)

   if(identical(as.integer(x$pid[1]), Sys.getpid()))               # our own claim
      return(NULL)

   as.list(x[1, ])
}



#' Remove a claim
#'
#' @param site_path Path to the site
#' @returns Nothing
#' @keywords internal
#' @noRd


gcp_clear_claim <- function(site_path) {

   suppressWarnings(file.remove(file.path(site_path, 'gcp_review.lock.claim')))
   invisible()
}
