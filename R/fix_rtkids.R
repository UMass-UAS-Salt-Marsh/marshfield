#' Rename duplicated RTK ids according to dedup_rtk.txt
#'
#' Parameter file:
#' - `dedup_rtk.txt` Drop duplicated RTKs with same rtk_id: `rtk_id`, `seq`, `new`, `reason`
#'
#' @param rtk RTK data frame
#' @param path Base path
#' @returns RTK data frame
#' @export


fix_rtkids <- function(rtk, path) {


   dup_rtk <- read.table(file.path(path, 'pars/dedup_rtk.txt'), sep = '\t', header = TRUE, quote = '')
   rtk <- rtk[order(rtk$date), ]                      # sort RTKs by date so sequence works out (assumed to be in order in dedup_rtk.txt)

   old <- rtk$rtk_id
   u <- unique(dup_rtk$rtk_id)
   for(i in u) {
      b <- rtk$rtk_id %in% i
      if(any(b)) {
         if(sum(b) != sum(dup_rtk$rtk_id %in% i))
            stop('You have the wrong number of duplicates in dedup_rtk.txt for ', i)
         rtk$rtk_id[b] <- dup_rtk$new[dup_rtk$rtk_id %in% i]
      }
   }

   cat('\nRenamed ', sum(rtk$rtk_id != old, na.rm = TRUE), ' RTK ids in rtk\n', sep = '')

   rtk
}
