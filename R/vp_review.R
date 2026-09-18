#' Read review table for view_plots
#'
#' Reads `review.txt` if it exists, and returns a review table with one row for each plot in
#' `plot_ids` (in that order), plus an `extra` table of rows for plots that aren't in `plot_ids`
#' (e.g., plots that have since been dropped or renamed) so they're preserved when writing.
#'
#' Newlines in comments are stored as `\n` so each plot stays on one line.
#'
#' @param file Path to review.txt
#' @param plot_ids Plot ids of all plots
#' @returns List of review (data frame aligned with plot_ids) and extra
#' @importFrom utils read.table
#' @keywords internal
#' @noRd


vp_read_review <- function(file, plot_ids) {

   review <- data.frame(plot_id = plot_ids, reviewed = FALSE, problems = FALSE, rejected = FALSE,
                        keywords = '', comments = '', rotation = NA_real_, center_x = NA_real_,
                        center_y = NA_real_)
   extra <- review[0, ]

   if(file.exists(file)) {
      x <- read.table(file, sep = '\t', header = TRUE, quote = '', comment.char = '',
                      colClasses = 'character', na.strings = character(0))
      for(f in names(review)[!names(review) %in% names(x)])                    # add any missing columns
         x[[f]] <- ''
      for(f in c('reviewed', 'problems', 'rejected'))
         x[[f]] <- x[[f]] %in% c('TRUE', 'T', 'true', '1')
      for(f in c('rotation', 'center_x', 'center_y'))
         x[[f]] <- suppressWarnings(as.numeric(x[[f]]))
      x$comments <- gsub('\\n', '\n', x$comments, fixed = TRUE)
      x <- x[, names(review)]

      i <- match(x$plot_id, plot_ids)
      review[i[!is.na(i)], ] <- x[!is.na(i), ]
      extra <- x[is.na(i), ]
   }

   list(review = review, extra = extra)
}



#' Write review table for view_plots
#'
#' Writes rows for plots that have any review information, plus extra rows for plots that
#' aren't in the current dataset.
#'
#' @param review Review table
#' @param extra Extra rows to preserve
#' @param file Path to review.txt
#' @returns TRUE if successful
#' @importFrom utils write.table
#' @keywords internal
#' @noRd


vp_write_review <- function(review, extra, file) {

   x <- rbind(review, extra)
   keep <- x$reviewed | x$problems | x$rejected | x$keywords != '' | x$comments != '' |
      !is.na(x$rotation) | !is.na(x$center_x)
   x <- x[keep, ]
   x <- x[order(x$plot_id), ]

   x$keywords <- gsub('[\t\r\n]+', ' ', x$keywords)
   x$comments <- gsub('\r', '', gsub('\t', ' ', x$comments, fixed = TRUE), fixed = TRUE)
   x$comments <- gsub('\n', '\\n', x$comments, fixed = TRUE)

   tryCatch({
      write.table(x, file, sep = '\t', row.names = FALSE, quote = FALSE, na = '')
      TRUE
   }, error = function(e) FALSE, warning = function(w) FALSE)
}
