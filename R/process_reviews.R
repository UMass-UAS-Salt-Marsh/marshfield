#' If review data are available, drop rejected plots
#'
#' @param plots Plots data frame
#' @param dropped Dropped plots data frame
#' @param path Base path
#' @returns Named list, `plots` = plots data frame, `dropped` = dropped plots data frame
#' @export


process_reviews <- function(plots, dropped, path) {


   f <- file.path(path, 'pars', 'review.txt')

   if(file.exists(f)) {
      cat('\nJoining in review fields from view_plots()...\n')
      review <- read.table(f, sep = '\t', header = TRUE, quote = '')
      keep <- c('plot_id', 'reviewed', 'problems', 'rejected', 'split', 'coverr', 'comments')
      plots <- merge(plots, review[, keep], by = 'plot_id', all.x = TRUE, all.y = FALSE)

      plots <- sort_plots(plots, path)                                  # sort plots by date, tablet, section, and plot number

      cols <- c('reviewed', 'problems', 'split', 'coverr')
      plots[cols][is.na(plots[cols])] <- FALSE
      plots$comments[is.na(plots$comments)] <- ''

      r <- plots$rejected
      r[is.na(r)] <- FALSE
      cat('\n', sum(r), ' plots that were rejected on review dropped; ', sum(!r), ' remaining. Dropped plots:\n', sep = '')
      print(plots[r, c('plot_id', 'comments')], row.names = FALSE, right = FALSE)

      dropped <- bind_rows(dropped, plots[r, ])

      plots <- plots[!r, !names(plots) %in% 'rejected']
   }

   list(plots = plots, dropped = dropped)
}
