#' Sort plots in canonical order
#
#' Sort order is field day, tablet id, section, plot number. Days are defined in field_days.txt column day, as J21, J22, ..., A07.
#'
#' @param plots Plots data frame. Requires column plot_id
#' @param path Project file path
#' #' @returns Plots data frame, sorted
#' @export


sort_plots <- function(plots, path) {


   days <- read.table(file.path(path, 'pars/field_days.txt'), sep = '\t', header = TRUE)
   days$day_n <- 1:nrow(days)

   y <- strsplit(toupper(plots$plot_id), '-')
   y1 <- sapply(y, '[[', 1)                     # part 1: tablet and date
   y2 <- sapply(y, '[[', 2)                     # part 2: section
   y3 <- sapply(y, '[[', 3)                     # part 3: plot number

   plots$tablet <- substr(y1, 1, 1)
   plots$day <- substr(y1, 2, 4)
   plots$section <- y2
   plots$plot_no <- y3

   plots <- merge(plots, days, by = 'day', all.x = TRUE, all.y = FALSE)
   plots <- plots[order(plots$day_n, plots$tablet, plots$section, plots$plot_no), ]
   row.names(plots) <- NULL
   plots

}
