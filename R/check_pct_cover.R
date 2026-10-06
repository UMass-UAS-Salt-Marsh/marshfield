#' Check percent cover for out-of-range values
#'
#' @param plots Plots data frame
#' @param pct_cover Percent cover data frame
#' @importFrom dplyr group_by summarise
#' @export


check_pct_cover <- function(plots, pct_cover) {


   x <- pct_cover |>
      group_by(plot_id) |>
      summarise(sum = sum(pct_cover)) |>
      data.frame()

   x$sum[is.na(x$sum)] <- 0                     # NAs are really 0 recorded percent cover

   over <- x[x$sum > 100, ]
   if(nrow(over) > 0) {
      cat('\n', nrow(over), ' plots with > 100 percent cover:\n', sep = '')
      print(over, row.names = FALSE)
   }

   zero <- x[x$sum == 0, ]
   if(nrow(zero) > 0) {
      cat('\n', nrow(zero), ' plots with zero percent cover:\n', sep = '')
      y <- merge(zero, plots, by = 'plot_id')[, c('plot_id', 'subclass')]
      print(y[order(y$subclass), ], row.names = FALSE, quote = FALSE)
   }

}
