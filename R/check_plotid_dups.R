#' Check for plot ids shared by more than one vegetation record and throw an error
#'
#' The plots table has one row per species, so plot ids are repeated within a vegetation record.
#' A plot id that shows up in more than one vegetation record means two different plots ended up
#' with the same id; collapsing the table would silently merge them.
#'
#' @param plots Plots data frame, before collapsing to one row per plot
#' @export


check_plotid_dups <- function(plots) {


   n <- tapply(plots$vegetation_record_id, plots$plot_id, function(x) length(unique(x)))   # number of vegetation records for each plot id
   d <- names(n)[n > 1]

   if(length(d) > 0) {
      cat('\nPlot ids used for more than one vegetation record:\n')
      x <- unique(plots[plots$plot_id %in% d, c('plot_id', 'old_plot_id', 'vegetation_record_id', 'notes')])
      print(x[order(x$plot_id), ], quote = FALSE, row.names = FALSE, right = FALSE)
      stop(length(d), ' plot ids are used for more than one vegetation record (fix in fix_plots.txt or fix_sections.txt)', call. = FALSE)
   }
}
