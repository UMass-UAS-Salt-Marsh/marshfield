#' Check for valid RTK ids in plots data and report errors
#'
#' @param plots Plots data frame
#' @export


check_plot_rtkids <- function(plots) {


   v <- valid_rtkids(plots$rtk_id) & !is.na(plots$easting)                    # valid RTK ids in plots data that matched an RTK point
   if(any(!v)) {
      cat('\nBad, missing, or unmatched RTK ids in plot data:\n')
      print(data.frame(row = 1:nrow(plots), plot_id = plots$plot_id, rtk_id = plots$rtk_id, notes = plots$notes)[!v,], quote = FALSE, row.names = FALSE, right = FALSE)
   }
}
