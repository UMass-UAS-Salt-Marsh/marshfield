#' Check for valid RTK ids in plots data and report errors
#'
#' @param plots Plots data frame
#' @export


check_plot_rtkids <- function(plots) {


   v <- valid_rtkids(plots$rtk_id)                                            # valid RTK ids in plots data
   if(any(!v)) {
      cat('\nBad or missing RTK ids in plot data:\n')
      print(data.frame(row = 1:nrow(plots), rtk_id = plots$rtk_id, notes = plots$notes)[!v,], quote = FALSE, row.names = FALSE, right = FALSE)
      plots$rtkid_err[!v] <- TRUE
   }
}
