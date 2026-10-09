#' Check plot ids, reporting invalid ids
#'
#' @param plots Plots data frame
#' @export


check_plotids <- function(plots) {


   v <- valid_plotids(plots$plot_id)               # valid plot ids

   if(any(!v)) {
      cat('\nBad plot ids:\n')
      print(data.frame(row = 1:nrow(plots), plotid = plots$plot_id, notes = plots$notes)[!v,], quote = FALSE, row.names = FALSE)
   }
}
