#' Check for duplicated RTK ids in plot data and flag them
#'
#' @param plots Plots data frame
#' @returns Plots data frame
#' @export


check_plot_rtk_dups <- function(plots) {


   d <- duplicated(plots$rtk_id)
   if(any(d)) {
      cat('\n', sum(d), ' duplicated RTK ids in plot data:\n', sep = '')
      d <- plots$rtk_id %in% plots$rtk_id[d]
      print(data.frame(row = 1:nrow(plots), plot_id = plots$plot_id, rtk_id = plots$rtk_id,
                       old_rtk_id = plots$old_rtk_id, notes = plots$notes)[d,],
            quote = FALSE, row.names = FALSE, right = FALSE)
      plots$rtkid_dup[d] <- TRUE
   }

   plots
}
