#' Check for wrong dates in plot ids
#'
#' @param plots Plots data frame
#' @export


check_plotid_dates <- function(plots) {


   months <- c(J = 7, A = 8, S = 9, O = 10, N = 11, D = 12)                            # month letters in plot ids
   pd <- data.frame(month = unname(months[toupper(substr(plots$plot_id, 2, 2))]), day = suppressWarnings(as.numeric(substr(plots$plot_id, 3, 4))))
   ad <- data.frame(month = month(plots$date), day = day(plots$date))
   b <- apply(pd != ad, 1, 'any')
   b[is.na(b)] <- TRUE                                                                 # catch malformed names

   if(any(b)) {
      cat('\n', sum(b), ' plots have plot_id with wrong date or malformed name (add these to fix_plots.txt):', sep = '')
      print(plots$plot_id[b])
      cat('\n')
   }
}
