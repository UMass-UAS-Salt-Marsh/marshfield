#' Check for valid RTK ids in RTK data and report errors
#'
#' @param rtk RTK data frame
#' @export


check_rtk_rtkids <- function(rtk) {


   v <- valid_rtkids(rtk$rtk_id)                                             # valid RTK ids in RTK data
   if(any(!v)) {
      cat('\nBad RTK ids in RTK data:\n')
      print(data.frame(row = 1:nrow(rtk), rtk_id = rtk$rtk_id)[!v,], quote = FALSE, row.names = FALSE)
      rtk$rtkid_err[!v] <- TRUE
   }
}
