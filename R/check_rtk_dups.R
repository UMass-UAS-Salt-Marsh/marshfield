#' Check for duplicate RTK ids and flag them
#'
#' @param rtk RTK data frame
#' @returns RTK data frame
#' @export


check_rtk_dups <- function(rtk) {


   d <- duplicated(rtk$rtk_id)                                                # dup ids in RTK data
   if(any(d)) {
      cat('\nDuplicated RTK ids in RTK data: (these should have been fixed in dedup_rtk.txt)\n')
      d <- rtk$rtk_id %in% rtk$rtk_id[d]
      print(data.frame(row = 1:nrow(rtk), rtk_id = rtk$rtk_id, old_rtk_id = rtk$old_rtk_id)[d,], quote = FALSE, row.names = FALSE)
      rtk$rtkid_dup[d] <- TRUE
   }

   rtk
}
