#' Correct bad RTKs in plots from parameter files
#'
#' Parameter file:
#' - `fix_rtk.txt` RTK ids to reassign (usually thanks to off-by-one errors): `plot_id`,
#'   `new_rtk_id`, `fix_rtk_confirmed`, `fix_rtk_reason`
#'
#' @param plots Plots data frame
#' @param path Base path
#' @returns Plots data frame
#' @export


fix_plot_rtk <- function(plots, path) {


   fix_rtk <- read.table(file.path(path, 'pars/fix_rtk.txt'), sep = '\t', header = TRUE, quote = '')
   plots <- merge(plots, fix_rtk, by = 'plot_id', all.x = TRUE)
   plots$fix_rtk_confirmed[is.na(plots$fix_rtk_confirmed)] <- FALSE
   plots$fix_rtk_reason[is.na(plots$fix_rtk_reason)] <- ''

   b <- !is.na(plots$new_rtk_id)
   plots$rtk_id[b] <- plots$new_rtk_id[b]
   plots <- plots[, !names(plots) %in% 'new_rtk_id']

   plots
}
