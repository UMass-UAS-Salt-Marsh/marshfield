#' Clean up incorrect plot ids
#'
#' Bad plot ids are usually from the id using the wrong date, but there could be other reasons.
#'
#' Parameter file:
#' - `fix_plots.txt` `old`, `new`, `fix_plots_reason`
#'
#' @param plots Plots data frame
#' @param path Base path
#' @returns Plots data frame
#' @export


fix_plots <- function(plots, path) {


   fix <- read.table(file.path(path, 'pars/fix_plots.txt'), sep = '\t', header = TRUE, quote = '')
   b <- match(plots$plot_id, fix$old)
   plots[!is.na(b), c('plot_id', 'old_plot_id_fixed', 'fix_plots_reason')] <- fix[b[!is.na(b)], c('new', 'old', 'fix_plots_reason')]

   plots
}
