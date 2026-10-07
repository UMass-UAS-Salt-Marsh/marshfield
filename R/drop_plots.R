#' Drop bad plots designated in drop_plots.txt
#'
#' Parameter file:
#' - `drop_plots.txt` Plots to drop: `plot_id`, `drop_confirmed`, and `drop_reason`
#'
#' @param plots Plots data frame
#' @param path Base path
#' @returns Named list of `plots` = plots data frame, `dropped` = plots that were dropped
#' @export


drop_plots <- function(plots, path) {


   drop <- read.table(file.path(path, 'pars/drop_plots.txt'), sep = '\t', header = TRUE, quote = '')
   drop$drop <- TRUE
   plots <- merge(plots, drop, by = 'plot_id', all.x = TRUE)
   plots$drop[is.na(plots$drop)] <- FALSE
   dropped <- plots[plots$drop, ]
   plots <- plots[!plots$drop, !names(plots) %in% c('drop_confirmed', 'drop_reason', 'drop')]

   cat('\n', nrow(dropped), ' plots dropped; ', nrow(plots), ' remaining. Dropped plots:\n', sep = '')
   print(dropped[, c('plot_id', 'drop_confirmed', 'drop_reason')], row.names = FALSE, right = FALSE)

   list(plots = plots, dropped = dropped)
}
