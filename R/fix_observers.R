#' Fix observer initials for correctness and consistency
#'
#' Parameter file:
#' - `fix_observers.txt` Fix errors and inconsistencies in observers: `old`, `new`, `reason`.
#'
#' @param plots Plots data frame
#' @param path Base path
#' @returns Plots data frame
#' @export


fix_observers <- function(plots, path) {


   fix_obs <- read.table(file.path(path, 'pars/fix_observers.txt'), sep = '\t', header = TRUE, quote = '')
   b <- match(plots$observers, fix_obs$old)
   plots$observers[!is.na(b)] <- fix_obs$new[b[!is.na(b)]]

   plots
}
