#' Return TRUE for fields where the plot_id is valid for UMass UAS 2026
#'
#' Valid ids are in the form <tablet letter><month><day>-<section>-<plot no>. Valid tablet letters are
#' B - blue, P - pink, J - jet black. Valid month letters are J - July, A - August.
#'
#' @param x A vector of plot ids
#' @returns TRUE for elements that are valid
#' @export


valid_plotids <- function(x) {


   grepl('[BJP][JA]\\d+-\\d+-\\d+', x, ignore.case = TRUE)
}
