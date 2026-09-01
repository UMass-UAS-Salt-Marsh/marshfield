#' Return TRUE for fields where the RTK id is valid for UMass UAS 2026
#'
#' Valid ids are in the form <RTK unit><month><day>-<point no>. Valid RTK letters are
#' W, X, Y, Z. Valid month letters are J - July, A - August.
#'
#' @param x A vector of RTK ids
#' @returns TRUE for elements that are valid
#' @export


valid_rtkids <- function(x) {


   grepl('[WXYZ][JA]\\d+-\\d+', x, ignore.case = TRUE)
}
