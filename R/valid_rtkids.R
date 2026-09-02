#' Return TRUE for fields where the RTK id is valid for UMass UAS 2026
#'
#' Valid ids are in the form <RTK unit><month><day>-<point no>. Valid RTK units are
#' W, X, Y, Z, optionally followed by a digit. Valid month letters are J - July, A - August.
#'
#' @param x A vector of RTK ids
#' @returns TRUE for elements that are valid
#' @export


valid_rtkids <- function(x) {


   grepl('[WXYZ]\\d{0,1}[JA]\\d+-\\d+', x, ignore.case = TRUE)
}
