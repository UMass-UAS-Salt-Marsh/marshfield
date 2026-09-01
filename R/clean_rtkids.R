#' Clean RTK ids for UMass UAS 2026
#'
#' Valid ids are in the form <RTK unit><month><day>-<point no>. Valid RTK letters are
#' W, X, Y, Z. Valid month letters are J - July, A - August. The day should be 2
#' digits and the point number is 3 digits, all with leading zeros. All characters
#' are uppercase. This function makes the formatting corrections.
#'
#' To check whether an RTK id is valid (ignoring) formatting niceties, use the companion function
#' `valid_rtkids`. You must use `valid_rtkids` before calling this function to avoid corrupting plot
#' ids by reformatting bad ids.
#'
#' @param x A vector of RTK ids
#' @returns A vector of RTK ids with the formatting cleaned up
#' @export


clean_rtkids <- function(x) {


   if(any(!valid_rtkids(x)))
      stop('clean_rtkids may only be called for valid RTK points')


   y <- strsplit(toupper(x), '-')
   l <- sapply(y, length)


   y1 <- sapply(y, '[[', 1)                     # part 1: tablet and date
   y2 <- sapply(y, '[[', 2)                     # part 2: point number

   y1 <- paste0(substring(y1, 1, 2), sprintf('%02d', as.numeric(substring(y1, 3))))
   y2 <- sprintf('%03d', suppressWarnings(as.numeric(y2)))

   paste(y1, y2, sep = '-')
}
