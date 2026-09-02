#' Clean plot ids for UMass UAS 2026
#'
#' Valid ids are in the form <tablet letter><month><day>-<section>-<plot no>. Valid tablet letters are
#' B - blue, P - pink, J - jet black. Valid month letters are J - July, A - August. The day should be 2
#' digits, the section is 2 digits, and the plot number is 3 digits, all with leading zeros. All characters
#' are uppercase. This function makes the formatting corrections. If the plot id has only 2 parts, the
#' section is assumed to be 00.
#'
#' To check whether a plot id is valid (ignoring) formatting niceties, use the companion function
#' `valid_plotids`.
#'
#' @param x A vector of plot ids
#' @returns A vector of plot ids with the formatting cleaned up
#' @export


clean_plotids <- function(x) {


   y <- strsplit(toupper(x), '-')

   l <- sapply(y, length)                       # if section number is missing, add section 00
   for(i in 1:length(y))
      if(l[i] == 2)
         y[[i]] <- c(y[[i]][[1]], '00', y[[i]][[2]])

   y1 <- sapply(y, '[[', 1)                     # part 1: tablet and date
   y2 <- sapply(y, '[[', 2)                     # part 2: section
   y3 <- sapply(y, '[[', 3)                     # part 3: plot number

   y1 <- paste0(substring(y1, 1, 2), sprintf('%02d', as.numeric(substring(y1, 3))))
   y2 <- sprintf('%02d', as.numeric(y2))
   y3 <- sprintf('%03d', suppressWarnings(as.numeric(y3)))

   z <- paste(y1, y2, y3, sep = '-')


   v <- valid_plotids(z)                        # undo changes that leave invalid ids
   z[!v] <- x[!v]

   z
}
