#' Clean RTK ids for UMass UAS 2026
#'
#' Valid ids are in the form <RTK unit><month><day>-<point no>. Valid RTK units are W, X, Y, Z,
#' optionally followed by a digit. Valid month letters are J - July, A - August. The day should be 2
#' digits and the point number is 3 digits, all with leading zeros. All characters are uppercase.
#' This function makes the formatting corrections.
#'
#' To check whether an RTK id is valid (ignoring) formatting niceties, use the companion function
#' `valid_rtkids`.
#'
#' @param x A vector of RTK ids
#' @returns A vector of RTK ids with the formatting cleaned up
#' @export


clean_rtkids <- function(x) {


   y <- strsplit(toupper(x), '-')
   l <- sapply(y, length)

   for(i in 1:length(y))                        # if there's a section number, drop it
      if(l[i] == 3)
         y[[i]] <- c(y[[i]][[1]], y[[i]][[3]])


   w <- sapply(y, length) == 2                  # we're only going to work with ids that have two elements (after fixing 3)

   y1 <- sapply(y[w], '[[', 1)                  # part 1: tablet and date
   y2 <- sapply(y[w], '[[', 2)                  # part 2: point number

   y1 <- paste0(substring(y1, 1, 2), sprintf('%02d', suppressWarnings(as.numeric(substring(y1, 3)))))
   y2 <- sprintf('%03d', suppressWarnings(as.numeric(y2)))

   z <- x
   z[w] <- paste(y1, y2, sep = '-')


   v <- valid_rtkids(z)                         # undo changes that leave invalid ids
   z[!v] <- x[!v]


   z
}
