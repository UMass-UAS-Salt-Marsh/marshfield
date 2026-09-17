#' Rename and delete columns in data frame from parameter file
#'
#' @param x Data frame
#' @param file Delimited text file with two columns:
#'   - `old` existing column names
#'   - `new` new names for columns, or `-` to delete columns
#' @returns Data frame with new column names and dropped columns
#' @export


rename_cols <- function(x, file) {


   p <- read.table(file, sep = '\t', header = TRUE)
   x <- x[, p$new != '-']           # drop columns we don't like
   p <- p[!p$new %in% c('', '-'), ]

   i <- match(p$old, names(x))
   names(x)[i] <- p$new
   x
}
