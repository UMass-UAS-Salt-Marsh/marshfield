#' Print frequency table of subclasses
#'
#' @param plots Plots data frame
#' @param classes Classes table (tab-delimited)
#' @export


subclasses <- function(plots, classes = 'C:/Work/saltmarsh/pars/classes.txt') {


   cl <- read.table(classes, sep = '\t', header = TRUE, quote = '', comment.char = '')
   x <- paste(sprintf('%02d', plots$subclass), cl$subclass_name[match(plots$subclass, cl$subclass)])
   z <- data.frame(table(x))

   names(z) <- c('subclass', 'frequency')
   z$subclass <- as.character(z$subclass)
   z <- rbind(z, c('Total', sum(z$freq)))
   z$frequency <- as.numeric(z$frequency)
   print(z[, 2:1], row.names = FALSE, right = FALSE)
}
