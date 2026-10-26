#' Summarize field effort by day, tablet, observers, and section
#'
#' Prints a summary of plots by field day, with one line for each tablet/set of observers listing
#' sections (with number of plots), and writes a tab-delimited table to `field_effort.txt` with
#' one row for each site, date, tablet, observers, and section.
#'
#' @param plots Plots data frame. Requires columns plot_id, date, site, section, observers
#' @param pathR Path to write `field_effort.txt` to
#' @returns Field effort table, invisibly
#' @importFrom utils write.table
#' @export


field_effort <- function(plots, pathR) {


   x <- data.frame(site = plots$site,
                   date = substr(as.character(plots$date), 1, 10),
                   time = as.character(plots$date),
                   tablet = toupper(substr(plots$plot_id, 1, 1)),
                   observers = sapply(strsplit(plots$observers, ' +'), function(o)      # sort observers so order doesn't matter, keeping + (et al) last
                      paste(c(sort(o[o != '+']), o[o == '+']), collapse = ' ')),
                   section = sprintf('%02d', as.numeric(plots$section)))
   x$day_of_week <- format(as.Date(x$date), '%a')

   g <- c('site', 'date', 'day_of_week', 'tablet', 'observers', 'section')
   z <- aggregate(list(plots = x$time), by = x[, g], FUN = length)
   first <- aggregate(list(first = x$time), by = x[, g], FUN = min)       # sections in the order they were sampled
   z <- merge(z, first, by = g)
   z <- z[order(z$date, z$tablet, z$observers, z$first), ]
   z$first <- NULL
   row.names(z) <- NULL


   cat('\nField effort by day\n')
   for(d in unique(z$date)) {                                             # for each day,
      zd <- z[z$date == d, ]
      cat('\n', d, ' ', zd$day_of_week[1], '\n', sep = '')
      grp <- unique(zd[, c('tablet', 'observers')])
      secs <- n <- NULL
      for(i in seq_len(nrow(grp))) {                                      #    for each tablet/set of observers,
         zg <- zd[zd$tablet == grp$tablet[i] & zd$observers == grp$observers[i], ]
         secs[i] <- paste0(zg$section, ' (', zg$plots, ')', collapse = ', ')
         n[i] <- sum(zg$plots)
      }
      cat(sprintf('   %s  %-12s %-*s   n = %d\n', grp$tablet, grp$observers, max(nchar(secs)), secs, n), sep = '')
      cat('   Total: ', sum(zd$plots), ' plots\n', sep = '')
   }
   cat('\nTotal: ', sum(z$plots), ' plots\n', sep = '')


   write.table(z, file.path(pathR, 'field_effort.txt'), sep = '\t', row.names = FALSE, quote = FALSE, na = '')
   invisible(z)
}
