#' Summzarize subclass frequency
#'
#' Pulls draft database from UMass UAS 2026 field plots and summarizes subclass counts.
#'
#' Note some ugly hard-coded fixes: test data are dropped, and a missing date is added.
#'
#' @param source Path to source data file (.CSV)
#' @param get_photos If TRUE, download all photos (checks to see if each exists first)
#' @returns List of
#'    - data = Full dataset
#'    - z = Summarized data
#' @importFrom utils read.csv
#' @importFrom dplyr distinct


subclass_freq <- function(source = 'C:/Work/saltmarsh/data/uas2026/field/plots/vegetation_records.csv',
                          get_photos = FALSE) {


   data <- read.csv(source)


   ### Ugly data fixes ###
   data <- data[!data$site_name %in% c('beech hill', 'Beech Hill Road', 'blue test', 'hoop', 'Office', 'photos', 'pink test', 'test', 'test Aug 2', 'yard', 'yard2', 'yard03'), ]
   data$date[data$date == ''] <- '2026-08-05'   # Fix missing date for Andrew & Ava


   if(get_photos)
      get_photos(unique(data$photo_filename))


   cat('Sites:\n')
   print(table(data$site_name))
   cat('\n\n')

   x <- distinct(data, session_id, plot_number, subclass)

   z <- as.data.frame(table(x$subclass))
   names(z) <- c('subclass', 'freq')
   z$subclass <- as.character(z$subclass)
   z$subclass[z$subclass == ''] <- '0 - missing'

   y <- strsplit(z$subclass, ' - ')
   z$subclass_n <- as.numeric(unlist(lapply(y, function(x) x[[1]])))
   z$subclass <- unlist(lapply(y, function(x) x[[2]]))
   z <- z[order(z$subclass_n), c('subclass_n', 'subclass', 'freq')]
   z <- rbind(z, c(NA, 'TOTAL', sum(z$freq)))


   cat('Plots by date\n')
   d <- as.data.frame(table(distinct(data, session_id, date, plot_number)$date))
   names(d) <- c('Date', 'Count')
   d$Date <- as.character(d$Date)
   d <- rbind(d, c('Total', sum(as.numeric(d$Count))))
   print(d)
   cat('\n\n')

   cat('Subclass count\n')
   print(z)

   invisible(list(data = data, z = z))
}
