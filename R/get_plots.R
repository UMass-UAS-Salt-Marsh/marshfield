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
#'


get_plots <- function(path = 'C:/Work/saltmarsh/data/uas2026/field/',
                      plots = 'plots/vegetation_records.csv',
                      sessions = 'plots/field_sessions.csv',
                      rtk = 'RTK',
                      get_photos = FALSE) {


   rtk <- gather_rtk(path = file.path(path, rtk), result = NULL)           # gather and reproject RTK points from Emlid downloads


   plots <- read.csv(file.path(path, plots))
   sessions <- read.csv(file.path(path, sessions))



   ### Remove test data that's still up on the server ###
   plots <- plots[!plots$site_name %in% c('beech hill', 'Beech Hill Road', 'blue test', 'hoop', 'Office', 'photos', 'pink test', 'test', 'test Aug 2', 'yard', 'yard2', 'yard03'), ]


   if(get_photos)                                                          # download photos
      get_photos(unique(plots$photo_filename))



   plots <<- plots
   sessions <<- sessions


   # fix missing plot ids
   plotno <- suppressWarnings(as.numeric(plots$plot_id))
   bad <- !is.na(plotno)                                    # plot ids that weren't set, and thus were assigned 1, 2, 3, ...
   x <- strsplit(plots$site_name[bad], '-')
   plots$old_plot_id <- plots$plot_id
   plots$plot_id[bad] <- paste(sapply(x, '[[', 1), sapply(x, '[[', 2), sprintf('%03d', plotno[bad]), sep = '-')

   # fix plot ids
   # split out percent cover to a separate table and collapse plots table
   # fix missing/incorrect dates
   # fix RTK ids
   # recover missing subclasses
   # pull missing GPS from photos
   # join in observers from session
   #






}
