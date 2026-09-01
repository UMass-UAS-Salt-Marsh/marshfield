#' Top-level function to clean up and process field data
#'
#' Pulls draft database from UMass UAS 2026 field plots and summarizes subclass counts.
#'
#' Note some ugly hard-coded fixes: test data are dropped, and a missing date is added.
#'
#' @param path Path to source data
#' @param plot_file Name of plots file from Salt Marsh Data
#' @param session_file Name of sessions file from Salt Marsh data
#' @param rtk_dir Path to directory of RTK CSVs from Emlid
#' @param get_photos If TRUE, download all photos (checks to see if each exists first)
#' @returns List of
#'    - data = Full dataset
#'    - z = Summarized data
#' @importFrom utils read.csv
#' @importFrom dplyr distinct
#' @export


get_plots <- function(path = 'C:/Work/saltmarsh/data/uas2026/field/',
                      plot_file = 'plots/vegetation_records.csv',
                      session_file = 'plots/field_sessions.csv',
                      rtk_dir = 'RTK',
                      get_photos = FALSE) {


   rtk <- gather_rtk(path = file.path(path, rtk_dir), result = NULL)           # gather and reproject RTK points from Emlid downloads


   plots <- read.csv(file.path(path, plot_file))
   sessions <- read.csv(file.path(path, session_file))



   ### Remove test data that's still up on the server ###
   plots <- plots[!plots$site_name %in% c('beech hill', 'Beech Hill Road', 'blue test', 'hoop', 'Office', 'photos', 'pink test', 'test', 'test Aug 2', 'yard', 'yard2', 'yard03'), ]


   if(get_photos)                                                          # download photos
      get_photos(unique(plots$photo_filename))


   rtk <<- rtk
   plots <<- plots
   sessions <<- sessions

   return()

   plots$orig_plot_ids <- plots$plot_ids
   # ....... and other stuff I'll change


   # fix missing plot ids
   plotno <- suppressWarnings(as.numeric(plots$plot_id))
   bad <- !is.na(plotno)                                    # plot ids that weren't set, and thus were assigned 1, 2, 3, ...
   fixable <- valid_plotids(plots$site)
   x <- strsplit(plots$site_name[bad & fixable], '-')
   plots$old_plot_id <- plots$plot_id
   plots$plot_id[bad & fixable] <- paste(sapply(x, '[[', 1), sapply(x, '[[', 2), sprintf('%03d', plotno[bad & fixable]), sep = '-')


   # clean plot ids
   plots$plot_id <- clean_plotids(plots$plot_id)            # clean up formatting of all plot ids, and add section 00 if necessary
   bad <- !valid_plotids(plots$plot_id)
   if(any(bad)) {
      print('Bad plot ids:')
      print(cbind(1:nrow(plots), plots$plot_id)[bad])
      stop()
   }

   # split out percent cover to a separate table and collapse plots table
   # fix missing/incorrect dates
   # fix RTK ids
   # recover missing subclasses
   # pull missing GPS from photos
   # join in observers from session
   #






}
