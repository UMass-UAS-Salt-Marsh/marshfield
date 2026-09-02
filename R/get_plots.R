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
#' @param stop_on_error If TRUE, throw an error if there are any bad plot ids or RTK ids
#' @returns List of
#'    - data = Full dataset
#'    - z = Summarized data
#' @importFrom utils read.csv
#' @importFrom dplyr distinct
#' @importFrom stringi stri_extract_first_regex
#' @export


get_plots <- function(path = 'C:/Work/saltmarsh/data/uas2026/field/',
                      plot_file = 'plots/vegetation_records.csv',
                      session_file = 'plots/field_sessions.csv',
                      rtk_dir = 'RTK',
                      get_photos = FALSE,
                      stop_on_error = FALSE) {


   rtk <- gather_rtk(path = file.path(path, rtk_dir), result = NULL)           # gather and reproject RTK points from Emlid downloads


   plots <- read.csv(file.path(path, plot_file))
   sessions <- read.csv(file.path(path, session_file))



   ### Remove test data that's still up on the server ###
   plots <- plots[!plots$site_name %in% c('beech hill', 'Beech Hill Road', 'blue test', 'hoop', 'Office', 'photos', 'pink test', 'test', 'test Aug 2', 'yard', 'yard2', 'yard03'), ]

   rownames(plots) <- 1:nrow(plots)


   if(get_photos)                                                          # download photos
      get_photos(unique(plots$photo_filename))


   cat('\n\nCleaning up plot data...\n\n')

   plots$old_plot_id <- plots$plot_id
   plots$old_rtk_point_number <- plots$rtk_point_number
   plots$full_subclass <- plots$subclass
   rtk$old_name <- rtk$Name

   plots$plotid_err <- FALSE
   plots$rtkid_err <- FALSE
   rtk$rtkid_err <- FALSE

   # ....... and other stuff I'll change


   # fix missing plot ids when plot ids were in site_name
   plotno <- suppressWarnings(as.numeric(plots$plot_id))
   bad <- !is.na(plotno)                                    # plot ids that weren't set, and thus were assigned 1, 2, 3, ...
   fixable <- valid_plotids(plots$site)
   x <- strsplit(plots$site_name[bad & fixable], '-')
   plots$old_plot_id <- plots$plot_id
   plots$plot_id[bad & fixable] <- paste(sapply(x, '[[', 1), sapply(x, '[[', 2), sprintf('%03d', plotno[bad & fixable]), sep = '-')


   err <- FALSE

   # clean plot ids
   plots$plot_id <- clean_plotids(plots$plot_id)                           # clean up formatting of plot ids, and add section 00 if necessary
   v <- valid_plotids(plots$plot_id)                                       # valid plot ids

   if(any(!v)) {
      cat('\nBad plot ids:\nd')
      print(data.frame(row = 1:nrow(plots), plotid = plots$plot_id)[!v,], quote = FALSE, row.names = FALSE)
      plots$plotid_err[!v] <- TRUE
      err <- TRUE
   }

   # clean RTK ids
   plots$rtk_point_number <- clean_rtkids(plots$rtk_point_number)          # clean up formatting of RTK ids in plots
   rtk$Name <- clean_rtkids(rtk$Name)                                      # clean up formatting of RTK ids in RTK data

   v <- valid_rtkids(plots$rtk_point_number)                               # valid RTK ids in plots data
   if(any(!v)) {
      cat('\nBad RTK ids in plot data:\n')
      print(data.frame(row = 1:nrow(plots), rtkid = plots$rtk_point_number)[!v,], quote = FALSE, row.names = FALSE)
      plots$rtkid_err[!v] <- TRUE
      err <- TRUE
   }

   v2 <- valid_rtkids(rtk$Name)                                            # valid RTK ids in RTK data
   if(any(!v2)) {
      cat('\nBad RTK ids in RTK data:\n')
      print(data.frame(row = 1:nrow(rtk), rtkid = rtk$Name)[!v2,], quote = FALSE, row.names = FALSE)
      rtk$rtkid_err[!v2] <- TRUE
      err <- TRUE
   }


   if(err & stop_on_error)
      stop('There were unresolved errors in plot or RTK ids')



   # split out percent cover to a separate table and collapse plots table

   pct_cover <- plots[, c('plot_id', 'species_code', 'species_scientific_name', 'percentage_cover')]
   plots <- plots[, !names(plots) %in% c('species_code', 'species_scientific_name', 'species_common_name', 'percentage_cover')]
   plots <- plots[!duplicated(plots$plot_id), ]

   # clean up plots table some more

   plots$site_name <- 'Essex'
   plots$subclass <- as.numeric(stri_extract_first_regex(plots$subclass, '\\d+'))

   plots <- plots[, !names(plots) %in% c('protocol_code', 'transect_id', 'habitat_type', 'distance_along_transect_m', 'canopy_height_m', 'thatch_height_m', 'elevation_navd88_m')]


   # join in observers from session

   sessions$other_members <- gsub(',|and ', '', sessions$other_members)                   # clean up
   sessions$other_members <- gsub('YJS', 'YKS', sessions$other_members)                   # typo
   sessions$other_members <- gsub('et al', '+', sessoins$other_members)
   sessions$other_members <- toupper(sessions$other_members)

   plots <- merge(plots, sessions[, c('id', 'start_time', 'other_members')], by.x = 'session_id', by.y = 'id', all.y = FALSE)
   names(plots)[names(plots) == 'other_members'] <- 'observers'

   rtk <<- rtk
   plots <<- plots
   pct_cover <<- pct_cover
   sessions <<- sessions


   # fix missing RTK cascades
   # fix missing/incorrect dates
   # recover missing subclasses
   # pull missing GPS from photos ... ?







}
