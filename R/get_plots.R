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
#' @importFrom exifr read_exif
#' @importFrom lubridate ymd_hms with_tz
#' @importFrom sf st_crs st_transform st_as_sf st_coordinates
#' @export


get_plots <- function(path = 'C:/Work/saltmarsh/data/uas2026/field',
                      plot_file = 'plots/vegetation_records.csv',
                      session_file = 'plots/field_sessions.csv',
                      rtk_dir = 'RTK',
                      get_photos = FALSE,
                      stop_on_error = FALSE) {


   rtk <- gather_rtk(path = file.path(path, rtk_dir), result = NULL)       # gather and reproject RTK points from Emlid downloads


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
   bad <- !is.na(plotno)                                                   # plot ids that weren't set, and thus were assigned 1, 2, 3, ...
   fixable <- valid_plotids(plots$site)
   x <- strsplit(plots$site_name[bad & fixable], '-')
   plots$old_plot_id <- plots$plot_id
   plots$plot_id[bad & fixable] <- paste(sapply(x, '[[', 1), sapply(x, '[[', 2), sprintf('%03d', plotno[bad & fixable]), sep = '-')



   err <- FALSE                                                            # --- find and report errors ---

   # clean plot ids
   plots$plot_id <- clean_plotids(plots$plot_id)                           # clean up formatting of plot ids, and add section 00 if necessary
   v <- valid_plotids(plots$plot_id)                                       # valid plot ids

   if(any(!v)) {
      cat('\nBad plot ids:\nd')
      print(data.frame(row = 1:nrow(plots), plotid = plots$plot_id, notes = plots$notes)[!v,], quote = FALSE, row.names = FALSE)
      plots$plotid_err[!v] <- TRUE
      err <- TRUE
   }

   # clean RTK ids
   plots$rtk_point_number <- clean_rtkids(plots$rtk_point_number)          # clean up formatting of RTK ids in plots
   rtk$Name <- clean_rtkids(rtk$Name)                                      # clean up formatting of RTK ids in RTK data

   v <- valid_rtkids(plots$rtk_point_number)                               # valid RTK ids in plots data
   if(any(!v)) {
      cat('\nBad or missing RTK ids in plot data:\n')
      print(data.frame(row = 1:nrow(plots), rtkid = plots$rtk_point_number, notes = plots$notes)[!v,], quote = FALSE, row.names = FALSE)
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


   d <- duplicated(rtk$Name)                                               # dups ids in RTK data
   if(any(d)) {
      cat('\nDuplicated RTK ids in RTK data:\n')
      d <- rtk$Name %in% rtk$Name[d]
      print(data.frame(row = 1:nrow(rtk), Name = rtk$Name, old_name = rtk$old_name)[d,], quote = FALSE, row.names = FALSE)
      rtk$rtkid_dup[d] <- TRUE
      err <- TRUE
   }


   if(err & stop_on_error)
      stop('There were unresolved errors in plot or RTK ids')



   # split out percent cover to a separate table and collapse plots table

   pct_cover <- plots[, c('plot_id', 'species_code', 'species_scientific_name', 'percentage_cover')]
   plots <- plots[, !names(plots) %in% c('species_code', 'species_scientific_name', 'species_common_name', 'percentage_cover')]
   plots <- plots[!duplicated(plots$plot_id), ]


   # Now check for duplicated RTK ids in plot data

   d <- duplicated(plots$rtk_point_number)
   if(any(d)) {
      cat('\nDuplicated RTK ids in plot data:\n')
      d <- plots$rtk_point_number %in% plots$rtk_point_number[d]
      print(data.frame(row = 1:nrow(plots), rtk_point_number = plots$rtk_point_number, old_rtk_point_number = plots$old_rtk_point_number, notes = plots$notes)[d,], quote = FALSE, row.names = FALSE)
      rtk$rtkid_dup[d] <- TRUE
      err <- TRUE
   }



   # clean up plots table some more

   plots$site_name <- 'Essex'
   plots$subclass <- as.numeric(stri_extract_first_regex(plots$subclass, '\\d+'))

   plots <- plots[, !names(plots) %in% c('protocol_code', 'transect_id', 'habitat_type', 'distance_along_transect_m', 'canopy_height_m', 'thatch_height_m', 'elevation_navd88_m')]


   # join in observers from session

   sessions$other_members <- gsub(',|and ', '', sessions$other_members)    # clean up
   sessions$other_members <- gsub('YJS', 'YKS', sessions$other_members)    # typo
   sessions$other_members <- gsub('et al', '+', sessions$other_members)
   sessions$other_members <- toupper(sessions$other_members)

   plots <- merge(plots, sessions[, c('id', 'start_time', 'other_members')], by.x = 'session_id', by.y = 'id', all.y = FALSE)
   names(plots)[names(plots) == 'other_members'] <- 'observers'
   names(plots[names(plots) == 'latitude']) <- 'tablet_lat'
   names(plots[names(plots) == 'longitude']) <- 'tablet_long'


   # rescue plot photo from my iPhone! *************************************


   # rename photos to plot numbers and pull plot date & time from EXIF data
   # photos have been downloaded to photos/downloads; here, we copy them to photos/plots renamed to <plot id>.jpg


   b <- plots$photo_filename != ''                                               # skip plots without photos
   plots$has_photo <- b

   f <- file.path(path, 'photos', 'downloads', basename(plots$photo_filename[b]))
   p <- file.path(path, 'photos', 'plots')

   d <- list.files(p)
   invisible(file.remove(file.path(p, d)))

   cat('\nCopying photos to ', p, '...\n')
   r <- file.path(p, paste0(plots$plot_id[b], '.jpg'))
   e <- file.copy(f, r, recursive = FALSE)
   if(any(!e))
      message('Error copying ', sum(!e), 'photos')

   cat('Reading EXIF data...\n')
   e <- read_exif(r, , tags = c('DateTimeOriginal', 'GPSLatitude', 'GPSLongitude'))    # get EXIF data from plot photos
   plots[b, c('photo_date', 'photo_lat', 'photo_long')] <- e[, c('DateTimeOriginal', 'GPSLatitude', 'GPSLongitude')]
   plots$photo_date <- ymd_hms(plots$photo_date, tz = 'America/New_York')                             # fix dumb EXIF date formatting

   rtk$date <- ymd_hms(rtk$Averaging.start, tz = 'UTC') |>
      with_tz('America/New_York')                                # RTK points are labeled as UTC, but are really EDT


   x <- merge(plots, rtk, by.x = 'rtk_point_number', by.y = 'Name', all.y = FALSE)
   d <- data.frame(plotid = x$plot_id, rtk_id = x$rtk_point_number, photo_date = x$photo_date, rtk_date = x$date.y, delta = difftime(x$photo_date, x$date.y), delta_lag_1 = difftime(x$photo_date, c(0, x$date.y[-nrow(x)])))
   d$lag_better <- abs(d$delta_lag_1) < abs(d$delta)

   tab <- st_as_sf(x, coords = c('latitude', 'longitude')[c(2, 1)])
   st_crs(tab) <- 'EPSG:4326'
   tab <- st_transform(tab, crs = 'EPSG:6491')
   x[, c('tablet_easting', 'tablet_northing')] <- st_coordinates(tab)

   d$dist <- sqrt((x$Easting - x$tablet_easting)^2 + (x$Northing - x$tablet_northing)^2)
   d$dist_lag_1 <-sqrt(   ((x$Easting - c(0, x$tablet_easting[-nrow(x)]))^2) + ((x$Northing - c(0, x$tablet_northing[-nrow(x)]))^2))

   d$dist[d$dist > 100] <- NA                                  # if the distance is > 100 m, it's worthless
   d$dist_lag_1[d$dist_lag_1 > 100] <- NA                      # if the distance is > 100 m, it's worthless

   d$notes <- x$notes


   rtk_diag <<- d                                              # save table of RTK error diagnostics


   # flag non-continuous RTK ids




   # split plots (primary data) and aux (corresponding, with extra info)

   # ..... ADD HERE: rename fields to ones I like .....

   primary <- c('plot_id', 'date', 'subclass', 'rtk_id', 'easting', 'northing', 'elevation', 'observers', 'notes', 'has_photo', 'plotid_err', 'rtkid_err')
   aux <- plots[, c('plot_id', names(plots)[!names(plots) %in% primary])]
   # plots <- plots[, primary]



   rtk <<- rtk
   plots <<- plots
   pct_cover <<- pct_cover
   sessions <<- sessions





   # fix missing RTK cascades
   # fix missing/incorrect dates
   # recover missing subclasses
   # pull missing GPS from photos ... ?







}
