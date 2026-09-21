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
#' @importFrom sf st_crs<- st_transform st_as_sf st_coordinates
#' @export


get_plots <- function(path = 'C:/Work/saltmarsh/data/uas2026/field',
                      plot_file = 'plots/vegetation_records.csv',
                      session_file = 'plots/field_sessions.csv',
                      rtk_dir = 'RTK',
                      get_photos = FALSE,
                      stop_on_error = FALSE) {


   rtk <- gather_rtk(path = file.path(path, rtk_dir), result = NULL,
                     file.path(path, 'pars', rename_file = 'rtk_names.txt'))        # gather and reproject RTK points from Emlid downloads


   plots <- read.csv(file.path(path, plot_file)) |>
      rename_cols(file.path(path, 'pars', 'plots_names.txt'))

   sessions <- read.csv(file.path(path, session_file)) |>
      rename_cols(file.path(path, 'pars', 'sessions_names.txt'))


   ### Remove test data that's still up on the server ###
   plots <- plots[!plots$site_name %in% c('beech hill', 'Beech Hill Road', 'blue test', 'hoop', 'Office', 'photos', 'pink test', 'test', 'test Aug 2', 'yard', 'yard2', 'yard03'), ]

   rownames(plots) <- 1:nrow(plots)


   if(get_photos)                                                          # download photos
      get_photos(unique(plots$photo_filename))


   cat('\n\nCleaning up plot data...\n\n')

   plots$old_plot_id <- plots$plot_id
   plots$old_rtk_id <- plots$rtk_id
   plots$old_subclass <- plots$subclass
   rtk$old_rtk_id <- rtk$rtk_id

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
   plots$rtk_id <- clean_rtkids(plots$rtk_id)                              # clean up formatting of RTK ids in plots
   rtk$rtk_id <- clean_rtkids(rtk$rtk_id)                                    # clean up formatting of RTK ids in RTK data

   v <- valid_rtkids(plots$rtk_id)                                         # valid RTK ids in plots data
   if(any(!v)) {
      cat('\nBad or missing RTK ids in plot data:\n')
      print(data.frame(row = 1:nrow(plots), rtk_id = plots$rtk_id, notes = plots$notes)[!v,], quote = FALSE, row.names = FALSE)
      plots$rtkid_err[!v] <- TRUE
      err <- TRUE
   }

   v2 <- valid_rtkids(rtk$rtk_id)                                            # valid RTK ids in RTK data
   if(any(!v2)) {
      cat('\nBad RTK ids in RTK data:\n')
      print(data.frame(row = 1:nrow(rtk), rtk_id = rtk$rtk_id)[!v2,], quote = FALSE, row.names = FALSE)
      rtk$rtkid_err[!v2] <- TRUE
      err <- TRUE
   }


   d <- duplicated(rtk$rtk_id)                                               # dups ids in RTK data
   if(any(d)) {
      cat('\nDuplicated RTK ids in RTK data:\n')
      d <- rtk$rtk_id %in% rtk$rtk_id[d]
      print(data.frame(row = 1:nrow(rtk), rtk_id = rtk$rtk_id, old_rtk_id = rtk$old_rtk_id)[d,], quote = FALSE, row.names = FALSE)
      rtk$rtkid_dup[d] <- TRUE
      err <- TRUE
   }


   if(err & stop_on_error)
      stop('There were unresolved errors in plot or RTK ids')



   # split out percent cover to a separate table and collapse plots table

   pct_cover <- plots[, c('plot_id', 'species_code', 'species', 'pct_cover')]
   plots <- plots[, !names(plots) %in% c('species_code', 'species', 'pct_cover')]
   plots <- plots[!duplicated(plots$plot_id), ]


   # Now check for duplicated RTK ids in plot data

   d <- duplicated(plots$rtk_id)
   if(any(d)) {
      cat('\n', sum(d), ' duplicated RTK ids in plot data:\n', sep = '')
      d <- plots$rtk_id %in% plots$rtk_id[d]
      print(data.frame(row = 1:nrow(plots), rtk_id = plots$rtk_id, old_rtk_id = plots$old_rtk_id, notes = plots$notes)[d,], quote = FALSE, row.names = FALSE)
      rtk$rtkid_dup[d] <- TRUE
      err <- TRUE
   }


   # clean up plots table some more

   plots$site_name <- 'Essex'
   plots$subclass <- as.numeric(stri_extract_first_regex(plots$subclass, '\\d+'))



   # join in observers from session

   sessions$observers <- gsub(',|and ', '', sessions$observers)    # clean up
   sessions$observers <- gsub('YJS', 'YKS', sessions$observers)    # typo
   sessions$observers <- gsub('et al', '+', sessions$observers)
   sessions$observers <- toupper(sessions$observers)


   plots <- merge(plots, sessions[, c('session_id', 'observers')], by = 'session_id', all.y = FALSE)


   # rescue plot photo from my iPhone! *************************************   <<<<<------



   # rename photos to plot numbers and pull plot date & time from EXIF data
   # photos have been downloaded to photos/downloads; here, we copy them to photos/plots renamed to <plot id>.jpg

   b <- plots$photo_filename != ''                                               # skip plots without photos
   plots$has_photo <- b

   f <- file.path(path, 'photos', 'downloads', basename(plots$photo_filename[b]))
   p <- file.path(path, 'photos', 'plots')

   d <- list.files(p)
   invisible(file.remove(file.path(p, d)))

   cat('\nCopying photos to ', p, '...\n', sep = '')
   r <- file.path(p, paste0(plots$plot_id[b], '.jpg'))
   e <- file.copy(f, r, recursive = FALSE)
   if(any(!e))
      message('Error copying ', sum(!e), 'photos')

   cat('Reading EXIF data...\n')
   e <- read_exif(r, , tags = c('DateTimeOriginal', 'GPSLatitude', 'GPSLongitude'))    # get EXIF data from plot photos
   plots[b, c('photo_date', 'photo_lat', 'photo_long')] <- e[, c('DateTimeOriginal', 'GPSLatitude', 'GPSLongitude')]
   plots$photo_date <- ymd_hms(plots$photo_date, tz = 'America/New_York')                             # fix dumb EXIF date formatting

   rtk$date <- ymd_hms(rtk$date, tz = 'UTC') |>
      with_tz('America/New_York')                                # RTK points are labeled as UTC, but are really EDT


   rtk_names <- c('rtk_id', 'easting', 'northing', 'elevation', 'longitude', 'latitude', 'ellipsoidal_height', 'lateral_rms', 'elevation_rms', 'date', 'PDOP', 'tilt')
   plots <- merge(plots, rtk[, rtk_names], by = 'rtk_id', all.y = FALSE)



   # correct section numbers
   ##   fix_sections <- read.table(file.path(path, 'pars/fix_sections.txt'), sep = '\t', header = TRUE)                       # <<<<<<<<<<<<<---------- in progress


   # drop bad plots and correct bad RTKs from parameter files
   # dropped gets dropped plots, with drop_confirmd and drop_reason
   # plots gets all plots that were not dropped

   drop <- read.table(file.path(path, 'pars/drop_plots.txt'), sep = '\t', header = TRUE)
   drop$drop <- TRUE
   plots <- merge(plots, drop, by = 'plot_id', all.x = TRUE)
   plots$drop[is.na(plots$drop)] <- FALSE
   dropped <- plots[plots$drop, ]
   plots <- plots[!plots$drop, !names(plots) %in% c('drop_confirmed', 'drop_reason', 'drop')]

   message(nrow(dropped), ' plots dropped; ', nrow(plots), ' remaining')


   fix_rtk <- read.table(file.path(path, 'pars/fix_rtk.txt'), sep = '\t', header = TRUE)
   plots <- merge(plots, fix_rtk, by = 'plot_id', all.x = TRUE)
   plots$fix_rtk_confirmed[is.na(plots$fix_rtk_confirmed)] <- FALSE
   plots$fix_rtk_reason[is.na(plots$fix_rtk_reason)] <- ''
   plots <- plots[, !names(plots) %in% 'new_rtk_id']

   b <- !is.na(plots$new_rtk_id)
   plots$rtk_id[b] <- plots$new_rtk_id[b]

   # Check again for duplicated RTK ids in plot data

   d <- duplicated(plots$rtk_id)
   if(any(d)) {
      cat('\nAfter RTK corrections, ', sum(d), ' duplicated RTK ids in plot data:\n', sep = '')
      d <- plots$rtk_id %in% plots$rtk_id[d]
      print(data.frame(row = 1:nrow(plots), rtk_id = plots$rtk_id, old_rtk_id = plots$old_rtk_id, notes = plots$notes)[d,], quote = FALSE, row.names = FALSE)
      rtk$rtkid_dup[d] <- TRUE
      err <- TRUE
   }


   plots <- sort_plots(plots, path)                            # sort plots by date, tablet, section, and plot number


   ##  rtk_errors <<- flag_rtk(plots, rtk, path)             # ...................... try to find RTK errors .....................




   # split plots (primary data) and aux (corresponding, with extra info)


   primary <- c('plot_id', 'date', 'subclass', 'rtk_id', 'easting', 'northing', 'elevation', 'lateral_rms', 'section', 'observers', 'notes', 'has_photo', 'plotid_err', 'rtkid_err')
   aux <- plots[, c('plot_id', names(plots)[!names(plots) %in% primary])]
   everything <- plots
   plots <- plots[, primary]


   # Results:
   #   plots - primary fields without clutter
   #   aux - everything else
   #   everything - all fields

   plots <<- plots                                                                                    # save data frames as globals
   everything <<- everything
   aux <<- aux
   dropped <<- dropped
   rtk <<- rtk
   pct_cover <<- pct_cover
   sessions <<- sessions




   # write preliminary shapefiles
   pathP <- file.path(path, 'prelim')                                                                 # preliminary results path

   q <- st_as_sf(everything, coords = c('easting', 'northing', 'elevation'), crs = 'EPSG:6491+5703')  # 3d GeoPackage of everything
   st_write(q, file.path(pathP, 'everything.gpkg'), append = FALSE)

   q <- st_as_sf(plots, coords = c('easting', 'northing', 'elevation'), crs = 'EPSG:6491+5703')       # 3d GeoPackage of just primary fields
   st_write(q, file.path(pathP, 'plots.gpkg'), append = FALSE)


   everything$date <- as.character(everything$date)                                                   # so shapefile writing doesn't have conniptions
   everything$tablet_date <- as.character(everything$tablet_date)
   everything$photo_date <- as.character(everything$photo_date)
   q <- st_as_sf(everything, coords = c('easting', 'northing'), crs = 'EPSG:6491')                    # 2d shapefile of everything
   suppressWarnings(st_write(q, file.path(pathP, 'everything.shp'), append = FALSE))

   plots$date <- as.character(plots$date)
   q <- st_as_sf(plots, coords = c('easting', 'northing'), crs = 'EPSG:6491')                         # 2d shapefile of plots (just the good stuff)
   suppressWarnings(st_write(q, file.path(pathP, 'plots.shp'), append = FALSE))


   # write preliminary tables
   tables <- c('plots', 'everything', 'dropped', 'rtk', 'pct_cover', 'sessions', 'aux')
   for(f in tables)
      write.table(eval(parse(text = f)), file.path(pathP, paste0(f, '.txt')), sep = '\t', row.names = FALSE, quote = FALSE)

   message('Preliminary tables and shapefiles written to ', pathP)

   message(nrow(plots), ' plots in final dataset')





   # fix missing RTK cascades
   # recover missing subclasses
   # pull missing GPS from photos ... ?






}
