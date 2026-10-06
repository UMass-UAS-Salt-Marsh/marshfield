#' Top-level function to clean up and process field data
#'
#' Pulls draft database from UMass UAS 2026 field plots and summarizes subclass counts.
#'
#' Note some ugly hard-coded fixes: test data are dropped, and a missing date is added.
#'
#' Several parameter files (tab-delimited text) are in pars/. They include
#'
#' - `sessions_names.txt` Fields to drop (use `-`) or rename in sessions: `old`, `new`
#' - `plots_names.txt` Fields to drop (use `-`) or rename in plots: `old`, `new`
#' - `rtk_names.txt` Fields to drop (use `-`) or rename in RTK: `old`, `new`
#' - `field_days.txt` Day abbreviations (J21, J22, ..., A07) in order, for sorting data: `day`
#'
#' - `fix_plots.txt` Fix bad plot ids: `old`, `new`, `fix_plots_reason`
#' - `drop_plots.txt` Plots to drop: `plot_id`, `drop_confirmed`, and `drop_reason`
#' - `fix_observers.txt` Fix errors and inconsistencies in observers: `old`, `new`, `reason`.
#'    Full observer names are in observers.txt.
#' - `fix_sections.txt` Split field days into sections: `start`, `end`, `is_section`, `reason`
#'
#' - `fix_rtk.txt` RTK ids to reassign (usually thanks to off-by-one errors): `plot_id`,
#'   `new_rtk_id`, `fix_rtk_confirmed`, `fix_rtk_reason`
#' - `dedup_rtk.txt` Drop duplicated RTKs with same rtk_id: `rtk_id`, `seq`, `new`, `reason`
#'
#' - `species.txt` Change genera incorrectly listed as species by app in percent cover
#' - `fix_pct_cover.txt` Changes to percent cover fields: `plot_id`, `species_code`, `pct_cover`, `reason`
#'
#' - `review.txt` Written by `view_plots`, used to drop rejected plots. DO NOT EDIT!
#'
#'
#' Results include GeoPackages written to path/results:
#' - `plots.gpkg` 3d point shapefile of plots, primary fields only
#' - `circles.gpkg` 2d circle shapefile of plot area, primary fields only
#' - `everything.gpkg` 3d point shapefile of plots, all fields included
#'
#' Result tables are written as tab-delimited text to path/results:
#' - `plots` Plots data, primary fields
#' - `everything` Plots data, all fields
#' - `auxil` Auxillary data (`plots` + `auxil` = `everything`)
#' - `dropped` Plots data, all fields, dropped rows
#' - `pct_cover` Percent cover, to join with `plots` on `plot_id`
#' - `rtk` RTK data, including dropped rows
#' - `sessions` Original session data from `saltmarshdata` app
#' - `rtk_seq` RTK sequencing check; `rtk_nonseq` marks potential sequencing errors
#'
#' @param path Path to source data
#' @param plot_file Name of plots file from Salt Marsh Data
#' @param session_file Name of sessions file from Salt Marsh data
#' @param rtk_dir Path to directory of RTK CSVs from Emlid
#' @param get_photos If TRUE, download all photos (checks to see if each exists first)
#' @param plot_radius Radius of plots (m)
#' @returns List of
#'    - data = Full dataset
#'    - z = Summarized data
#' @importFrom utils read.csv
#' @importFrom dplyr distinct bind_rows
#' @importFrom stringi stri_extract_first_regex
#' @importFrom lubridate ymd_hms with_tz month day
#' @importFrom sf st_crs<- st_transform st_as_sf st_coordinates
#' @export


get_plots <- function(path = 'C:/Work/saltmarsh/data/uas2026/field',
                      plot_file = 'plots/vegetation_records.csv',
                      session_file = 'plots/field_sessions.csv',
                      rtk_dir = 'RTK',
                      get_photos = FALSE,
                      plot_radius = 0.7) {


   message('Processing plots data in ', path, '...')


   # Get original RTK, plot, and session data

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



   plots <- fix_plotid_in_sitename(plots)                                  # fix missing plot ids when plot ids were in site_name

   plots$plot_id <- clean_plotids(plots$plot_id)                           # clean up formatting of plot ids, and add section 00 if necessary


   # I want to have a fix option, so you can set it to FALSE and check without fixing, or TRUE to fix and then check for remaining errors


#---------------- MOVE TO CHECK
   check_plotids(plots)                                                    # check plot ids, reporting invalid ids


   # fix identified bad plot ids
   plots <- fix_plots(plots, path)



   # clean RTK ids
   plots$rtk_id <- clean_rtkids(plots$rtk_id)                              # clean up formatting of RTK ids in plots
   rtk$rtk_id <- clean_rtkids(rtk$rtk_id)                                  # clean up formatting of RTK ids in RTK data


#-----------        MOVE TO CHECK
   check_plot_rtkids(plots)                                                # check for valid RTK ids in plots data
   check_rtk_rtkids(rtk)                                                   # check for valid RTK ids in RTK data



   rtk <- fix_rtkids(rtk, path)                                            # now rename duplicated RTK ids according to dedup_rtk.txt



#-----------        MOVE TO CHECK
   rtk <- check_rtk_dups(rtk)                                              # check for duplicate RTK ids and flag them



   plots <- fix_sections(plots, path)                                      # correct section numbers - LAST CHANGE TO PLOT IDs!



   # split out percent cover to a separate table and collapse plots table. This must be done AFTER any changes to plot ids
   pct_cover <- plots[, c('plot_id', 'species_code', 'species', 'pct_cover')]
   plots <- plots[, !names(plots) %in% c('species_code', 'species', 'pct_cover')]
   plots <- plots[!duplicated(plots$plot_id), ]



   pct_cover <- fix_pctcover(pct_cover, path)                              # fix errors in percent cover according to fix_pct_cover.txt

   check_pct_cover(plots, pct_cover)                                       # check percent cover for out of range errors


#-----------        MOVE TO CHECK
   plots <- check_plot_rtk_dups(plots)                                     # check for duplicated RTK ids in plot data and flag them




   ##############



   # clean up plots table some more
   plots$site <- 'ESX'                                                     # all plots are for site ESX
   plots$site_name <- 'Essex'
   plots$subclass <- as.numeric(stri_extract_first_regex(plots$subclass, '\\d+'))



   # join in observers from session
   sessions$observers <- gsub(',|and ', '', sessions$observers)    # clean up
   sessions$observers <- gsub('YJS', 'YKS', sessions$observers)    # typo
   sessions$observers <- gsub('et al', '+', sessions$observers)
   sessions$observers <- toupper(sessions$observers)

   plots <- merge(plots, sessions[, c('session_id', 'observers')], by = 'session_id', all.y = FALSE)


   #----------- deal_with_photos
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
   e <- exifr::read_exif(r, , tags = c('DateTimeOriginal', 'GPSLatitude', 'GPSLongitude'))    # get EXIF data from plot photos
   plots[b, c('photo_date', 'photo_lat', 'photo_long')] <- e[, c('DateTimeOriginal', 'GPSLatitude', 'GPSLongitude')]
   plots$photo_date <- ymd_hms(plots$photo_date, tz = 'America/New_York')                             # fix dumb EXIF date formatting

   rtk$date <- ymd_hms(rtk$date, tz = 'UTC') |>
      with_tz('America/New_York')                                # RTK points are labeled as UTC, but are really EDT
   #-----------


   #----------- fix_observers
   # correct observers
   fix_obs <- read.table(file.path(path, 'pars/fix_observers.txt'), sep = '\t', header = TRUE, quote = '')
   b <- match(plots$observers, fix_obs$old)
   plots$observers[!is.na(b)] <- fix_obs$new[b[!is.na(b)]]
   #-----------


   #----------- fix_species
   # fix incorrect species names in data file (bogus specific for genera)
   sp <- read.table(file.path(path, 'pars/species.txt'), sep = '\t', header = TRUE, quote = '')
   b <- match(pct_cover$species, sp$old)
   pct_cover$species[!is.na(b)] <- sp$new[b[!is.na(b)]]
   #-----------



   # drop bad plots and correct bad RTKs from parameter files
   # dropped gets dropped plots, with drop_confirmd and drop_reason
   # plots gets all plots that were not dropped

   #----------- drop_plots
   drop <- read.table(file.path(path, 'pars/drop_plots.txt'), sep = '\t', header = TRUE, quote = '')
   drop$drop <- TRUE
   plots <- merge(plots, drop, by = 'plot_id', all.x = TRUE)
   plots$drop[is.na(plots$drop)] <- FALSE
   dropped <- plots[plots$drop, ]
   plots <- plots[!plots$drop, !names(plots) %in% c('drop_confirmed', 'drop_reason', 'drop')]

   cat('\n', nrow(dropped), ' plots dropped; ', nrow(plots), ' remaining. Dropped plots:\n', sep = '')
   print(dropped[, c('plot_id', 'drop_confirmed', 'drop_reason')], row.names = FALSE, right = FALSE)
   #-----------


   #----------- fix_rtk
   fix_rtk <- read.table(file.path(path, 'pars/fix_rtk.txt'), sep = '\t', header = TRUE, quote = '')
   plots <- merge(plots, fix_rtk, by = 'plot_id', all.x = TRUE)
   plots$fix_rtk_confirmed[is.na(plots$fix_rtk_confirmed)] <- FALSE
   plots$fix_rtk_reason[is.na(plots$fix_rtk_reason)] <- ''
   plots <- plots[, !names(plots) %in% 'new_rtk_id']

   b <- !is.na(plots$new_rtk_id)
   plots$rtk_id[b] <- plots$new_rtk_id[b]
   #-----------



   # now that RTK ids are cleaned up, join plots to RTK data
   rtk_names <- c('rtk_id', 'easting', 'northing', 'elevation', 'longitude', 'latitude', 'ellipsoidal_height', 'lateral_rms', 'elevation_rms', 'date', 'PDOP', 'tilt')
   plots <- merge(plots, rtk[, rtk_names], by = 'rtk_id', all.y = FALSE)



   #----------- fix_plotid_dates
   # look for bad dates in plot ids
   pd <- data.frame(month = ifelse(substr(plots$plot_id, 2, 2) == 'J', 7, 8), day = suppressWarnings(as.numeric(substr(plots$plot_id, 3, 4))))
   ad <- data.frame(month = month(plots$date), day = day(plots$date))
   b <- apply(pd != ad, 1, 'any')
   b[is.na(b)] <- TRUE                                                           # catch malformed names

   if(any(b)) {
      cat('\n', sum(b), ' plots have plot_id with wrong date or malformed name (add these to fix_plots.txt):', sep = '')
      print(plots$plot_id[b])
      cat('\n')
   }
   #-----------


   plots <- sort_plots(plots, path)                            # sort plots by date, tablet, section, and plot number


   #----------- check_rtk_seq
   # flag out-of-sequence RTK points
   plots <- rtk_sequence(plots)
   cat('\n', sum(plots$rtk_nonseq), ' RTK points seem to be out of sequence, marked by rtk_nonseq:\n', sep = '')
   print(plots[plots$rtk_nonseq, c('plot_id', 'rtk_id')], right = FALSE)
   rtk_seq <- plots[, c('date', 'plot_id', 'rtk_id', 'rtk_nonseq', 'mplotid_err', 'rtkid_err', 'notes')]
   cat('\nSee at global rtk_seq and rtk_seq.txt to resolve these\n')
   #-----------


   #----------- process_reviews
   # include review data if available, and drop rejected plots
   f <- file.path(path, 'pars', 'review.txt')
   if(file.exists(f)) {
      cat('\nJoining in review fields from view_plots()...\n')
      review <- read.table(f, sep = '\t', header = TRUE, quote = '')
      keep <- c('plot_id', 'reviewed', 'problems', 'rejected', 'split', 'coverr', 'comments')
      plots <- merge(plots, review[, keep], by = 'plot_id', all.x = TRUE, all.y = FALSE)

      plots <- sort_plots(plots, path)                            # sort plots by date, tablet, section, and plot number

      cols <- c('reviewed', 'problems', 'split', 'coverr')
      plots[cols][is.na(plots[cols])] <- FALSE
      plots$comments[is.na(plots$comments)] <- ''

      r <- plots$rejected
      r[is.na(r)] <- FALSE
      cat('\n', sum(r), ' plots that were rejected on review dropped; ', sum(!r), ' remaining. Dropped plots:\n', sep = '')
      print(plots[r, c('plot_id', 'comments')], row.names = FALSE, right = FALSE)

      dropped <- bind_rows(dropped, plots[r, ])

      plots <- plots[!r, !names(plots) %in% 'rejected']
   }



   # split plots (primary data) and auxil (corresponding, with extra info)
   primary <- c('plot_id', 'date', 'subclass', 'site', 'rtk_id', 'easting', 'northing', 'elevation', 'lateral_rms', 'section', 'observers',
                'notes', 'reviewed', 'problems', 'split', 'coverr', 'comments', 'has_photo')
   auxil <- plots[, c('plot_id', names(plots)[!names(plots) %in% primary])]
   everything <- plots
   plots <- plots[, primary]



   # Results:
   #   plots - primary fields without clutter
   #   auxil - everything else
   #   everything - all fields

   plots <<- plots                                                                                    # save data frames as globals
   everything <<- everything
   auxil <<- auxil
   dropped <<- dropped
   rtk <<- rtk
   pct_cover <<- pct_cover
   sessions <<- sessions
   rtk_seq <<- rtk_seq



   #----------- write results
   cat('\n\nWriting result files...\n')

   # write GeoPackages
   pathP <- file.path(path, 'results')                                                                # preliminary results path

   q <- st_as_sf(everything, coords = c('easting', 'northing', 'elevation'), crs = 'EPSG:6491+5703')  # 3d GeoPackage of everything
   st_write(q, file.path(pathP, 'everything.gpkg'), append = FALSE, quiet = TRUE)

   primary <- st_as_sf(plots, coords = c('easting', 'northing', 'elevation'), crs = 'EPSG:6491+5703') # 3d GeoPackage of just primary fields
   st_write(primary, file.path(pathP, 'plots.gpkg'), append = FALSE, quiet = TRUE)


   q <- st_as_sf(plots, coords = c('easting', 'northing'), crs = 'EPSG:6491')                         # 2d GeoPackage of plot circles, just primary fields
   circles <- st_buffer(primary, plot_radius)
   st_write(circles, file.path(pathP, 'circles.gpkg'), append = FALSE, quiet = TRUE)



   # write tables
   tables <- c('plots', 'everything', 'dropped', 'rtk', 'pct_cover', 'sessions', 'auxil', 'rtk_seq')
   for(f in tables)
      write.table(eval(parse(text = f)), file.path(pathP, paste0(f, '.txt')), sep = '\t', row.names = FALSE, quote = FALSE)

   message('Preliminary tables and shapefiles written to ', pathP)

   message(nrow(plots), ' plots in final dataset')

}
