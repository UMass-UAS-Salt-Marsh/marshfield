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
#' - `fix_species.txt` Change genera incorrectly listed as species by app in percent cover
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


   plots <- deal_with_photos(plots, path)                                  # rename photos to plot numbers and pull plot date & time from EXIF data

   rtk$date <- ymd_hms(rtk$date, tz = 'UTC') |>
      with_tz('America/New_York')                                          # RTK points are labeled as UTC, but are really EDT

   plots <- fix_observers(plots, path)                                     # correct observers




   pct_cover <- fix_species(pct_cover, path)                               # fix incorrect species names in data file (bogus specific for genera)


   dp < drop_plots(plots, path)                                            # drop bad plots designated in drop_plots.txt (plots rejected on review are dropped later)
   plots <- dp[['plots']]                                                  # plots gets all plots that were not dropped
   dropped <- dp[['dropped']]                                              # dropped gets dropped plots, with drop_confirmd and drop_reason




   plots <- fix_plot_rtk(plots, path)                                      # correct bad RTKs in plots from parameter files



   rtk_names <- c('rtk_id', 'easting', 'northing', 'elevation',
                  'longitude', 'latitude', 'ellipsoidal_height',
                  'lateral_rms', 'elevation_rms', 'date', 'PDOP', 'tilt')
   plots <- merge(plots, rtk[, rtk_names], by = 'rtk_id', all.y = FALSE)   # now that RTK ids are cleaned up, join plots to RTK data



   check_plotid_dates(plots)                                               # Check for bad dates in plot ids



   plots <- sort_plots(plots, path)                                        # sort plots by date, tablet, section, and plot number



   plots <- rtk_sequence(plots)                                            # flag out-of-sequence RTK points (must be sorted)
   cat('\n', sum(plots$rtk_nonseq), ' RTK points seem to be out of sequence, marked by rtk_nonseq:\n', sep = '')
   print(plots[plots$rtk_nonseq, c('plot_id', 'rtk_id')], right = FALSE)
   rtk_seq <- plots[, c('date', 'plot_id', 'rtk_id', 'rtk_nonseq', 'mplotid_err', 'rtkid_err', 'notes')]
   cat('\nSee at global rtk_seq and rtk_seq.txt to resolve these\n')





  pd <- process_reviews(plots, dropped, path)                              # if review data are available, drop rejected plots






   ............................................................................




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




   cat('\n\nWriting result files...\n')                                                               #----------- write results

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

   message('Tables and GeoPackages written to ', pathP)
   message(nrow(plots), ' plots in final dataset')
}
