#' Check for valid RTK ids in plots data and report errors
#'
#' @param plots Plots data frame
#' @export


check_plot_rtkids <- function(plots) {


   v <- valid_rtkids(plots$rtk_id) & !is.na(plots$easting)                    # valid RTK ids in plots data that matched an RTK point
   if(any(!v)) {
      cat('\nBad, missing, or unmatched RTK ids in plot data:\n')
      print(data.frame(row = 1:nrow(plots), plot_id = plots$plot_id, rtk_id = plots$rtk_id, notes = plots$notes)[!v,], quote = FALSE, row.names = FALSE, right = FALSE)
   }
}
#' Top-level function to clean up and process UAS 2026 field data
#'
#' Some of this code is specific to the dataset, for example, test data are dropped,
#' a missing date is added, plot numbers that ended up in the site field are moved.
#' Most changes are driven by a number of tab-delimited parameter files that
#' specify changs. These were driven by actual errors in this dataset.
#'
#' Parameter files (tab-delimited text) in pars/ include:
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
#' In addition, the tables are saved as global varibles.
#'
#' @param path Path to source data
#' @param plot_file Name of plots file from Salt Marsh Data
#' @param session_file Name of sessions file from Salt Marsh data
#' @param rtk_dir Path to directory of RTK CSVs from Emlid
#' @param get_photos If TRUE, download all photos (checks to see if each exists first)
#' @param plot_radius Radius of plots (m)
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
                     rename_file = file.path(path, 'pars', 'rtk_names.txt'))  # gather and reproject RTK points from Emlid downloads

   plots <- read.csv(file.path(path, plot_file)) |>
      rename_cols(file.path(path, 'pars', 'plots_names.txt'))

   sessions <- read.csv(file.path(path, session_file)) |>
      rename_cols(file.path(path, 'pars', 'sessions_names.txt'))

   tests <- c('beech hill', 'Beech Hill Road', 'blue test', 'hoop',
              'Office', 'photos', 'pink test', 'test', 'test Aug 2',
              'yard', 'yard2', 'yard03')
   plots <- plots[!plots$site_name %in% tests, ]                           # remove test data that's still up on the server

   rownames(plots) <- 1:nrow(plots)


   if(get_photos)                                                          # download photos
      get_photos(unique(plots$photo_filename))


   plots$old_plot_id <- plots$plot_id                                      # save original versions of fields we're going to clean up
   plots$old_rtk_id <- plots$rtk_id
   plots$old_subclass <- plots$subclass
   rtk$old_rtk_id <- rtk$rtk_id


   plots <- fix_plotid_in_sitename(plots)                                  # fix missing plot ids when plot ids were in site_name
   plots$plot_id <- clean_plotids(plots$plot_id)                           # clean up formatting of plot ids, and add section 00 if necessary
   plots$rtk_id <- clean_rtkids(plots$rtk_id)                              # clean up formatting of RTK ids in plots
   rtk$rtk_id <- clean_rtkids(rtk$rtk_id)                                  # clean up formatting of RTK ids in RTK data
   plots <- fix_plots(plots, path)                                         # fix identified bad plot ids from fix_plots.txt
   rtk <- fix_rtkids(rtk, path)                                            # now rename duplicated RTK ids according to dedup_rtk.txt
   plots <- fix_sections(plots, path)                                      # correct section numbers from fix_sections.txt - LAST CHANGE TO PLOT IDs!


   check_plotid_dups(plots)                                                # stop if any plot id is used for more than one vegetation record

   pct_cover <- plots[, c('plot_id', 'species_code', 'species', 'pct_cover')]
   plots <- plots[, !names(plots) %in% c('species_code', 'species', 'pct_cover')]
   plots <- plots[!duplicated(plots$plot_id), ]                            # split out percent cover to a separate table and collapse plots table

   pct_cover <- fix_pctcover(pct_cover, path)                              # fix errors in percent cover according to fix_pct_cover.txt

   # clean up plots table some more
   plots$site <- 'ESX'                                                     # all plots are for site ESX
   plots$site_name <- 'Essex'
   plots$subclass <- as.numeric(stri_extract_first_regex(plots$subclass, '\\d+'))


   # join in observers from session
   sessions$observers <- gsub(',|and ', '', sessions$observers)            # clean up
   sessions$observers <- gsub('YJS', 'YKS', sessions$observers)            # fix typo
   sessions$observers <- gsub('et al', '+', sessions$observers)
   sessions$observers <- toupper(sessions$observers)

   plots <- merge(plots, sessions[, c('session_id', 'observers')],
                  by = 'session_id', all.x = TRUE, all.y = FALSE)


   plots <- deal_with_photos(plots, path)                                  # rename photos to plot numbers and pull plot date & time from EXIF data

   rtk$date <- ymd_hms(rtk$date, tz = 'UTC') |>
      with_tz('America/New_York')                                          # Put RTK dates into EDT


   plots <- fix_observers(plots, path)                                     # correct observers from fix_observers.txt
   pct_cover <- fix_species(pct_cover, path)                               # fix incorrect species names from fix_species.txt (bogus specific from app)

   dp <- drop_plots(plots, path)                                           # drop bad plots designated in drop_plots.txt (plots rejected on review are dropped later)
   plots <- dp[['plots']]                                                  # plots gets all plots that were not dropped
   dropped <- dp[['dropped']]                                              # dropped gets dropped plots, with drop_confirmd and drop_reason

   plots <- fix_plot_rtk(plots, path)                                      # correct bad RTKs in plots from fix_rtk.txt

   rtk_names <- c('rtk_id', 'easting', 'northing', 'elevation',
                  'longitude', 'latitude', 'ellipsoidal_height',
                  'lateral_rms', 'elevation_rms', 'date', 'PDOP', 'tilt')


   plots <- merge(plots, rtk[, rtk_names], by = 'rtk_id',
                  all.x = TRUE, all.y = FALSE)                             # now that RTK ids are cleaned up, join plots to RTK data

   plots <- sort_plots(plots, path)                                        # sort plots by date, tablet, section, and plot number

   plots <- rtk_sequence(plots)                                            # flag out-of-sequence RTK points (must be sorted)
   cat('\n', sum(plots$rtk_nonseq), ' RTK points seem to be out of sequence, marked by rtk_nonseq:\n', sep = '')
   print(plots[plots$rtk_nonseq, c('plot_id', 'rtk_id')], right = FALSE)
   rtk_seq <- plots[, c('date', 'plot_id', 'rtk_id', 'rtk_nonseq', 'notes')]
   cat('\nSee global rtk_seq and rtk_seq.txt to resolve these\n')


   pd <- process_reviews(plots, dropped, path)                              # if review data are available, drop rejected plots
   plots <- pd[['plots']]
   dropped <- pd[['dropped']]


   # split plots (primary data) and auxil (corresponding, with extra info)
   primary <- c('plot_id', 'date', 'subclass', 'site', 'rtk_id', 'easting', 'northing', 'elevation', 'lateral_rms', 'section', 'observers',
                'notes', 'reviewed', 'problems', 'split', 'coverr', 'comments', 'has_photo')
   auxil <- plots[, c('plot_id', names(plots)[!names(plots) %in% primary])]
   everything <- plots
   plots <- plots[, primary]


   # Check for remaining errors
   check_plotids(plots)                                                    # check plot ids, reporting invalid ids
   check_plotid_dates(plots)                                               # Check for bad dates in plot ids
   rtk <- check_rtk_dups(rtk)                                              # check for duplicate RTK ids and flag them
   check_plot_rtkids(plots)                                                # check for valid RTK ids in plots data
   check_rtk_rtkids(rtk)                                                   # check for valid RTK ids in RTK data
   plots <- check_plot_rtk_dups(plots)                                     # check for duplicated RTK ids in plot data and flag them
   check_pct_cover(plots, pct_cover)                                       # check percent cover for out of range errors


   cat('\n\nWriting result files...\n')                                    #----------- write results
   pathR <- file.path(path, 'results')                                     # results path
   if(!dir.exists(pathR))                                                  # make sure result directory exists
      dir.create(pathR, recursive = TRUE)


   # write GeoPackages
   q <- st_as_sf(everything, coords = c('easting', 'northing', 'elevation'), crs = 'EPSG:6491+5703', na.fail = FALSE)  # 3d GeoPackage of everything
   st_write(q, file.path(pathR, 'everything.gpkg'), append = FALSE, quiet = TRUE)

   primary <- st_as_sf(plots, coords = c('easting', 'northing', 'elevation'), crs = 'EPSG:6491+5703', na.fail = FALSE) # 3d GeoPackage of just primary fields
   st_write(primary, file.path(pathR, 'plots.gpkg'), append = FALSE, quiet = TRUE)

   q <- st_as_sf(plots, coords = c('easting', 'northing'), crs = 'EPSG:6491', na.fail = FALSE)                      # 2d GeoPackage of plot circles, just primary fields
   circles <- st_buffer(q, plot_radius)
   st_write(circles, file.path(pathR, 'circles.gpkg'), append = FALSE, quiet = TRUE)


   # write tables
   tables <- c('plots', 'everything', 'dropped', 'rtk', 'pct_cover', 'sessions', 'auxil', 'rtk_seq')
   for(v in tables) {                                                                                 # for each table
      x <- get(v)
      write.table(x, file.path(pathR, paste0(v, '.txt')),
                  sep = '\t', row.names = FALSE, quote = FALSE)                                       #    write it as a tab-delimited text file
      assign(v, x, envir = .GlobalEnv)                                                                #    and save it as a global variable
   }

   message('Tables and GeoPackages written to ', pathR)
   message(nrow(plots), ' plots in final dataset')
}
#' Check for agreement in plot id and RTK id sequences
#'
#' Goal is to find potential errors where sequences diverge, possibly from skipped RTK. Each
#' tablet/day/section is treated separately.
#'
#' Within each tablet/day/section, RTK point numbers are expected to increment with plot
#' numbers, so `rtk point - plot number` should be constant. Gaps in plot numbers are fine as
#' long as the RTK points skip by the same amount. The expected offset is set by the first plot
#' in the tablet/day/section. When the RTK unit/date changes partway through, the expected offset
#' for the new RTK run is taken as its lowest point number minus its lowest plot number (i.e., we
#' assume the first RTK point from the new unit went with the first plot).
#'
#' The first plot in each tablet/day/section is always FALSE. A change in RTK unit/date partway
#' through a tablet/day/section is TRUE, as is any plot whose RTK point is out of sequence.
#'
#' Plots without a valid RTK id are skipped (and are FALSE); they're reported by `check_plot_rtkids`.
#'
#' Plots must be sorted in plot id order (see `sort_plots`).
#'
#' @param plots Plots data frame, sorted by `plot_id`
#' @returns Plots data frame with new column, `rtk_nonseq`, TRUE if `rtk_id` is out of sequence
#' @export


rtk_sequence <- function(plots) {


   ok <- valid_rtkids(plots$rtk_id)             # skip plots without a valid RTK id
   all_plots <- plots
   plots <- plots[ok, ]

   y <- strsplit(toupper(plots$plot_id), '-')
   y1 <- sapply(y, '[[', 1)                     # part 1: tablet and date
   y2 <- sapply(y, '[[', 2)                     # part 2: section
   y3 <- sapply(y, '[[', 3)                     # part 3: plot number

   r <- strsplit(toupper(plots$rtk_id), '-')
   r1 <- sapply(r, '[[', 1)                     # part 1: RTK unit and date
   r2 <- sapply(r, '[[', 2)                     # part 2: RTK point number


   tds <- paste(y1, y2, sep = '-')
   pn <- as.numeric(y3)
   new <- tds != c('xxx', tds[-length(tds)])    # starts of new plot sequences

   new_rtk <- r1 != c('xxx', r1[-length(r1)])   # starts of new RTK sequences
   rn <- as.numeric(r2)

   seg <- cumsum(new | new_rtk)                 # runs with the same tablet/day/section and RTK unit/date
   first <- which(new | new_rtk)                # first row of each run

   offset <- rep(NA, max(seg))
   for(i in seq_along(first)) {
      s <- seg == i
      if(new[first[i]])                         # run starts a tablet/day/section: anchor on first plot
         offset[i] <- rn[first[i]] - pn[first[i]]
      else                                      # RTK changed midstream: anchor on lowest RTK point
         offset[i] <- min(rn[s]) - min(pn[s])
   }

   all_plots$rtk_nonseq <- FALSE
   all_plots$rtk_nonseq[ok] <- !new & (new_rtk | (rn - pn) != offset[seg])
   all_plots
}
#' Check for plot ids shared by more than one vegetation record and throw an error
#'
#' The plots table has one row per species, so plot ids are repeated within a vegetation record.
#' A plot id that shows up in more than one vegetation record means two different plots ended up
#' with the same id; collapsing the table would silently merge them.
#'
#' @param plots Plots data frame, before collapsing to one row per plot
#' @export


check_plotid_dups <- function(plots) {


   n <- tapply(plots$vegetation_record_id, plots$plot_id, function(x) length(unique(x)))   # number of vegetation records for each plot id
   d <- names(n)[n > 1]

   if(length(d) > 0) {
      cat('\nPlot ids used for more than one vegetation record:\n')
      x <- unique(plots[plots$plot_id %in% d, c('plot_id', 'old_plot_id', 'vegetation_record_id', 'notes')])
      print(x[order(x$plot_id), ], quote = FALSE, row.names = FALSE, right = FALSE)
      stop(length(d), ' plot ids are used for more than one vegetation record (fix in fix_plots.txt or fix_sections.txt)', call. = FALSE)
   }
}
#' If review data are available, drop rejected plots
#'
#' @param plots Plots data frame
#' @param dropped Dropped plots data frame
#' @param path Base path
#' @returns Named list, `plots` = plots data frame, `dropped` = dropped plots data frame
#' @export


process_reviews <- function(plots, dropped, path) {


   f <- file.path(path, 'pars', 'review.txt')

   if(file.exists(f)) {
      cat('\nJoining in review fields from view_plots()...\n')
      review <- read.table(f, sep = '\t', header = TRUE, quote = '')
      keep <- c('plot_id', 'reviewed', 'problems', 'rejected', 'split', 'coverr', 'comments')
      plots <- merge(plots, review[, keep], by = 'plot_id', all.x = TRUE, all.y = FALSE)

      plots <- sort_plots(plots, path)                                  # sort plots by date, tablet, section, and plot number

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
   else
      plots[c('reviewed', 'problems', 'split', 'coverr', 'comments')] <- NA

   list(plots = plots, dropped = dropped)
}
#' Rename duplicated RTK ids according to dedup_rtk.txt
#'
#' Parameter file:
#' - `dedup_rtk.txt` Drop duplicated RTKs with same rtk_id: `rtk_id`, `seq`, `new`, `reason`
#'
#' @param rtk RTK data frame
#' @param path Base path
#' @returns RTK data frame
#' @export


fix_rtkids <- function(rtk, path) {


   dup_rtk <- read.table(file.path(path, 'pars/dedup_rtk.txt'), sep = '\t', header = TRUE, quote = '')
   rtk <- rtk[order(rtk$date), ]                      # sort RTKs by date so sequence works out (assumed to be in order in dedup_rtk.txt)

   old <- rtk$rtk_id
   u <- unique(dup_rtk$rtk_id)
   for(i in u) {
      b <- rtk$rtk_id %in% i
      if(any(b)) {
         if(sum(b) != sum(dup_rtk$rtk_id %in% i))
            stop('You have the wrong number of duplicates in dedup_rtk.txt for ', i)
         rtk$rtk_id[b] <- dup_rtk$new[dup_rtk$rtk_id %in% i]
      }
   }

   cat('\nRenamed ', sum(rtk$rtk_id != old, na.rm = TRUE), ' RTK ids in rtk\n', sep = '')

   rtk
}
#' Rename photos to plot numbers and pull plot date & time from EXIF data
#'
#' Photos have been downloaded to photos/downloads; here, we copy them to photos/plots renamed to `<plot id>.jpg`.
#'
#' @param plots Plots data frame
#' @param path Base path
#' @returns Plots data frame
#' @export


deal_with_photos <- function(plots, path) {


   b <- plots$photo_filename != ''                                                              # skip plots without photos
   plots$has_photo <- b

   f <- file.path(path, 'photos', 'downloads', basename(plots$photo_filename[b]))
   p <- file.path(path, 'photos', 'plots')

   d <- list.files(p)
   invisible(file.remove(file.path(p, d)))

   cat('\nCopying photos to ', p, '...\n', sep = '')
   r <- file.path(p, paste0(plots$plot_id[b], '.jpg'))
   ok <- file.copy(f, r, recursive = FALSE)
   if(any(!ok))
      message('Error copying ', sum(!ok), ' photos')

   cat('Reading EXIF data...\n')
   e <- exifr::read_exif(r[ok], tags = c('DateTimeOriginal', 'GPSLatitude', 'GPSLongitude')) |>   # get EXIF data from plot photos
      as.data.frame()
   i <- match(basename(r), basename(e$SourceFile))                                              # match EXIF rows to plots by file name
   plots[b, c('photo_date', 'photo_lat', 'photo_long')] <-
      e[i, c('DateTimeOriginal', 'GPSLatitude', 'GPSLongitude')]
   plots$photo_date <- ymd_hms(plots$photo_date, tz = 'America/New_York')                       # fix dumb EXIF date formatting

   plots
}
#' Clean RTK ids for UMass UAS 2026
#'
#' Valid ids are in the form <RTK unit><month><day>-<point no>. Valid RTK units are W, X, Y, Z,
#' optionally followed by a digit. Valid month letters are J - July, A - August. The day should be 2
#' digits and the point number is 3 digits, all with leading zeros. All characters are uppercase.
#' This function makes the formatting corrections.
#'
#' To check whether an RTK id is valid (ignoring) formatting niceties, use the companion function
#' `valid_rtkids`.
#'
#' @param x A vector of RTK ids
#' @returns A vector of RTK ids with the formatting cleaned up
#' @export


clean_rtkids <- function(x) {


   y <- strsplit(toupper(x), '-')
   l <- sapply(y, length)

   for(i in seq_along(y))                       # if there's a section number, drop it and include a digit (0) in the RTK id if there isn't one
      if(l[i] == 3)
         y[[i]] <- c(sub('^([WXYZ])([JA])', '\\10\\2', y[[i]][[1]]), y[[i]][[3]])


   w <- sapply(y, length) == 2                  # we're only going to work with ids that have two elements (after fixing 3)

   y1 <- sapply(y[w], '[[', 1)                  # part 1: RTK unit and date
   y2 <- sapply(y[w], '[[', 2)                  # part 2: point number

   u <- sub('^([WXYZ]\\d?[JA]).*', '\\1', y1)   # RTK unit (with optional digit) and month
   d <- sub('^[WXYZ]\\d?[JA]', '', y1)          # day
   y1 <- paste0(u, sprintf('%02d', suppressWarnings(as.numeric(d))))
   y2 <- sprintf('%03d', suppressWarnings(as.numeric(y2)))

   z <- x
   z[w] <- paste(y1, y2, sep = '-')


   v <- valid_rtkids(z)                         # undo changes that leave invalid ids
   z[!v] <- x[!v]


   z
}
#' Check plot ids, reporting invalid ids
#'
#' @param plots Plots data frame
#' @export


check_plotids <- function(plots) {


   v <- valid_plotids(plots$plot_id)               # valid plot ids

   if(any(!v)) {
      cat('\nBad plot ids:\n')
      print(data.frame(row = 1:nrow(plots), plotid = plots$plot_id, notes = plots$notes)[!v,], quote = FALSE, row.names = FALSE)
   }
}
#' fix missing plot ids when plot ids were in site_name
#'
#' @param plots Plots data frame
#' @returns Plots data frame
#' @export


fix_plotid_in_sitename <- function(plots) {


   plotno <- suppressWarnings(as.numeric(plots$plot_id))
   bad <- !is.na(plotno)                                                   # plot ids that weren't set, and thus were assigned 1, 2, 3, ...
   fixable <- valid_plotids(plots$site_name)                               # some plot ids ended up in sitename, which is otherwise variable and useless
   x <- strsplit(plots$site_name[bad & fixable], '-')
   plots$plot_id[bad & fixable] <- paste(sapply(x, '[[', 1), sapply(x, '[[', 2), sprintf('%03d', plotno[bad & fixable]), sep = '-')

   plots
}
#' Check for valid RTK ids in RTK data and report errors
#'
#' @param rtk RTK data frame
#' @export


check_rtk_rtkids <- function(rtk) {


   v <- valid_rtkids(rtk$rtk_id)                                             # valid RTK ids in RTK data
   if(any(!v)) {
      cat('\nBad RTK ids in RTK data:\n')
      print(data.frame(row = 1:nrow(rtk), rtk_id = rtk$rtk_id)[!v,], quote = FALSE, row.names = FALSE)
   }
}
#' Rename and delete columns in data frame from parameter file
#'
#' @param x Data frame
#' @param file Delimited text file with two columns:
#'   - `old` existing column names
#'   - `new` new names for columns, or `-` to delete columns
#' @returns Data frame with new column names and dropped columns
#' @export


rename_cols <- function(x, file) {


   p <- read.table(file, sep = '\t', header = TRUE)
   x <- x[, !names(x) %in% p$old[p$new == '-']]           # drop columns we don't like
   p <- p[!p$new %in% c('', '-'), ]

   i <- match(p$old, names(x))
   names(x)[i] <- p$new
   x
}
#' Correct bad RTKs in plots from parameter files
#'
#' Parameter file:
#' - `fix_rtk.txt` RTK ids to reassign (usually thanks to off-by-one errors): `plot_id`,
#'   `new_rtk_id`, `fix_rtk_confirmed`, `fix_rtk_reason`
#'
#' @param plots Plots data frame
#' @param path Base path
#' @returns Plots data frame
#' @export


fix_plot_rtk <- function(plots, path) {


   fix_rtk <- read.table(file.path(path, 'pars/fix_rtk.txt'), sep = '\t', header = TRUE, quote = '')
   plots <- merge(plots, fix_rtk, by = 'plot_id', all.x = TRUE)
   plots$fix_rtk_confirmed[is.na(plots$fix_rtk_confirmed)] <- FALSE
   plots$fix_rtk_reason[is.na(plots$fix_rtk_reason)] <- ''

   b <- !is.na(plots$new_rtk_id)
   plots$rtk_id[b] <- plots$new_rtk_id[b]
   plots <- plots[, !names(plots) %in% 'new_rtk_id']

   plots
}
#' Fix errors in percent cover
#'
#' Parameter file:
#' - `fix_pct_cover.txt` Changes to percent cover fields: `plot_id`, `species_code`, `pct_cover`, `reason`
#'
#' @param pct_cover Percent cover data frame
#' @param path Base path
#' @returns Percent cover data frame
#' @export


fix_pctcover <- function(pct_cover, path) {


   x <- read.table(file.path(path, 'pars/fix_pct_cover.txt'), sep = '\t', header = TRUE, quote = '')
   key_pc <- paste(pct_cover$plot_id, pct_cover$species_code)
   key_x <- paste(x$plot_id, x$species_code)

   new <- x[, c('plot_id', 'species_code', 'pct_cover')]
   new$species <- pct_cover$species[match(new$species_code, pct_cover$species_code)]   # look up names

   pct_cover <- rbind(pct_cover[!key_pc %in% key_x, ], new[, names(pct_cover)])
   pct_cover <- pct_cover[order(pct_cover$plot_id, pct_cover$species_code), ]
   rownames(pct_cover) <- NULL

   pct_cover
}
#' Check for wroong dates in plot ids
#'
#' @param plots Plots data frame
#' @export


check_plotid_dates <- function(plots) {


   pd <- data.frame(month = ifelse(substr(plots$plot_id, 2, 2) == 'J', 7, 8), day = suppressWarnings(as.numeric(substr(plots$plot_id, 3, 4))))
   ad <- data.frame(month = month(plots$date), day = day(plots$date))
   b <- apply(pd != ad, 1, 'any')
   b[is.na(b)] <- TRUE                                                                 # catch malformed names

   if(any(b)) {
      cat('\n', sum(b), ' plots have plot_id with wrong date or malformed name (add these to fix_plots.txt):', sep = '')
      print(plots$plot_id[b])
      cat('\n')
   }
}
#' Fix observer initials for correctness and consistency
#'
#' Parameter file:
#' - `fix_observers.txt` Fix errors and inconsistencies in observers: `old`, `new`, `reason`.
#'
#' @param plots Plots data frame
#' @param path Base path
#' @returns Plots data frame
#' @export


fix_observers <- function(plots, path) {


   fix_obs <- read.table(file.path(path, 'pars/fix_observers.txt'), sep = '\t', header = TRUE, quote = '')
   b <- match(plots$observers, fix_obs$old)
   plots$observers[!is.na(b)] <- fix_obs$new[b[!is.na(b)]]

   plots
}
#' Fix incorrect species names in data file (bogus specific for genera)
#'
#' Parameter file:
#' - `fix_species.txt` Change genera incorrectly listed as species by app in percent cover
#'
#' @param pct_cover Percent cover data frame
#' @param path Base path
#' @returns Percent cover data frame
#' @export


fix_species <- function(pct_cover, path) {


   sp <- read.table(file.path(path, 'pars/fix_species.txt'), sep = '\t', header = TRUE, quote = '')
   b <- match(pct_cover$species, sp$old)
   pct_cover$species[!is.na(b)] <- sp$new[b[!is.na(b)]]

   pct_cover
}
#' Drop bad plots designated in drop_plots.txt
#'
#' Parameter file:
#' - `drop_plots.txt` Plots to drop: `plot_id`, `drop_confirmed`, and `drop_reason`
#'
#' @param plots Plots data frame
#' @param path Base path
#' @returns Named list of `plots` = plots data frame, `dropped` = plots that were dropped
#' @export


drop_plots <- function(plots, path) {


   drop <- read.table(file.path(path, 'pars/drop_plots.txt'), sep = '\t', header = TRUE, quote = '')
   drop$drop <- TRUE
   plots <- merge(plots, drop, by = 'plot_id', all.x = TRUE)
   plots$drop[is.na(plots$drop)] <- FALSE
   dropped <- plots[plots$drop, ]
   plots <- plots[!plots$drop, !names(plots) %in% c('drop_confirmed', 'drop_reason', 'drop')]

   cat('\n', nrow(dropped), ' plots dropped; ', nrow(plots), ' remaining. Dropped plots:\n', sep = '')
   print(dropped[, c('plot_id', 'drop_confirmed', 'drop_reason')], row.names = FALSE, right = FALSE)

   list(plots = plots, dropped = dropped)
}
#' Check for duplicate RTK ids and flag them
#'
#' @param rtk RTK data frame
#' @returns RTK data frame
#' @export


check_rtk_dups <- function(rtk) {


   d <- duplicated(rtk$rtk_id)                                                # dup ids in RTK data
   if(any(d)) {
      cat('\nDuplicated RTK ids in RTK data: (these should have been fixed in dedup_rtk.txt)\n')
      d <- rtk$rtk_id %in% rtk$rtk_id[d]
      print(data.frame(row = 1:nrow(rtk), rtk_id = rtk$rtk_id, old_rtk_id = rtk$old_rtk_id)[d,], quote = FALSE, row.names = FALSE)
      rtk$rtkid_dup[d] <- TRUE
   }

   rtk
}
#' Check for duplicated RTK ids in plot data and flag them
#'
#' @param plots Plots data frame
#' @returns Plots data frame
#' @export


check_plot_rtk_dups <- function(plots) {


   d <- duplicated(plots$rtk_id)
   if(any(d)) {
      cat('\n', sum(d), ' duplicated RTK ids in plot data:\n', sep = '')
      d <- plots$rtk_id %in% plots$rtk_id[d]
      print(data.frame(row = 1:nrow(plots), plot_id = plots$plot_id, rtk_id = plots$rtk_id,
                       old_rtk_id = plots$old_rtk_id, notes = plots$notes)[d,],
            quote = FALSE, row.names = FALSE, right = FALSE)
      plots$rtkid_dup[d] <- TRUE
   }

   plots
}
#' Check percent cover for out-of-range values
#'
#' @param plots Plots data frame
#' @param pct_cover Percent cover data frame
#' @importFrom dplyr group_by summarise
#' @export


check_pct_cover <- function(plots, pct_cover) {


   x <- pct_cover |>
      group_by(plot_id) |>
      summarise(sum = sum(pct_cover)) |>
      data.frame()

   x$sum[is.na(x$sum)] <- 0                     # NAs are really 0 recorded percent cover

   over <- x[x$sum > 100, ]
   if(nrow(over) > 0) {
      cat('\n', nrow(over), ' plots with > 100 percent cover:\n', sep = '')
      print(over, row.names = FALSE)
   }

   zero <- x[x$sum == 0, ]
   if(nrow(zero) > 0) {
      cat('\n', nrow(zero), ' plots with zero percent cover:\n', sep = '')
      y <- merge(zero, plots, by = 'plot_id')[, c('plot_id', 'subclass')]
      print(y[order(y$subclass), ], row.names = FALSE, quote = FALSE)
   }

}
#' Sort plots in canonical order
#
#' Sort order is field day, tablet id, section, plot number. Days are defined in field_days.txt column day, as J21, J22, ..., A07.
#'
#' @param plots Plots data frame. Requires column plot_id
#' @param path Project file path
#' @returns Plots data frame, sorted
#' @export


sort_plots <- function(plots, path) {


   days <- read.table(file.path(path, 'pars/field_days.txt'), sep = '\t', header = TRUE, quote = '')
   days$day_n <- 1:nrow(days)

   y <- strsplit(toupper(plots$plot_id), '-')
   y1 <- sapply(y, '[[', 1)                     # part 1: tablet and date
   y2 <- sapply(y, '[[', 2)                     # part 2: section
   y3 <- sapply(y, '[[', 3)                     # part 3: plot number

   plots <- plots[, !names(plots) %in% c('tablet', 'day', 'section', 'plot_no', 'day_n')]

   plots$tablet <- substr(y1, 1, 1)
   plots$day <- substr(y1, 2, 4)
   plots$section <- y2
   plots$plot_no <- y3

   plots <- merge(plots, days, by = 'day', all.x = TRUE, all.y = FALSE)
   plots <- plots[order(plots$day_n, plots$tablet, plots$section, plots$plot_no), ]
   row.names(plots) <- NULL
   plots

}
#' Clean up incorrect section numbers
#'
#' Section number errors are generally from a crew not closing the app at the end of a section
#'
#' Parameter file:
#' #' - `fix_sections.txt` Split field days into sections: `start`, `end`, `is_section`, `reason`
#'
#' @param plots Plots data frame
#' @param path Base path
#' @returns Plots data frame
#' @export


fix_sections <- function(plots, path) {


   fix <- read.table(file.path(path, 'pars/fix_sections.txt'), sep = '\t', header = TRUE, quote = '')
   y <- strsplit(fix$start, '-')
   for(i in 1:length(y)) {                                                                                     # For each row in fix_sections,
      seq <- as.numeric(y[[i]][3]):as.numeric(fix$end[i])   # plot number sequence                             #    plot number sequence
      old <- paste(y[[i]][1], y[[i]][2], sprintf('%03d', seq), sep = '-')                                      #    old plot ids
      new <- paste(y[[i]][1], sprintf('%02d', fix$is_section[i]), sprintf('%03d', seq), sep = '-')             #    new plot ids
      b <- match(plots$plot_id, old)                                                                           #    for every row in plots (including duplicate plot ids), index into old
      plots$plot_id[!is.na(b)] <- new[b[!is.na(b)]]                                                            #    update section in plot id
   }

   plots
}
