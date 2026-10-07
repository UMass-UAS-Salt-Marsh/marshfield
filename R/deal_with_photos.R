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
   e <- file.copy(f, r, recursive = FALSE)
   if(any(!e))
      message('Error copying ', sum(!e), 'photos')

   cat('Reading EXIF data...\n')
   e <- exifr::read_exif(r, , tags = c('DateTimeOriginal', 'GPSLatitude', 'GPSLongitude'))      # get EXIF data from plot photos
   plots[b, c('photo_date', 'photo_lat', 'photo_long')] <-
      e[, c('DateTimeOriginal', 'GPSLatitude', 'GPSLongitude')]
   plots$photo_date <- ymd_hms(plots$photo_date, tz = 'America/New_York')                       # fix dumb EXIF date formatting

   plots
}
