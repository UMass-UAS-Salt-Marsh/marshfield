#' Gather RTK GPS points for UMass UAS 2026
#'
#' Reads RTK points from Emlid files and reprojects to NAD83(2011) / Massachusetts
#' Mainland + NAVD88(GEOID18) height. Optionally writes a preliminary geopackage of all
#' RTK points.
#'
#' @param path Path to folder of RTK files (.CSVs with nothing else in folder)
#' @param result Path and filename of preliminary result file; use NULL to skip writing it
#' @returns Data frame of RTK points with all info
#' @importFrom utils read.csv
#' @export


gather_rtk <- function(path = 'C:/Work/saltmarsh/data/uas2026/field/RTK',
                       result = 'C:/Work/saltmarsh/data/uas2026/field/prelim/rtk.gpkg') {


   x <- list.files(path)
   z <- NULL

   for(i in x) {
      f <- read.csv(file.path(path, i))
      if(is.null(f$Tilt.angle))
         f <- cbind(f, Tilt.angle = NA)

      if(is.null(z))
         z <- f

      else
         z <- rbind(z, f)
   }

   y <- reproj_rtk(z)      # reproject and validate RTK points

   q <- st_as_sf(y, coords = c('Easting', 'Northing', 'Elevation'), crs = 'EPSG:6491+5703')

   if(!is.null(result)) {
      st_write(q, result, append = FALSE)
      message(nrow(y), ' points projected and preliminary version written to ', result)
   }

   invisible(y)
}
