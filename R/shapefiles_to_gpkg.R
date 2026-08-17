#' Package shapefiles to a gpkg for Avenza
#'
#' Fire up VPN before running if you're writing over the network to marsh01.
#'
#' Files are hard-coded in this version. Note that open space is clipped; the other files are not.
#'
#' @param source Source directory
#' @param dest Destination directory
#' @param openspace Path to open space shapefile
#' @param clipper Path to site AOI shapefile for clipping open space
#' @importFrom sf st_read st_crop st_write
#' @importFrom zip zipr


shapefiles_to_gpkg <- function(source = 'C:/Work/saltmarsh/data/uas2026/result',
                               dest = '//marsh01.ecs.umass.edu/web/uas/uas2026/',
                               openspace = 'C:/GIS/GIS/openspace/OPENSPACE_POLY.shp',
                               clipper = 'C:/GIS/Orthophotos/2025/Essex2025.shp') {


   zip_layer <- function(base) {
      parts <- list.files(source,
                          pattern = paste0('^', base, '\\.(shp|shx|dbf|prj|cpg)$'))
      out <- file.path(source, paste0(base, '.zip'))
      zip::zipr(zipfile = out, files = parts, root = source)  # root= stores files flat at the zip root
      file.copy(out, dest, overwrite = TRUE)
      out
   }


   # clip open space to our area of interest
   open <- st_read(openspace)
   clip <- st_read(clipper)
   open <- st_crop(open, clip)
   st_write(open, file.path(source, 'open_space.shp'), layer = 'open_space', append = FALSE)


   zip_layer('open_space')
   zip_layer('castle_sections')
   zip_layer('work_areas')
}
