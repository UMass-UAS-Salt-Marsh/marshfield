#' Format sections shapefile for Avenza
#'
#' @param source Name of source shapefile
#' @param result Name of result shapefile
#' @param path Directory of shapefiles
#' @importFrom sf st_read st_write
#' @export

format_sections <- function(source = 'sections.shp', result = 'castle_sections.shp',
                            path = 'C:/Work/saltmarsh/data/uas2026/result') {


   v <- st_read(file.path(path, source))
   v$acres <- round(v$acres, 1)
   v$name <- sprintf("Sec %02d (target %d)", v$section, v$plots)
   st_write(v, file.path(path, result), append = FALSE)

}
