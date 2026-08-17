#' Process MassGIS 2025 orthos into a geoTIFF for Avenza
#
#' Pick out tiles here: https://maps.massgis.digital.mass.gov/MassMapper/MassMapper.html?bl=2025%20Aerial%20Imagery__100&l=massgis:GISDATA.COQ2025INDEX_POLY__GISDATA.COQ2025INDEX_POLY::Labels__ON__100,massgis:GISDATA.COQ2025INDEX_POLY__GISDATA.COQ2025INDEX_POLY::Default__ON__100&b=-71.51206970214845,42.235635122140614,-70.7292938232422,42.538409837545586
#'
#' @param path Path to downloaded orthophoto tiles (.JP2)
#' @param aoi Shapefile with area of interest - result will be clipped to its bounding box
#' @param result Path and filename of result geoTIFF. It'll be a Cloud Optimized geoTIFF.
#' @importFrom terra vect project crop writeRaster vrt ext
#' @export


orthos_to_tiff <- function(path = 'C:/GIS/Orthophotos/2025/',
                           aoi = 'C:/GIS/Orthophotos/2025/Essex2025.shp',
                           result = 'C:/GIS/Orthophotos/2025/essex2025.tif') {


   # 1. Virtual mosaic of the tiles — no memory load, no intermediate file
   tiles <- list.files(path, pattern = '\\.jp2$', full.names = TRUE)
   m <- vrt(tiles)

   # 2. Drop NIR -> RGB only (bands are 1=R 2=G 3=B 4=NIR)
   m <- m[[1:3]]

   # 3. Clip to study area's bounding box
   aoi <- vect(aoi)
   aoi <- project(aoi, m)          # reproject AOI to the imagery CRS if needed
   clipped <- crop(m, ext(aoi))    # rectangular basemap; use mask(clipped, aoi) if you want it cut to the polygon

   # 4. Write an Avenza-friendly COG
   writeRaster(clipped, result,
               filetype = 'COG',
               gdal = c('COMPRESS=JPEG', 'QUALITY=85', 'BLOCKSIZE=512'),
               overwrite = TRUE)
}
