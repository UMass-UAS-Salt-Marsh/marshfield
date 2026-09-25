#' Add overviews and statistics to geoTIFFs
#'
#' Overviews and statistics are created as sidecar files, `*.tif.ovr`
#' and `*.tif.aux.xml`, so you can take them or leave them. If base files
#' are updated, you'll need to rerun this.
#'
#' @param files Vector of geoTIFF file names
#' @param path Common path to files
#' @param nodata Value to use for nodata if it wasn't set on creation
#' @importFrom sf gdal_addo gdal_utils
#' @importFrom terra nlyr
#' @export


add_overviews <- function(files = c('cst_2026_08_21_mid_micap_ortho.tif',
                                    'cst_2026_08_07_mid_micap_ortho.tif',
                                    'cst_2026_07_20_low_micap_ortho.tif'),
                          path = 'K:/projects/uas_veg/sites/cst/ortho/',
                          nodata = 65535) {


   write_nodata_aux <- function(f, nodata) {
      n <- nlyr(rast(f))
      bands <- sprintf('  <PAMRasterBand band="%d">\n    <NoDataValue>%s</NoDataValue>\n  </PAMRasterBand>',
                       seq_len(n), nodata)
      writeLines(c("<PAMDataset>", bands, "</PAMDataset>"), paste0(f, ".aux.xml"))
   }


   files <- file.path(path, files)

   for(x in files) {
      message('Processing ', x, '...')
      write_nodata_aux(x, nodata)                                             # 1. NoData first

      gdal_addo(x,
                overviews = c(2, 4, 8, 16, 32),
                method = "AVERAGE",
                read_only = TRUE,
                config_options = c(COMPRESS_OVERVIEW = "DEFLATE",
                                   BIGTIFF_OVERVIEW = "IF_SAFER"))            # 2. Overviews

      gdal_utils("info", x, options = c("-stats", "-hist"), quiet = TRUE)     # 3. Statistics
   }

   message('Finished processing ', length(files), ' files')
}
