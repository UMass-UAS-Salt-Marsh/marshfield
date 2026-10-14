#' Finish orthophotos after a Metashape run: compressing/set nodata/statistics/overviews
#'
#' Performs standard post-processing for orthophotos created by Agisoft Metashape. Files are
#' copied from a source directory (by default, `ortho_raw/`) to a result directory (`ortho/`)
#' using a lossless compression algorithm that should reduce the file size by about a third,
#' and setting the nodata value, adding statistics, and building overviews (also known as
#' pyramids). The result files will be quick to load and view in ArcGIS Pro or QGIS, as
#' overviews will show the data quickly when zoomed out, and statistics have been
#' pre-calculated. Setting the nodata value properly will result in correct scaling in
#' displays.
#'
#' Does the following:
#' 1. Compresses the file, using ZSTD, a space-efficient lossless algorithm
#' 2. Set the nodata value to 65535
#' 3. Adds statistics
#' 4. Adds overviews (as a _ovr sidecar file, by default)
#'
#' Overviews and statistics are created as sidecar files, `*.tif.ovr`
#' and `*.tif.aux.xml`, so you can take them or leave them. If base files
#' are updated, you'll need to rerun this.
#'
#' Only run this for 2-byte integer files (INT2U). This should work for all orthophotos.
#'
#' Files are copied from <base_path>/<source_path>/<file> to <base_path>/<result_path>/<file>.
#' (The result path is created if it doesn't exist). The default is set up for the `uas_veg`
#' project; for other projects, you'll need to specify `base_path`.
#'
#' @param files Vector of geoTIFF file names
#' @param site Optional site abbreviation, inserted in place of `<site>` in paths and filenames
#' @param base_path Optional base path, shared between source and result paths (use
#' `base_path = ''` to skip a base path)
#' @param source_path source path, appended to base_path if present
#' @param result_path result path, appended to base_path if present
#' @param nodata Value to use for nodata, or FALSE to skip this step
#' @param compress If TRUE, compress the file with ZSTD
#' @param stats If TRUE, create statistics files in sidecar `*.tif.aux.xml`
#' @param overviews If TRUE, create overviews in sidecar `*.tif.ovr`
#' @param in_place If TRUE, files are procssed in place and not copied. Only available if `compress = FALSE`.
#' @importFrom sf gdal_addo gdal_utils
#' @importFrom terra nlyr
#' @export


finish_orthos <- function(files, site,
                          base_path = 'U:/projects/uas_veg/sites/<site>/ortho',
                          source_path = 'orthos_raw',
                          result_path = 'orthos',
                          nodata = 65535,
                          stats = TRUE,
                          overviews = TRUE,
                          compress = TRUE,
                          in_place = FALSE) {


   write_nodata_aux <- function(f, nodata) {                                  # helper function for setting nodata value
      n <- nlyr(rast(f))
      bands <- sprintf('  <PAMRasterBand band="%d">\n    <NoDataValue>%s</NoDataValue>\n  </PAMRasterBand>',
                       seq_len(n), nodata)
      writeLines(c("<PAMDataset>", bands, "</PAMDataset>"), paste0(f, ".aux.xml"))
   }


   filenames <- function(files, site, base_path, subpath) {                   # helper to assemble file paths and names
      z <- file.path(base_path, subpath, files) |>
         gsub(pattern = '<site>', replacement = site, x = _, ignore.case = TRUE)
      z
   }



   if(in_place & compress)
      stop('You may not use in_place = TRUE with compress = TRUE, as compression requires creating a new file')


   message('Processing ', length(files), ' orthophotos...')

   f <- filenames(files, site, base_path, source_path)                        # source path and filenames
   if(in_place)
      g <- f
   else
      g <- filenames(files, site, base_path, result_path)                     # result path and filenames


   for(i in seq_along(f)) {                                                   # for each ortho,
      message('Processing ', f[i], '...')
      message('   CRS = ', crs(rast(f[i])))

      if(compress) {                                                          #    1. compression
         writeRaster(rast(f[i]), g[i], datatype = "INT2U", NAflag = 65535,
                     gdal = c("COMPRESS=ZSTD", "PREDICTOR=2", "TILED=YES", "BIGTIFF=IF_SAFER"))
      }

      if(nodata != FALSE)                                                     #    2. NoData
         write_nodata_aux(g[i], nodata)

      if(overviews)                                                           #    3. Overviews
         gdal_addo(g[i],
                   overviews = c(2, 4, 8, 16, 32),
                   method = "AVERAGE",
                   read_only = TRUE,
                   config_options = c(COMPRESS_OVERVIEW = "DEFLATE",
                                      BIGTIFF_OVERVIEW = "IF_SAFER"))

      if(stats)                                                               #    4. Statistics
         gdal_utils("info", g[i], options = c("-stats", "-hist"), quiet = TRUE)
   }

   message('Finished processing ', length(files), ' files')
}
