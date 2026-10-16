#' Finish orthophotos after a Metashape run: compressing/set nodata/statistics/overviews
#'
#' Performs standard post-processing for orthophotos created by Agisoft Metashape. Files are
#' copied from a source directory (by default, `orthos_raw/`) to a result directory (`orthos/`)
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
#' Each file keeps its own data type. That matters: orthophotos are 2-byte unsigned integers
#' (INT2U), but DEMs and DTMs are floating point, and writing one as INT2U truncates every
#' elevation to a whole number and clamps the negatives to zero, which destroys the file.
#'
#' `nodata` defaults to 65535, which is right for MicaSense orthophotos. A sentinel value like
#' that only makes sense for integer data -- in a floating-point file 65535 is simply a real
#' elevation, and nodata is NaN -- so NA is used for floating-point files whatever `nodata`
#' says, and a note is printed. For integer files, a `nodata` the type can't hold is refused
#' rather than silently written as something else. `nodata = FALSE` uses NA throughout.
#'
#' Files are copied from `<base_path>/<type><source_postfix>/<file>` to
#' `<base_path>/<type><result_postfix>/<file>`. (The result path is created if it doesn't
#' exist). The default is set up for the `uas_veg` project; for other projects, you'll need to
#' specify `base_path`.
#'
#' `type` is what makes this usable for more than orthophotos: with the default postfixes,
#' `type = 'orthos'` goes from `orthos_raw/` to `orthos/`, and `type = 'dem'` from `dem_raw/`
#' to `dem/`. Statistics matter beyond ArcGIS and QGIS, as `gcp` and `view_plots` read their
#' display stretch from them rather than sampling the raster, which is the difference between
#' an image opening instantly and taking the better part of a minute over the network.
#'
#' Before processing, all source files are checked, and the function stops if any are missing.
#' Then any existing files in the result directory that share a base name with the files being
#' processed (e.g., `<file>.tif`, `<file>.tif.ovr`, `<file>.tif.aux.xml`) are deleted, so stale
#' sidecars from a previous run don't linger. If `in_place = TRUE`, only the sidecars are deleted.
#' If any of these files are locked (for instance, open in ArcGIS), the function stops before
#' deleting or processing anything.
#'
#' @param files Vector of geoTIFF file names
#' @param site Optional site abbreviation, inserted in place of `<site>` in paths and filenames
#' @param type Type of files, such as `orthos`, `dem`, or `dtm`. Used as the source and result
#' directory names, with the postfixes below, and inserted in place of `<type>` in paths
#' @param base_path Optional base path, shared between source and result paths (use
#' `base_path = ''` to skip a base path)
#' @param source_postfix Appended to `type` to give the source directory (so `orthos` and
#' `_raw` give `orthos_raw`)
#' @param result_postfix Appended to `type` to give the result directory (empty, by default,
#' so results land in `<type>` itself)
#' @param nodata Value to use for nodata, or FALSE to use NA. Ignored for floating-point
#' files, which always get NA
#' @param compress If TRUE, compress the file with ZSTD
#' @param stats If TRUE, create statistics files in sidecar `*.tif.aux.xml`
#' @param overviews If TRUE, create overviews in sidecar `*.tif.ovr`
#' @param in_place If TRUE, files are procssed in place and not copied. Only available if `compress = FALSE`.
#' @importFrom sf gdal_addo gdal_utils
#' @importFrom terra nlyr rast crs writeRaster
#' @export


finish_orthos <- function(files, site = '<site>', type = 'orthos',
                          base_path = 'U:/projects/uas_veg/sites/<site>',
                          source_postfix = '_raw',
                          result_postfix = '',
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
         gsub(pattern = '<site>', replacement = site, x = _, ignore.case = TRUE) |>
         gsub(pattern = '<type>', replacement = type, x = _, ignore.case = TRUE)
      z
   }



   same_base <- function(f) {                                                 # helper: existing files sharing base name with f
      base <- sub('\\.[^.]*$', '', basename(f))
      x <- list.files(dirname(f))
      file.path(dirname(f), x[x == base | startsWith(x, paste0(base, '.'))])
   }


   is_locked <- function(x) {                                                 # helper: TRUE if file is locked (e.g., by ArcGIS)
      tmp <- paste0(x, '.locktest')
      if(!suppressWarnings(file.rename(x, tmp)))
         return(TRUE)
      file.rename(tmp, x)
      FALSE
   }



   if(in_place && compress)
      stop('You may not use in_place = TRUE with compress = TRUE, as compression requires creating a new file')


   f <- filenames(files, site, base_path, paste0(type, source_postfix))       # source path and filenames
   if(in_place)
      g <- f
   else
      g <- filenames(files, site, base_path, paste0(type, result_postfix))    # result path and filenames


   missing <- f[!file.exists(f)]                                              # check for missing source files
   if(length(missing) > 0)
      stop('Missing source file(s):\n   ', paste(missing, collapse = '\n   '))

   if(!in_place && any(normalizePath(dirname(f)) == normalizePath(dirname(g), mustWork = FALSE)))
      stop('Source and result paths are the same; use in_place = TRUE to process files in place')


   datatype <- vapply(f, function(x) terra::datatype(rast(x))[1], character(1))   # keep each file's own type

   bad <- unique(datatype[!datatype %in% c('INT1U', 'INT2U', 'INT2S', 'INT4U', 'INT4S',
                                           'FLT4S', 'FLT8S')])
   if(length(bad) > 0)
      stop('Unsupported data type(s): ', paste(bad, collapse = ', '))

   na_flag <- rep(if(isFALSE(nodata)) NA_real_ else as.numeric(nodata), length(f))

   float <- startsWith(datatype, 'FLT')                                       # floating-point files use NaN for nodata,
   if(any(float) && !isFALSE(nodata)) {                                       # so a sentinel like 65535 is both wrong and
      na_flag[float] <- NA_real_                                              # a real elevation as far as anything knows
      message('Using NA for nodata on ', sum(float), ' floating-point file(s); ',
              'a sentinel value is only wanted for integer data')
   }

   if(!isFALSE(nodata) && any(!float)) {                                      # a value the type can't hold would be
      lim <- c(INT1U = 255, INT2U = 65535, INT2S = 32767,                     # written as something else entirely
               INT4U = 4294967295, INT4S = 2147483647)[datatype]
      over <- !is.na(lim) & nodata > lim
      if(any(over))
         stop('nodata = ', nodata, ' does not fit the data type of:\n   ',
              paste(unique(paste0(basename(f[over]), ' (', datatype[over], ')')), collapse = '\n   '))
   }


   old <- unique(unlist(lapply(g, same_base)))                                # existing results (or sidecars if in place) to delete
   if(in_place)
      old <- setdiff(old, f)
   locked <- old[vapply(old, is_locked, logical(1))]
   if(length(locked) > 0)
      stop('These files are locked (are they open in ArcGIS?):\n   ', paste(locked, collapse = '\n   '))
   if(length(old) > 0) {
      message('Deleting ', length(old), ' existing result file(s)')
      file.remove(old)
   }

   dir.create(unique(dirname(g)), recursive = TRUE, showWarnings = FALSE)


   message('Processing ', length(files), ' files from ', type, '...')

   for(i in seq_along(f)) {                                                   # for each file,
      message('Processing ', f[i], '...')
      r <- rast(f[i])
      cr <- crs(r, describe = TRUE)
      message('   CRS = ', paste0(cr$authority, ':', cr$code))

      dt <- datatype[i]
      if(compress)                                                            #    1. compression
         writeRaster(r, g[i], datatype = dt,                                  #       keep the source type: writing a
                     NAflag = na_flag[i],                                     #       float DEM as INT2U destroys it
                     gdal = c("COMPRESS=ZSTD", if(startsWith(dt, 'INT')) "PREDICTOR=2" else "PREDICTOR=3",
                              "TILED=YES", "BIGTIFF=IF_SAFER"))
      else if(!in_place)
         file.copy(f[i], g[i])

      if(!is.na(na_flag[i]))                                                  #    2. NoData (float files keep NaN,
         write_nodata_aux(g[i], na_flag[i])                                   #       which needs no declaration)

      if(stats)                                                               #    3. Statistics
         gdal_utils("info", g[i], options = c("-stats", "-hist"), quiet = TRUE)

      if(overviews)                                                           #    4. Overviews
         gdal_addo(g[i],
                   overviews = c(2, 4, 8, 16, 32),
                   method = "AVERAGE",
                   read_only = TRUE,
                   config_options = c(COMPRESS_OVERVIEW = "DEFLATE",
                                      BIGTIFF_OVERVIEW = "IF_SAFER"))
   }

   message('Finished processing ', length(files), ' files')
}
