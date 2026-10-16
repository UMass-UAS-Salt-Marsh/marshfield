#' List projects for gcp
#'
#' @param basepath Base path holding projects.txt and project folders
#' @returns Character vector of project names (empty if there's no projects.txt)
#' @importFrom utils read.table
#' @keywords internal
#' @noRd


gcp_projects <- function(basepath) {

   f <- file.path(basepath, 'projects.txt')
   if(!file.exists(f))
      return(character(0))
   x <- read.table(f, sep = '\t', header = TRUE, quote = '', comment.char = '')
   p <- trimws(x[[1]])
   p[p != '' & dir.exists(file.path(basepath, p))]                     # skip projects that aren't there yet
}



#' List sites for a project
#'
#' @param basepath Base path
#' @param project Project name
#' @returns Character vector of site names (empty if there's no sites.txt)
#' @importFrom utils read.table
#' @keywords internal
#' @noRd


gcp_sites <- function(basepath, project) {

   f <- file.path(basepath, project, 'sites', 'sites.txt')
   if(!file.exists(f))
      return(character(0))
   x <- read.table(f, sep = '\t', header = TRUE, quote = '', comment.char = '')
   s <- trimws(x[[1]])
   s[s != '' & dir.exists(file.path(basepath, project, 'sites', s))]
}



#' Build a site path from the template
#'
#' @param template Site path template containing <project> and <site>
#' @param basepath Base path, substituted for <basepath>
#' @param project Project name
#' @param site Site name
#' @returns Path to the site
#' @keywords internal
#' @noRd


gcp_site_path <- function(template, basepath, project, site) {

   x <- gsub('<basepath>', basepath, template, fixed = TRUE)
   x <- gsub('<project>', project, x, fixed = TRUE)
   gsub('<site>', site, x, fixed = TRUE)
}



#' List images (orthos and DEMs) for a site
#'
#' Orthos come first, then DEMs, each sorted by name. micap and panchro images are listed
#' separately, as they're processed independently and may disagree.
#'
#' @param site_path Path to the site
#' @returns Data frame of file (full path), name (basename), and type ('ortho' or 'dem')
#' @keywords internal
#' @noRd


gcp_images <- function(site_path) {

   one <- function(dir, type) {
      f <- list.files(file.path(site_path, dir), pattern = '\\.tif$', full.names = TRUE)
      if(length(f) == 0)
         return(NULL)
      data.frame(file = f, name = basename(f), type = type)
   }

   x <- rbind(one('orthos', 'ortho'), one('dem', 'dem'))
   if(is.null(x))
      return(data.frame(file = character(0), name = character(0), type = character(0)))

   date <- sub('^.*?(\\d{4}_\\d{2}_\\d{2}).*$', '\\1', x$name)                 # names are <site>_<date>_<stage>_<camera>_<type>
   date[date == x$name] <- ''                                                  # no date in the name: sort it first
   x[order(x$type != 'ortho', date, x$name), ]                                 # orthos first, then by date
}



#' Find the DEM paired with an ortho, or vice versa
#'
#' DEMs are named like their orthos, with `_dem.tif` for `_ortho.tif`. Some DEMs have no
#' matching ortho.
#'
#' @param name Image name (basename)
#' @param images Data frame from `gcp_images`
#' @returns Name of the paired image, or NA
#' @keywords internal
#' @noRd


gcp_pair <- function(name, images) {

   p <- if(grepl('_dem\\.tif$', name))
      sub('_dem\\.tif$', '_ortho.tif', name)
   else
      sub('_ortho\\.tif$', '_dem.tif', name)

   if(p %in% images$name) p else NA_character_
}



#' Find and read the GCP vector file for a site
#'
#' Reads a GeoPackage if there is one, otherwise a shapefile, from `gcp_placement/vector`.
#' Warns if any file in `gcp_placement/raw` is newer, as the vector file may be out of date.
#' This never writes: converting raw RTK files is a separate job.
#'
#' @param site_path Path to the site
#' @returns List of gcp (sf points, or NULL), file (what was read), and msg (character vector
#'   of problems to display)
#' @importFrom sf st_read st_zm
#' @keywords internal
#' @noRd


gcp_read_points <- function(site_path) {

   vdir <- file.path(site_path, 'gcp_placement', 'vector')
   msg <- character(0)

   f <- list.files(vdir, pattern = '\\.gpkg$', full.names = TRUE)
   if(length(f) == 0)
      f <- list.files(vdir, pattern = '\\.shp$', full.names = TRUE)
   if(length(f) == 0)
      return(list(gcp = NULL, file = NA, msg = paste0('No GeoPackage or shapefile in ', vdir)))

   if(length(f) > 1)
      msg <- c(msg, paste0(length(f), ' vector files; using ', basename(f[1])))
   f <- f[1]

   raw <- list.files(file.path(site_path, 'gcp_placement', 'raw'), full.names = TRUE)
   if(length(raw) > 0) {
      newer <- raw[file.mtime(raw) > file.mtime(f)]
      if(length(newer) > 0)
         msg <- c(msg, paste0(length(newer), ' raw file(s) newer than ', basename(f),
                              ' (', paste(basename(newer), collapse = ', '),
                              '). The GCPs may be out of date.'))
   }

   x <- tryCatch(st_read(f, quiet = TRUE), error = function(e) NULL)
   if(is.null(x))
      return(list(gcp = NULL, file = f, msg = c(msg, paste0('Could not read ', basename(f)))))

   list(gcp = x, file = f, msg = msg)
}



#' Find the GCP name column
#'
#' Vector files come from several sources, so the name column varies.
#'
#' @param x sf data frame of GCPs
#' @returns Character vector of names, one per row
#' @importFrom sf st_drop_geometry
#' @keywords internal
#' @noRd


gcp_names <- function(x) {

   d <- st_drop_geometry(x)
   for(f in c('gcp_name', 'rtk_id', 'Name', 'name', 'id', 'ID'))
      if(!is.null(d[[f]]))
         return(as.character(d[[f]]))
   paste0('gcp_', seq_len(nrow(x)))                                    # no name column: number them
}



#' Check that a GCP and image CRS are compatible
#'
#' The UAS group has been inconsistent about coordinate reference systems, and a datum
#' mismatch (NAD83 vs. NAD83(2011), GEOID12B vs. GEOID18) shows up as about a meter of
#' apparent GCP error, which is the same size as what we're trying to measure. So this
#' reports anything suspicious rather than quietly correcting it.
#'
#' @param gcp_crs CRS of the GCPs (an sf crs)
#' @param r SpatRaster
#' @param type 'ortho' or 'dem'
#' @returns Character vector of warnings (empty if all is well)
#' @importFrom sf st_crs
#' @importFrom terra crs
#' @keywords internal
#' @noRd


gcp_check_crs <- function(gcp_crs, r, type) {

   msg <- character(0)
   icrs <- st_crs(crs(r))

   if(is.na(icrs))
      return('Image has no CRS; positions can\'t be trusted.')

   horiz <- function(x) sub(' *\\+.*$', '', x$Name)                 # drop the vertical part of a compound CRS
   if(horiz(gcp_crs) != horiz(icrs))
      msg <- c(msg, paste0('GCPs are ', horiz(gcp_crs), ' but image is ', horiz(icrs),
                           '. Points are projected to match; if the datums differ, error includes a datum shift.'))

   for(nm in list(c('GCPs', horiz(gcp_crs)), c('Image', horiz(icrs))))
      if(!grepl('2011', nm[2]))
         msg <- c(msg, paste0(nm[1], ' CRS (', nm[2], ') isn\'t NAD83(2011); error may include a datum shift of up to ~1 m.'))

   if(type == 'dem' && !grepl('NAVD88|5703', gcp_crs$wkt))           # the DEM's own vertical datum is gcp_vdatum's job
      msg <- c(msg, 'GCPs declare no vertical datum; Z error can\'t be trusted.')

   msg
}



#' Decide which vertical datum a DEM uses
#'
#' DEMs from the photogrammetry pipeline often declare no vertical CRS, and some are in
#' ellipsoidal heights rather than NAVD88. The difference is the geoid separation, about 27 m
#' in Massachusetts, so getting it wrong makes Z error meaningless rather than merely wrong.
#'
#' This compares the DEM against both heights at the GCPs and picks whichever is plausible.
#' That's an inference, not a reading of metadata, so the result is always displayed alongside
#' the Z error rather than applied silently. If the DEM declares NAVD88 outright, that's
#' believed without argument.
#'
#' A DEM built on GEOID12B rather than GEOID18 is a fourth possibility, and this can't detect
#' it: the two geoids differ by only 1.2 cm at Essex (measured across the ESX GCPs), which
#' is well inside any sensible tolerance and indistinguishable from real vertical error. Such a
#' DEM is reported as `navd88` and carries a constant bias of about that much -- negligible
#' against the meter-scale Z errors we're seeing, but worth knowing about if vertical accuracy
#' ever improves to the point where a centimeter matters. The ellipsoidal case, by contrast, is
#' 27 m and has to be caught.
#'
#' @param dem_z DEM heights at the GCPs
#' @param navd88 GCP NAVD88 heights
#' @param ellipsoidal GCP ellipsoidal heights, or NULL if the vector file has none
#' @param icrs CRS of the DEM (an sf crs)
#' @param tol Largest plausible median difference (m)
#' @returns List of datum ('navd88', 'ellipsoidal', or 'unknown'), z (the heights to compare
#'   against), and msg (what was decided and why)
#' @importFrom stats median
#' @keywords internal
#' @noRd


gcp_vdatum <- function(dem_z, navd88, ellipsoidal, icrs, tol = 3) {

   if(grepl('NAVD88|5703', icrs$wkt))                                     # declared: believe it
      return(list(datum = 'navd88', z = navd88, msg = character(0)))

   d <- function(h) if(is.null(h) || all(is.na(h))) NA else
      abs(median(dem_z - h, na.rm = TRUE))

   dn <- d(navd88)
   de <- d(ellipsoidal)

   if(!is.na(dn) && dn <= tol && (is.na(de) || dn <= de))
      return(list(datum = 'navd88', z = navd88,
                  msg = 'DEM declares no vertical datum; its heights match NAVD88, so Z error is against NAVD88.'))

   if(!is.na(de) && de <= tol)
      return(list(datum = 'ellipsoidal', z = ellipsoidal,
                  msg = paste0('DEM declares no vertical datum, and its heights are ellipsoidal, not NAVD88 ',
                               '(off by ', round(median(navd88 - ellipsoidal, na.rm = TRUE), 1),
                               ' m, the geoid separation). Z error is against ellipsoidal heights.')))

   list(datum = 'unknown', z = navd88,                                    # neither fits: say so and don't pretend
        msg = paste0('DEM heights match neither NAVD88 (off by ', round(dn, 1), ' m) nor ellipsoidal ',
                     if(is.na(de)) '(not available)' else paste0('(off by ', round(de, 1), ' m)'),
                     '. Z error is against NAVD88 and is probably meaningless.'))
}
