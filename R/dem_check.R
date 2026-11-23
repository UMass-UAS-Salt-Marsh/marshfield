#' Compare a DEM with RTK plot heights, writing a point GeoPackage and a map
#'
#' Samples a DEM at each plot center and computes `dz` = DEM - RTK height (m). Positive `dz`
#' is expected where the DEM includes vegetation; spatially coherent patches of large
#' positive or negative `dz` suggest vertical warping in the DEM.
#'
#' Writes
#' - `results/dz_<dem>.gpkg`, a point layer in the plots' CRS (EPSG:6491) with `plot_id`,
#'   `date`, `rtk_id`, `section`, `subclass`, `subclass_name`, `rtk_height` (in the DEM's
#'   vertical datum), `dem_height`, and `dz`.
#' - `results/dz_<dem>.png`, a map of `dz` over a faded orthophoto, with section labels.
#'
#' The only field data needed are `results/plots.gpkg` and, for ellipsoidal DEMs,
#' `results/everything.gpkg`, so DEMs can be checked before the rest of the field processing.
#' `pars/orthos.txt` (see `view_plots`) is optional: if it's there, DEMs and basemaps can be
#' given by name, and a DEM given by file name picks up its `vdatum`. Plot centers are put into
#' the DEM's CRS as in `view_plots` (including the TEMPORARY datum shift).
#'
#' The DEM's vertical datum is inferred from the heights rather than taken on trust: DEMs often
#' declare none, and NAVD88 and ellipsoidal heights differ by the geoid separation (~27 m here),
#' so a wrong guess would make every `dz` meaningless. Whichever of the plots' NAVD88 heights
#' (from `plots.gpkg`) or ellipsoidal heights (from `everything.gpkg`) the DEM matches is used
#' (see `gcp_vdatum`). If that disagrees with `ellipsoidal` or with `orthos.txt` you're told;
#' if the heights match neither, `ellipsoidal` or `orthos.txt` is used with a warning, or NAVD88
#' if neither says.
#'
#' @param dem The DEM: a file name (ending in `.tif`), relative to `dem_path` or a full path, or
#'   the name of a DEM in `orthos.txt` (e.g., 'Jul 20 low DEM')
#' @param site Optional site abbreviation (e.g., 'esx'), inserted in place of `<site>` in paths.
#'   If omitted, paths that are used must be complete.
#' @param ellipsoidal TRUE if you believe the DEM is in ellipsoidal heights, FALSE if NAVD88, or
#'   NULL (default) to go by the heights (and `orthos.txt`, if any). The heights win if they
#'   clearly disagree.
#' @param basemap RGB ortho to use as a basemap: a file name relative to `ortho_path` or a full
#'   path, or a name in `orthos.txt`. Default is the first RGB ortho for the same site in
#'   `orthos.txt`, or else the ortho matching the DEM's file name (`_dem` -> `_ortho`) in
#'   `ortho_path`, or else none.
#' @param limit Color scale runs from `-limit` to `limit` m; larger values are clamped
#' @param path Path to field data
#' @param dem_path Path to DEMs, for DEMs given by relative file name
#' @param ortho_path Path to orthoimages, for basemaps and files in `orthos.txt`
#' @param classes Path to classes table, used for subclass names
#' @returns sf point data frame of results, invisibly
#' @importFrom sf st_read st_coordinates st_drop_geometry st_crs st_as_sf st_write
#' @importFrom terra rast extract spatSample crop ext as.array xmin xmax ymin ymax crs
#' @importFrom grDevices png dev.off colorRamp rgb as.raster
#' @importFrom graphics par plot.new plot.window rasterImage points text rect segments
#' @importFrom stats median
#' @importFrom utils read.table
#' @export


dem_check <- function(dem, site = NULL, ellipsoidal = NULL, basemap = NULL, limit = 1.2,
                      path = 'U:/projects/uas_veg/sites/<site>/field',
                      dem_path = 'U:/projects/uas_veg/sites/<site>/dem',
                      ortho_path = 'U:/projects/uas_veg/sites/<site>/orthos',
                      classes = 'U:/projects/uas_veg/marshmap/pars/classes.txt') {


   fill <- function(x, what) {                                                       # fill in <site>, or insist it's not needed
      if(!is.null(site))
         x <- gsub('<site>', site, x, ignore.case = TRUE)
      if(grepl('<site>', x, ignore.case = TRUE))
         stop(what, ' (', x, ') contains <site>; supply site or a complete path', call. = FALSE)
      x
   }
   is_file <- function(f) grepl('\\.tiff?$', f, ignore.case = TRUE)
   resolve <- function(f, dir, what)                                                 # dir is only needed for relative names
      if(grepl('^([A-Za-z]:)?[/\\\\]', f)) f else file.path(fill(dir, what), f)

   path <- fill(path, 'path')
   classes <- fill(classes, 'classes')


   # orthos.txt is optional, as DEMs may be checked before field data are processed
   orthos <- NULL
   of <- file.path(path, 'pars/orthos.txt')
   if(file.exists(of)) {
      orthos <- read.table(of, sep = '\t', header = TRUE, quote = '', comment.char = '', fill = TRUE)
      if(is.null(orthos$vdatum))
         orthos$vdatum <- ''
      orthos$vdatum[is.na(orthos$vdatum)] <- ''
   }
   lookup <- function(name, type) {                                                  # row of a named image in orthos.txt
      if(is.null(orthos))
         stop('There\'s no ', of, ', so give ', name, ' as a file name', call. = FALSE)
      i <- match(name, orthos$name)
      if(is.na(i) || orthos$type[i] != type)
         stop(name, ' is not ', if(type == 'dem') 'a DEM' else 'an RGB ortho', ' in orthos.txt', call. = FALSE)
      i
   }


   # the DEM
   if(is_file(dem)) {
      dem_file <- resolve(dem, dem_path, 'dem_path')
      k <- if(is.null(orthos)) NA else match(basename(dem_file), basename(orthos$file))
      if(!is.na(k) && orthos$type[k] != 'dem')
         k <- NA
   }
   else {
      k <- lookup(dem, 'dem')
      dem_file <- resolve(orthos$file[k], ortho_path, 'ortho_path')
   }
   if(!file.exists(dem_file))
      stop('DEM ', dem_file, ' not found', call. = FALSE)
   dem_name <- if(is.na(k)) basename(dem_file) else orthos$name[k]


   # the basemap
   base_file <- NULL
   if(!is.null(basemap)) {
      base_file <- if(is_file(basemap)) resolve(basemap, ortho_path, 'ortho_path') else
         resolve(orthos$file[lookup(basemap, 'rgb')], ortho_path, 'ortho_path')
      if(!file.exists(base_file))
         stop('Basemap ', base_file, ' not found', call. = FALSE)
   }
   else {
      if(!is.na(k) && length(b <- which(orthos$site == orthos$site[k] & orthos$type == 'rgb')) > 0)
         base_file <- tryCatch(resolve(orthos$file[b[1]], ortho_path, 'ortho_path'), error = function(e) NULL)
      else {
         f <- sub('_dem(\\.tiff?)$', '_ortho\\1', basename(dem_file), ignore.case = TRUE)
         base_file <- tryCatch(resolve(f, ortho_path, 'ortho_path'), error = function(e) NULL)
      }
      if(!is.null(base_file) && !file.exists(base_file))
         base_file <- NULL
      if(is.null(base_file))
         message('No basemap found; the map will have no ortho behind it.')
   }


   # plots, with RTK heights in both vertical datums
   p <- st_read(file.path(path, 'results/plots.gpkg'), quiet = TRUE)
   plot_crs <- st_crs(p)
   xyz <- st_coordinates(p)
   p <- st_drop_geometry(p)
   eh <- NULL
   ef <- file.path(path, 'results/everything.gpkg')
   if(file.exists(ef)) {
      e <- st_drop_geometry(st_read(ef, quiet = TRUE))
      if(!is.null(e$ellipsoidal_height))
         eh <- e$ellipsoidal_height[match(p$plot_id, e$plot_id)]
   }
   cl <- read.table(classes, sep = '\t', header = TRUE, quote = '', comment.char = '')
   p$subclass_name <- cl$subclass_name[match(p$subclass, cl$subclass)]


   # sample DEM
   r <- rast(dem_file)
   xy <- project_xy(xyz[, 1], xyz[, 2], plot_crs, r)                               # plot centers in DEM's CRS
   p$dem_height <- extract(r, xy)[, 1]


   # vertical datum: inferred from the heights, as what we're told can be wrong or missing (see gcp_vdatum)
   stated <- NULL
   if(!is.null(ellipsoidal)) {
      stated <- if(ellipsoidal) 'ellipsoidal' else 'navd88'
      source <- paste0('ellipsoidal = ', ellipsoidal)
   }
   else if(!is.na(k) && orthos$vdatum[k] != '') {
      stated <- orthos$vdatum[k]
      source <- 'orthos.txt'
   }
   v <- gcp_vdatum(p$dem_height, xyz[, 3], eh, st_crs(crs(r)))
   if(v$datum == 'unknown') {
      datum <- if(is.null(stated)) 'navd88' else stated
      warning('DEM heights match neither NAVD88 nor ellipsoidal RTK heights; using ', datum,
              if(is.null(stated)) ' by default' else paste0(' from ', source),
              ', but dz is probably meaningless. ', sub(' *Z error.*$', '', v$msg), call. = FALSE)
      if(datum == 'ellipsoidal' && is.null(eh))
         stop('Ellipsoidal heights need ', ef, ', which is missing or has no ellipsoidal_height', call. = FALSE)
   }
   else {
      datum <- v$datum
      if(!is.null(stated) && datum != stated)
         message(source, ' says ', dem_name, ' is ', stated, ', but its heights are ', datum, '; using ', datum, '.',
                 if(source == 'orthos.txt') ' Fix orthos.txt so view_plots gets it right too.')
   }
   p$rtk_height <- if(datum == 'ellipsoidal') eh else xyz[, 3]
   p$dz <- p$dem_height - p$rtk_height

   out <- st_as_sf(cbind(p[, c('plot_id', 'date', 'rtk_id', 'section', 'subclass', 'subclass_name',
                               'rtk_height', 'dem_height', 'dz')], x = xyz[, 1], y = xyz[, 2]),
                   coords = c('x', 'y'), crs = 'EPSG:6491')
   base <- file.path(path, 'results', paste0('dz_', sub('\\.tiff?$', '', basename(dem_file), ignore.case = TRUE)))
   st_write(out, paste0(base, '.gpkg'), append = FALSE, quiet = TRUE)


   # map
   pad <- 40
   xl <- range(xyz[, 1]) + c(-pad, pad)
   yl <- range(xyz[, 2]) + c(-pad, pad)
   w <- 2400
   h <- round(w * diff(yl) / diff(xl))
   png(paste0(base, '.png'), width = w + 500, height = h + 260, res = 200, type = 'cairo')
   on.exit(dev.off())
   par(mar = c(0, 0, 0, 0), bg = 'white')
   plot.new()
   plot.window(xlim = c(xl[1], xl[2] + diff(xl) * 500 / w), ylim = c(yl[1], yl[2] + diff(yl) * 260 / h),
               xaxs = 'i', yaxs = 'i', asp = 1)

   if(!is.null(base_file)) {                                                         # faded grayscale ortho
      o <- vp_load_ortho(base_file, 'rgb')
      m <- spatSample(crop(o$rast, ext(xl[1], xl[2], yl[1], yl[2])), 1.5e6, method = 'regular', as.raster = TRUE)
      a <- as.array(m)
      a[a == 65535] <- NA
      g <- pmin(pmax((apply(a, c(1, 2), mean) - o$lo[1]) / (o$hi[1] - o$lo[1]), 0), 1)
      g <- 0.55 + 0.45 * g                                                             # fade toward white
      na <- is.na(g)
      g[na] <- 1
      col <- rgb(g, g, g)
      rasterImage(as.raster(matrix(col, nrow(a))), xmin(m), ymin(m), xmax(m), ymax(m), interpolate = TRUE)
   }

   ramp <- colorRamp(c('#104281', '#3987e5', '#9ec5f4', '#f0efec', '#f4b2ae', '#e34948', '#8f1d1f'), space = 'Lab')
   pal <- function(z) {
      v <- ramp((pmin(pmax(z, -limit), limit) + limit) / (2 * limit))
      rgb(v[, 1], v[, 2], v[, 3], maxColorValue = 255)
   }
   ok <- !is.na(p$dz)
   points(xyz[!ok, 1], xyz[!ok, 2], pch = 4, col = '#6b6a66', cex = 0.7)
   o <- order(abs(p$dz[ok]))                                                         # draw largest |dz| on top
   points(xyz[ok, 1][o], xyz[ok, 2][o], pch = 21, bg = pal(p$dz[ok][o]), col = '#3b3a37', lwd = 0.6, cex = 1.1)

   for(s in unique(p$section)) {                                                     # section labels
      j <- p$section == s
      text(median(xyz[j, 1]), max(xyz[j, 2]) + 6, s, cex = 0.6, col = '#3b3a37', font = 2)
   }

   ux <- diff(xl) / w                                                                # map units per pixel
   text(xl[1] + 20 * ux, yl[2] + 190 * ux, paste0('DEM \u2212 RTK height: ', dem_name,
                                                 ' (', if(datum == 'ellipsoidal') 'ellipsoidal' else 'NAVD88', ')'),
        adj = c(0, 0.5), cex = 1.2, font = 2, col = '#1f1f1d')
   text(xl[1] + 20 * ux, yl[2] + 110 * ux,
        sprintf('%d plots;  median %+.2f m;  blue = DEM below RTK, red = DEM above RTK;  x = no DEM data',
                sum(ok), median(p$dz, na.rm = TRUE)), adj = c(0, 0.5), cex = 0.8, col = '#4a4945')

   lx <- xl[2] + 120 * ux                                                            # legend color bar
   ly <- seq(yl[1] + diff(yl) * 0.25, yl[1] + diff(yl) * 0.75, length.out = 101)
   zz <- seq(-limit, limit, length.out = 100)
   rect(lx, ly[-101], lx + 60 * ux, ly[-1], col = pal(zz), border = NA)
   ticks <- pretty(c(-limit, limit))
   for(t in ticks[abs(ticks) <= limit + 1e-9]) {
      yy <- ly[1] + (t + limit) / (2 * limit) * diff(range(ly))
      segments(lx + 60 * ux, yy, lx + 75 * ux, yy, col = '#4a4945')
      text(lx + 85 * ux, yy, sprintf('%+.1f', t), adj = c(0, 0.5), cex = 0.7, col = '#4a4945')
   }
   text(lx, ly[101] + 40 * ux, 'dz (m)', adj = c(0, 0.5), cex = 0.8, font = 2, col = '#1f1f1d')

   message('Wrote ', base, '.gpkg and .png')
   invisible(out)
}
