#' Compare a DEM with RTK plot heights, writing a point GeoPackage and a map
#'
#' Samples a DEM at each plot center and computes `dz` = DEM - RTK height (m). Positive `dz`
#' is expected where the DEM includes vegetation; spatially coherent patches of large
#' positive or negative `dz` suggest vertical warping in the DEM. Writes
#'
#' - `prelim/dz_<dem>.gpkg`, a point layer in the plots' CRS (EPSG:6491) with `plot_id`,
#'   `date`, `rtk_id`, `section`, `subclass`, `subclass_name`, `rtk_height` (in the DEM's
#'   vertical datum), `dem_height`, and `dz`.
#' - `prelim/dz_<dem>.png`, a map of `dz` over a faded orthophoto, with section labels.
#'
#' The DEM and its vertical datum come from `pars/orthos.txt` (see `view_plots`). Plot centers
#' are put into the DEM's CRS as in `view_plots` (including the TEMPORARY datum shift).
#'
#' @param dem Name of the DEM in `orthos.txt` (e.g., 'Jul 20 low DEM')
#' @param basemap Name of the RGB ortho in `orthos.txt` to use as a basemap; default is the first
#'   RGB ortho for the same site
#' @param limit Color scale runs from `-limit` to `limit` m; larger values are clamped
#' @param path Path to field data
#' @param ortho_path Path to orthoimages and DEMs
#' @param classes Path to classes table, used for subclass names
#' @returns sf point data frame of results, invisibly
#' @importFrom sf st_read st_coordinates st_drop_geometry st_crs st_as_sf st_write
#' @importFrom terra rast extract spatSample crop ext as.array xmin xmax ymin ymax
#' @importFrom grDevices png dev.off colorRamp rgb as.raster
#' @importFrom graphics par plot.new plot.window rasterImage points text rect segments
#' @importFrom stats median
#' @importFrom utils read.table
#' @export


dem_check <- function(dem, basemap = NULL, limit = 1.2,
                      path = 'C:/Work/saltmarsh/data/uas2026/field',
                      ortho_path = 'C:/Work/saltmarsh/data/uas2026/orthos',
                      classes = 'C:/Work/saltmarsh/pars/classes.txt') {


   orthos <- read.table(file.path(path, 'pars/orthos.txt'), sep = '\t', header = TRUE, quote = '',
                        comment.char = '', fill = TRUE)
   if(is.null(orthos$vdatum))
      orthos$vdatum <- ''
   absolute <- grepl('^([A-Za-z]:)?[/\\\\]', orthos$file)
   orthos$file[!absolute] <- file.path(ortho_path, orthos$file[!absolute])

   k <- match(dem, orthos$name)
   if(is.na(k) || orthos$type[k] != 'dem')
      stop(dem, ' is not a DEM in orthos.txt')
   if(is.null(basemap))
      basemap <- orthos$name[orthos$site == orthos$site[k] & orthos$type == 'rgb'][1]
   b <- match(basemap, orthos$name)


   # plots and RTK heights in the DEM's vertical datum
   p <- st_read(file.path(path, 'prelim/plots.gpkg'), quiet = TRUE)
   plot_crs <- st_crs(p)
   xyz <- st_coordinates(p)
   p <- st_drop_geometry(p)
   if(orthos$vdatum[k] == 'ellipsoidal') {
      e <- st_drop_geometry(st_read(file.path(path, 'prelim/everything.gpkg'), quiet = TRUE))
      p$rtk_height <- e$ellipsoidal_height[match(p$plot_id, e$plot_id)]
   }
   else
      p$rtk_height <- xyz[, 3]
   cl <- read.table(classes, sep = '\t', header = TRUE, quote = '', comment.char = '')
   p$subclass_name <- cl$subclass_name[match(p$subclass, cl$subclass)]


   # sample DEM
   r <- rast(orthos$file[k])
   xy <- vp_temp_reproject(xyz[, 1], xyz[, 2], plot_crs, r)                         # TEMPORARY: plot centers in DEM's CRS
   p$dem_height <- extract(r, xy)[, 1]
   p$dz <- p$dem_height - p$rtk_height

   out <- st_as_sf(cbind(p[, c('plot_id', 'date', 'rtk_id', 'section', 'subclass', 'subclass_name',
                               'rtk_height', 'dem_height', 'dz')], x = xyz[, 1], y = xyz[, 2]),
                   coords = c('x', 'y'), crs = 'EPSG:6491')
   base <- file.path(path, 'prelim', paste0('dz_', sub('\\.tif$', '', basename(orthos$file[k]))))
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

   if(!is.na(b)) {                                                                   # faded grayscale ortho
      o <- vp_load_ortho(orthos$file[b], 'rgb')
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
   text(xl[1] + 20 * ux, yl[2] + 190 * ux, paste0('DEM \u2212 RTK height: ', orthos$name[k]),
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
