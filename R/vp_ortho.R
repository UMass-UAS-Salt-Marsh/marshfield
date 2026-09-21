#' Load an ortho or DEM for view_plots
#'
#' For RGB orthos, picks the Red, Green, and Blue bands (or the first 3 bands if they aren't
#' named) and computes a fixed, linked 1-99% stretch from a regular sample of the whole image,
#' so colors are consistent from plot to plot. 65535 is treated as nodata (as in MicaSense
#' orthos).
#'
#' @param file Path to geoTIFF
#' @param type Either 'rgb' or 'dem'
#' @returns List of rast, type, and for rgb, lo and hi (stretch limits for each band)
#' @importFrom terra rast spatSample
#' @importFrom stats quantile complete.cases
#' @keywords internal
#' @noRd


vp_load_ortho <- function(file, type) {

   r <- rast(file)
   if(type == 'dem')
      return(list(rast = r, type = type))

   bands <- match(c('Red', 'Green', 'Blue'), names(r))
   if(any(is.na(bands)))
      bands <- 1:3
   r <- r[[bands]]

   s <- spatSample(r, 20000, method = 'regular', as.df = TRUE)                  # sample for stretch (uses overviews, so it's fast)
   s[s == 65535] <- NA
   s <- s[complete.cases(s), ]

   list(rast = r, type = type,                                                   # linked stretch (same for all bands) keeps colors natural
        lo = rep(min(sapply(s, quantile, 0.01)), 3),
        hi = rep(max(sapply(s, quantile, 0.99)), 3))
}



#' Render a clip of an ortho or DEM centered on a plot to a PNG
#'
#' Draws the clip north-up with a white circle at the plot perimeter, an orange dot at plot
#' center, and a scale bar. DEMs are displayed as shaded relief with an elevation color ramp
#' stretched to the clip.
#'
#' @param o Ortho object from `vp_load_ortho`
#' @param x Plot easting
#' @param y Plot northing
#' @param width Width (and height) of clip in m
#' @param file Result PNG file
#' @param px Size of PNG in pixels
#' @param radius Plot radius (m)
#' @param offset Recorded offset c(dx, dy) of actual plot center, or NULL; drawn as a cyan
#'   cross and dashed circle at the actual location, with a line from the plot center
#' @importFrom terra ext crop xmin xmax ymin ymax res
#' @importFrom grDevices png dev.off
#' @importFrom graphics par plot.new plot.window rasterImage lines points segments text
#' @keywords internal
#' @noRd


vp_render_ortho <- function(o, x, y, width, file, px = 800, radius = 0.7, offset = NULL) {

   png(file, width = px, height = px)
   on.exit(dev.off())
   par(mar = rep(0, 4), bg = 'gray30')
   plot.new()
   xlim <- x + c(-1, 1) * width / 2
   ylim <- y + c(-1, 1) * width / 2
   plot.window(xlim = xlim, ylim = ylim, xaxs = 'i', yaxs = 'i', asp = 1)

   r <- o$rast
   if(xlim[2] < xmin(r) | xlim[1] > xmax(r) | ylim[2] < ymin(r) | ylim[1] > ymax(r)) {
      text(x, y, 'Plot is outside this image', col = 'white', cex = 2)
      return(invisible())
   }

   buf <- if(o$type == 'dem') res(r)[1] else 0                                   # extra pixel so hillshade reaches the edges
   cr <- crop(r, ext(xlim[1] - buf, xlim[2] + buf, ylim[1] - buf, ylim[2] + buf), snap = 'out')

   if(o$type == 'dem') {
      z <- vp_dem_colors(cr)
      ras <- z$ras
   }
   else
      ras <- vp_rgb_colors(cr, o)
   rasterImage(ras, xmin(cr), ymin(cr), xmax(cr), ymax(cr), interpolate = FALSE)

   th <- seq(0, 2 * pi, length.out = 361)                                        # plot perimeter and center
   lines(x + radius * cos(th), y + radius * sin(th), col = 'white', lwd = 5)
   points(x, y, pch = 21, bg = '#FF7F00', col = 'black', cex = 1.8)

   if(!is.null(offset)) {                                                        # recorded offset: actual plot location
      ax <- x + offset[1]
      ay <- y + offset[2]
      lines(ax + radius * cos(th), ay + radius * sin(th), col = '#00E5FF', lwd = 2, lty = 2)
      segments(x, y, ax, ay, col = '#00E5FF', lwd = 2)
      points(ax, ay, pch = 3, col = '#00E5FF', cex = 2.5, lwd = 3)
   }

   sb <- if(width <= 2) 0.5 else if(width <= 5) 1 else 2                         # scale bar
   sx <- xlim[1] + width * 0.04
   sy <- ylim[1] + width * 0.04
   segments(sx, sy, sx + sb, sy, col = 'black', lwd = 7, lend = 1)
   segments(sx, sy, sx + sb, sy, col = 'white', lwd = 4, lend = 1)
   text(sx + sb / 2, sy, paste0(sb, ' m'), pos = 3, col = 'white', cex = 2, font = 2)

   if(o$type == 'dem')
      text(xlim[2] - width * 0.03, sy, sprintf('%.2f to %.2f m', z$range[1], z$range[2]),
           adj = c(1, 0.5), col = 'white', cex = 2, font = 2)
}



#' Color an RGB clip using the ortho's fixed stretch
#' @importFrom terra as.array
#' @importFrom grDevices rgb as.raster
#' @keywords internal
#' @noRd


vp_rgb_colors <- function(cr, o) {

   a <- as.array(cr)
   na <- apply(is.na(a) | a == 65535, c(1, 2), all)
   for(b in 1:3)
      a[, , b] <- pmin(pmax((a[, , b] - o$lo[b]) / (o$hi[b] - o$lo[b]), 0), 1)
   a[is.na(a)] <- 0
   col <- rgb(a[, , 1], a[, , 2], a[, , 3])
   col[na] <- NA
   as.raster(matrix(col, nrow(a)))
}



#' Color a DEM clip as shaded relief with an elevation ramp stretched to the clip
#' @returns list of ras (raster of colors) and range (elevation range of stretch)
#' @importFrom terra terrain shade as.matrix
#' @importFrom grDevices hcl.colors col2rgb rgb as.raster
#' @keywords internal
#' @noRd


vp_dem_colors <- function(cr) {

   hs <- shade(terrain(cr, 'slope', unit = 'radians'), terrain(cr, 'aspect', unit = 'radians'),
               angle = 45, direction = 315)
   z <- as.matrix(cr, wide = TRUE)
   h <- as.matrix(hs, wide = TRUE)
   h <- h / max(h, na.rm = TRUE)
   h[is.na(h)] <- 1

   q <- quantile(z, c(0.02, 0.98), na.rm = TRUE)
   zi <- pmin(pmax((z - q[1]) / max(q[2] - q[1], 1e-6), 0), 1)
   pal <- col2rgb(hcl.colors(256, 'Terrain 2')) / 255
   k <- 1 + round(zi * 255)
   k[is.na(k)] <- 1
   s <- 0.35 + 0.65 * h
   col <- rgb(pal[1, k] * s, pal[2, k] * s, pal[3, k] * s)
   col[is.na(z)] <- NA

   list(ras = as.raster(matrix(col, nrow(z))), range = q)
}
