#' Load an image for gcp
#'
#' Handles the three kinds of image we assess: multiband orthos (displayed as RGB), single-band
#' panchromatic orthos (grayscale), and DEMs (shaded relief). Stretches for orthos are computed
#' once from a sample of the whole image, so brightness doesn't jump from GCP to GCP.
#'
#' @param file Path to geoTIFF
#' @param type 'ortho' or 'dem'
#' @returns List of rast, type, mode ('rgb', 'gray', or 'dem'), and for orthos, lo and hi
#' @importFrom terra rast nlyr spatSample
#' @importFrom stats quantile complete.cases
#' @keywords internal
#' @noRd


gcp_load_image <- function(file, type) {

   r <- rast(file)

   if(type == 'dem')
      return(list(rast = r, type = type, mode = 'dem'))

   if(nlyr(r) == 1)                                                       # panchro orthos are a single band
      mode <- 'gray'
   else {
      mode <- 'rgb'
      bands <- match(c('Red', 'Green', 'Blue'), names(r))                 # micap orthos carry band names
      if(any(is.na(bands)))
         bands <- 1:3
      r <- r[[bands]]
   }

   p <- if(mode == 'gray') c(0.02, 0.98) else c(0.01, 0.99)
   s <- ortho_stretch(r, probs = p)                                       # from the file's statistics, when it has them

   if(mode == 'gray')
      list(rast = r, type = type, mode = mode,
           lo = as.numeric(s$lo[1]), hi = as.numeric(s$hi[1]))
   else                                                                   # linked stretch keeps colors natural
      list(rast = r, type = type, mode = mode,
           lo = rep(min(s$lo), 3), hi = rep(max(s$hi), 3))
}



#' Render a clip of an image centered on a GCP
#'
#' The window is anchored on the surveyed GCP, so every image gets an identical footprint and
#' they can be compared directly. The surveyed position is a cross; a measured position, if
#' there is one, is a second marker with a line showing the offset.
#'
#' Pixels are drawn without interpolation: a 30 cm target is only about 4 pixels across on a
#' micap ortho, and smoothing it would invent detail that isn't there and make the center
#' harder, not easier, to place.
#'
#' @param o Image object from `gcp_load_image`
#' @param x GCP easting (in the image's CRS)
#' @param y GCP northing
#' @param width Width (and height) of clip in m
#' @param file Result PNG file
#' @param px Size of PNG in pixels
#' @param measured Measured position c(x, y) in the image's CRS, or NULL
#' @importFrom terra ext crop xmin xmax ymin ymax res
#' @importFrom grDevices png dev.off gray.colors as.raster
#' @importFrom graphics par plot.new plot.window rasterImage points segments text
#' @keywords internal
#' @noRd


gcp_render <- function(o, x, y, width, file, px = 800, measured = NULL) {

   png(file, width = px, height = px)
   on.exit(dev.off())
   par(mar = rep(0, 4), bg = 'gray30')
   plot.new()
   xlim <- x + c(-1, 1) * width / 2
   ylim <- y + c(-1, 1) * width / 2
   plot.window(xlim = xlim, ylim = ylim, xaxs = 'i', yaxs = 'i', asp = 1)

   r <- o$rast
   if(xlim[2] < xmin(r) | xlim[1] > xmax(r) | ylim[2] < ymin(r) | ylim[1] > ymax(r)) {
      text(x, y, 'GCP is outside this image', col = 'white', cex = 1.8)
      return(invisible())
   }

   buf <- if(o$mode == 'dem') res(r)[1] else 0                            # extra pixel so hillshade reaches the edges
   cr <- crop(r, ext(xlim[1] - buf, xlim[2] + buf, ylim[1] - buf, ylim[2] + buf), snap = 'out')

   ras <- switch(o$mode,
                 dem = vp_dem_colors(cr)$ras,
                 gray = gcp_gray_colors(cr, o),
                 vp_rgb_colors(cr, o))
   rasterImage(ras, xmin(cr), ymin(cr), xmax(cr), ymax(cr), interpolate = FALSE)

   if(!is.null(measured)) {                                               # offset from surveyed to measured
      segments(x, y, measured[1], measured[2], col = '#00E5FF', lwd = 2)
      gcp_cross(measured[1], measured[2], width, '#00E5FF')
   }
   gcp_cross(x, y, width, '#FF7F00')                                      # surveyed position

   sb <- if(width <= 1) 0.2 else if(width <= 2) 0.5 else if(width <= 5) 1 else 5     # scale bar
   sx <- xlim[1] + width * 0.04
   sy <- ylim[1] + width * 0.04
   segments(sx, sy, sx + sb, sy, col = 'black', lwd = 7, lend = 1)
   segments(sx, sy, sx + sb, sy, col = 'white', lwd = 4, lend = 1)
   text(sx + sb / 2, sy, paste0(sb, ' m'), pos = 3, col = 'white', cex = 1.6, font = 2)
}



#' Color a single-band clip as grayscale, using the image's fixed stretch
#'
#' @param cr Cropped SpatRaster
#' @param o Image object from `gcp_load_image`
#' @returns A raster of colors
#' @importFrom terra as.matrix
#' @importFrom grDevices gray as.raster
#' @keywords internal
#' @noRd


gcp_gray_colors <- function(cr, o) {

   z <- as.matrix(cr, wide = TRUE)
   z[z == 65535] <- NA
   v <- pmin(pmax((z - o$lo) / max(o$hi - o$lo, 1e-6), 0), 1)
   col <- gray(v)
   col[is.na(v)] <- NA
   as.raster(matrix(col, nrow(z)))
}



#' Draw an open cross marking a position
#'
#' The arms stop short of the center, leaving a gap, so the marker says exactly where it is
#' without covering the few pixels you need to see to place it. Sized as a fraction of the
#' window, so it looks the same at every zoom.
#'
#' @param x,y Position
#' @param width Window width (m)
#' @param col Color
#' @importFrom graphics segments
#' @keywords internal
#' @noRd


gcp_cross <- function(x, y, width, col) {

   gap <- width * 0.018
   arm <- width * 0.06

   for(w in list(c(5, 'black'), c(2, col))) {                             # dark outline, so it shows on any background
      lwd <- as.numeric(w[1])
      segments(c(x - arm, x + gap, x, x), c(y, y, y - arm, y + gap),
               c(x - gap, x + arm, x, x), c(y, y, y - gap, y + arm),
               col = w[2], lwd = lwd, lend = 1)
   }
}
