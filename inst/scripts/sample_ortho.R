library(terra)
library(sf)

path <- 'U:/projects/uas_veg/sites/esx/orthos'
file <- 'esx_2026_08_07_mid_micap_ortho.tif'
#file <- 'esx_2026_08_07_mid_panchro_ortho.tif'
x <- rast(file.path(path, file))


pts <- st_read('U:/projects/uas_veg/sites/esx/gcp_placement/vector/gcp.gpkg')
ctr <- pts[1,]
window <- 2                                                                # width of window (m)


xy <- st_coordinates(ctr)[1, 1:2, drop = FALSE]
half <- window / 2                                                           # anchored on the point, so every ortho gets the same footprint
e <- ext(xy[1, 1] - half, xy[1, 1] + half, xy[1, 2] - half, xy[1, 2] + half)

bands <- if(nlyr(x) == 1) 1 else c('Red', 'Green', 'Blue')                   # panchro orthos are a single band
cr <- crop(x[[bands]], e, snap = 'near')
cr[cr == 65535] <- NA                                                        # MicaSense nodata

if(nlyr(cr) == 1) {                                                          # panchro: gray with a 2-98% stretch
   q <- as.numeric(global(cr, quantile, probs = c(0.02, 0.98), na.rm = TRUE))
   plot(cr, col = gray.colors(256, 0, 1, gamma = 1), range = q, legend = FALSE,
        axes = FALSE, mar = 2)
} else                                                                       # else must hug the brace at top level
   plotRGB(cr, stretch = 'lin', mar = 2)
points(xy[1, 1], xy[1, 2], pch = 3, col = 'cyan', cex = 2, lwd = 2)
