#' TEMPORARY: reproject plot coordinates into an ortho's CRS for view_plots
#'
#' **Temporary workaround, to be removed** once the UAS group delivers orthos in
#' NAD83(2011) / Massachusetts Mainland (EPSG:6491). The July 2026 orthos are labeled
#' EPSG:26986 (NAD83(1986)); the correct NADCON5 shift from the plots' NAD83(2011) is about
#' 0.15 m NW.
#'
#' Uses an explicit grid-based pipeline. Beware: a plain `st_transform` (or `terra::project`)
#' between these CRSs silently applies a zero shift, because GDAL doesn't find the NADCON5
#' grids. This refuses to proceed if no grid-based transformation is available.
#'
#' Orthos in any NAD83(2011) CRS (e.g., EPSG:6491, or UTM 19N) are simply projected, with no
#' datum shift, so this is harmless for correctly-labeled orthos and LiDAR. If all orthos end
#' up in EPSG:6491, it's a no-op; if any are in another projection (e.g., UTM), keep the
#' projection part permanently.
#'
#' To remove it entirely (only if all orthos are in EPSG:6491):
#' 1. Delete R/vp_temp_reproject.R.
#' 2. In `get_ortho` in view_plots, change the line marked TEMPORARY to
#'    `o$xy <- cbind(plots$easting, plots$northing)`.
#' 3. Run devtools::document().
#'
#' @param x Plot eastings
#' @param y Plot northings
#' @param plot_crs CRS of plots (sf crs); must be NAD83(2011) / Massachusetts Mainland
#' @param ortho SpatRaster
#' @returns Two-column matrix of coordinates in the ortho's CRS
#'
#' @importFrom sf sf_proj_network sf_proj_pipelines st_as_sf st_transform st_coordinates st_crs sf_project
#' @importFrom terra crs
#' @keywords internal
#' @noRd


vp_temp_reproject <- function(x, y, plot_crs, ortho) {

   xy <- cbind(x, y)
   if(!startsWith(plot_crs$Name, 'NAD83(2011) / Massachusetts Mainland'))
      stop('TEMPORARY vp_temp_reproject expects plots in NAD83(2011) / Massachusetts Mainland')

   target <- st_crs(crs(ortho))                                                     # ortho's CRS, from its WKT (may be compound)
   if(is.na(target))
      stop('Ortho has no CRS')

   if(startsWith(target$Name, 'NAD83(2011)'))                                       # same datum: plain projection (identity for 6491)
      return(sf_project('EPSG:6491', target$wkt, xy))

   sf_proj_network(enable = FALSE)                                                  # projection with no datum shift, for comparison
   r0 <- sf_project('EPSG:6491', target$wkt, xy)
   sf_proj_network(enable = TRUE)                                                   # fetch NADCON5 grids from PROJ CDN
   pl <- as.data.frame(sf_proj_pipelines('EPSG:6491', target))
   ok <- pl$instantiable & pl$grid_count > 0
   if(!any(ok))
      stop('No grid-based transformation from EPSG:6491 to ', target$Name, ' (grids unavailable, or offline?)')

   p <- st_as_sf(data.frame(x = x, y = y), coords = c('x', 'y'), crs = 'EPSG:6491')
   r <- st_coordinates(st_transform(p, target, pipeline = pl$definition[which(ok)[1]]))[, 1:2]

   d <- sqrt(rowSums((r - r0) ^ 2))                                                 # datum shift
   if(!all(is.finite(d)) | any(d == 0) | any(d > 5))
      stop('Suspicious transformation from EPSG:6491 to ', target$Name, ': shifts of ', min(d), ' to ', max(d), ' m')

   message('TEMPORARY: plot centers shifted from NAD83(2011) to ', target$Name, ' for display (',
           round(mean(d), 3), ' m)')
   r
}
