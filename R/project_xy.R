#' Project coordinates into a raster's CRS
#'
#' Projects point coordinates into the CRS of a raster, with no datum shift. Both CRSs must
#' use the same datum: orthos, DEMs, and RTK points are all NAD83(2011), but in different
#' projections (EPSG:6491 for Massachusetts Mainland, or UTM 19N for some projects), so this
#' is a projection change only. It's the identity when the CRSs match.
#'
#' This deliberately doesn't do datum shifts. A silent shift between NAD83 and NAD83(2011)
#' is about 1 m in Massachusetts, which is the same size as the ortho registration errors
#' we're trying to measure, so a mismatch has to be reported rather than papered over. Use
#' `gcp_check_crs` to detect one.
#'
#' Note that a compound CRS (EPSG:6491+5703, as the GCPs use) and its horizontal part
#' (EPSG:6491, as the orthos use) have different WKT but the same horizontal projection, so
#' this isn't a no-op for them -- it's a 2D projection between identical horizontal systems,
#' which PROJ handles as the identity on X and Y.
#'
#' @param x Eastings
#' @param y Northings
#' @param from CRS of x and y (an sf crs)
#' @param r SpatRaster to project into
#' @returns Two-column matrix of coordinates in the raster's CRS
#' @importFrom sf st_crs sf_project
#' @importFrom terra crs
#' @keywords internal
#' @noRd


project_xy <- function(x, y, from, r) {

   xy <- cbind(x, y)
   target <- st_crs(crs(r))                                          # raster's CRS, from its WKT (may be compound)
   if(is.na(target))
      stop('Image has no CRS')

   if(identical(st_crs(from)$wkt, target$wkt))                       # same CRS: nothing to do
      return(xy)

   sf_project(st_crs(from)$wkt, target$wkt, xy)
}
