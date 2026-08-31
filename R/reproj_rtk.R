## Pinned operation, verified 2026 against ~800 plots that Emlid had already
## computed with its own on-device GEOID18. Pinned so a PROJ upgrade can't
## silently substitute a different geoid (GEOID12B differs from GEOID18 by only
## a few cm in MA - it would look fine and be wrong). Change only after
## re-running with pipeline = NULL and confirming the checks still pass.

PIPE_2018 <- paste0(
   '+proj=pipeline ',
   '+step +proj=unitconvert +xy_in=deg +xy_out=rad ',
   '+step +inv +proj=vgridshift +grids=us_noaa_g2018u0.tif +multiplier=1 ',
   '+step +proj=lcc +lat_0=41 +lon_0=-71.5 +lat_1=42.6833333333333 ',
   '+lat_2=41.7166666666667 +x_0=200000 +y_0=750000 +ellps=GRS80')


#' Reproject RTK points from Emlid to EPSG:6491+5703
#'
#' NAD83(2011) / Massachusetts Mainland + NAVD88(GEOID18) height.
#' Works from Longitude/Latitude/Ellipsoidal.height, which Emlid writes in the
#' project datum regardless of the configured projection, so every CS.name
#' variant (including plots collected with no vertical datum set) is handled
#' by one transform.
#'
#' @param x Dataframe from Emlid upload
#' @param pipeline PROJ pipeline string; defaults to the pinned one. Pass NULL
#'   to let PROJ choose (and report what it chose).
#' @param tol_mm Max allowed disagreement with Emlid on correctly-configured plots
#' @param validate Require correctly-configured plots to check against
#' @returns x with old_easting/old_northing/old_elevation/old_cs_name retained
#'   and Easting/Northing/Elevation/CS.name replaced. Provenance in attr(., 'reproj').
#' @importFrom sf sf_proj_network sf_proj_pipelines sf_project st_crs st_as_sf st_transform st_coordinates sf_extSoftVersion
#' @importFrom utils packageVersion
#' @export

reproj_rtk <- function(x, pipeline = PIPE_2018, tol_mm = 2, validate = TRUE) {


   target_h <- 'EPSG:6491'                      # NAD83(2011) / MA Mainland, metres
   target   <- 'EPSG:6491+5703'                 # ... + NAVD88 height
   good_cs  <- 'NAD83(2011) / Massachusetts Mainland + NAVD88(GEOID18) height'
   src_3d   <- 'EPSG:6319'                      # NAD83(2011) geographic 3D


   ## ---- input checks ----------------------------------------------------
   need <- c('CS.name', 'Longitude', 'Latitude', 'Ellipsoidal.height',
             'Easting', 'Northing', 'Elevation')
   miss <- setdiff(need, names(x))
   if (length(miss))
      stop('Missing column(s): ', paste(miss, collapse = ', '))

   if (anyNA(x$Longitude) | anyNA(x$Latitude) | anyNA(x$Ellipsoidal.height))
      stop('NA in Longitude, Latitude, or Ellipsoidal.height')

   if (any(x$Latitude < 41 | x$Latitude > 43) |
       any(x$Longitude < -74 | x$Longitude > -69))
      stop('Coordinates fall outside Massachusetts - swapped lat/long, or wrong datum?')

   nm <- st_crs(target_h)$Name
   if (grepl('ft', nm, ignore.case = TRUE) | !identical(st_crs(target_h)$units, 'm'))
      stop('Target CRS is not in metres: ', nm)

   sf_proj_network(enable = TRUE)               # allow CDN fetch of the GEOID18 grid


   ## ---- pipeline: pinned, or derived and verified -----------------------
   descr <- 'pinned'
   if (is.null(pipeline)) {
      pl <- as.data.frame(sf_proj_pipelines(src_3d, target))
      ok <- grepl('g2018', pl$definition, ignore.case = TRUE) & pl$instantiable
      if (!any(ok))
         stop('No instantiable GEOID18 operation - grid unavailable and CDN unreachable?')
      if (which(ok)[1] != 1L)
         stop('GEOID18 is not the top-ranked instantiable operation')
      chosen   <- pl[which(ok)[1], ]
      bp <- grep('ballpark', names(pl), ignore.case = TRUE, value = TRUE)
      if (length(bp) && isTRUE(chosen[[bp[1]]]))
         stop('Selected operation is a ballpark transformation')
      pipeline <- chosen$definition
      descr    <- chosen$description
   }
   if (!grepl('g2018', pipeline, ignore.case = TRUE))
      stop('Pipeline does not use the GEOID18 grid')


   ## ---- axis order ------------------------------------------------------
   ## st_transform() with an explicit pipeline applies sf's own axis handling
   ## for the source CRS, and EPSG:6319 is lat-first - so sf wants (lat,long)
   ## here, while sf_project() wants (long,lat). Not visible in the pipeline
   ## string, and could change with sf. So detect it: transform one point both
   ## ways and keep whichever matches the ordinary CRS-pair transform.
   ## The wrong order lands in the Southern Ocean, off the geoid grid, and
   ## returns non-finite - so the two are never ambiguous.
   probe <- cbind(mean(x$Longitude), mean(x$Latitude), mean(x$Ellipsoidal.height))
   ref   <- sf_project(src_3d, target, probe, keep = TRUE)

   run <- function(xyz, swap) {
      if (swap) xyz <- xyz[, c(2, 1, 3), drop = FALSE]
      p <- st_as_sf(data.frame(X = xyz[, 1], Y = xyz[, 2], Z = xyz[, 3]),
                    coords = c('X', 'Y', 'Z'), crs = src_3d)
      r <- st_coordinates(st_transform(p, crs = target, pipeline = pipeline))
      if (nrow(r) != nrow(xyz))               # st_coordinates drops EMPTY points
         return(matrix(NA_real_, nrow(xyz), 3))
      r
   }
   off <- function(swap) {
      v <- tryCatch(max(abs(run(probe, swap)[, 1:2] - ref[, 1:2])),
                    error = function(e) Inf)
      if (isTRUE(is.finite(v))) v else Inf
   }
   swap <- if (off(FALSE) < 1) FALSE else if (off(TRUE) < 1) TRUE else
      stop('Pipeline disagrees with the CRS-pair transform in either axis order')


   ## ---- transform -------------------------------------------------------
   out <- run(cbind(x$Longitude, x$Latitude, x$Ellipsoidal.height), swap)
   if (!all(is.finite(out)))
      stop(sum(!is.finite(out[, 1])), ' plot(s) returned non-finite coordinates - ',
           'geoid grid missing or points off-grid')


   ## ---- validate against correctly-configured plots ---------------------
   good <- !is.na(x$CS.name) & x$CS.name == good_cs
   dh <- dv <- NA_real_

   if (any(good)) {
      dh <- max(abs(out[good, 1] - x$Easting[good]),
                abs(out[good, 2] - x$Northing[good])) * 1000
      if (dh > tol_mm)
         stop('Horizontal disagreement with Emlid on good plots: ', round(dh, 1), ' mm')

      v <- good & !is.na(x$Elevation)
      if (any(v)) {
         dv <- max(abs(out[v, 3] - x$Elevation[v])) * 1000
         if (dv > tol_mm)
            stop('Vertical disagreement with Emlid on good plots: ', round(dv, 1),
                 ' mm - wrong geoid grid?')
      } else if (validate) {
         stop('No correctly-configured plots carry an Elevation to validate against')
      }
   } else if (validate) {
      stop('No plots with CS.name "', good_cs, '" to validate against; ',
           'set validate = FALSE to override')
   }


   ## ---- replace ---------------------------------------------------------
   x$old_easting   <- x$Easting
   x$old_northing  <- x$Northing
   x$old_elevation <- x$Elevation
   x$old_cs_name   <- x$CS.name

   x$Easting   <- out[, 1]
   x$Northing  <- out[, 2]
   x$Elevation <- out[, 3]
   x$CS.name   <- good_cs

   attr(x, 'reproj') <- list(
      date = Sys.Date(), pipeline = pipeline, axis_swap = swap,
      sf = as.character(packageVersion('sf')),
      proj = unname(sf_extSoftVersion()['PROJ']),
      max_h_mm = dh, max_v_mm = dv)


   ## ---- report ----------------------------------------------------------
   cat('Reprojected', nrow(x), 'plots to', good_cs, '\n\n')
   print(table(x$old_cs_name, dnn = NULL))
   cat('\nOperation: ', descr, '\n')
   cat('sf', as.character(packageVersion('sf')),
       '| PROJ', unname(sf_extSoftVersion()['PROJ']),
       '| axis swap', swap, '\n')
   cat('\nMax disagreement with Emlid on', sum(good), 'good plots:\n')
   cat('  horizontal', round(dh, 2), 'mm\n')
   cat('  vertical  ', round(dv, 2), 'mm\n')

   invisible(x)
}
