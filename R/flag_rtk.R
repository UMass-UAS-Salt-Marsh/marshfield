#' Try to find RTK errors
#'
#' Flag plots with RTK that may be off by one. The only error I expect is when an RTK was not
#' taken for a plot, so the plot is auto-assigned the RTK for the next plot. RTK ids will lag
#' until field crew notices and readjusts (or traces down the error and fixes it in the field).
#'
#' Note that a handful of RTKs were taken out of sequence, as in RTK failures and flagging on
#' day 1.
#'
#' This function isn't very useful, as tablet GPS points are really noisy, and the time sequence
#' isn't consistent. It may provide supporting evidence of bad RTKs.
#'
#' @param x everything data frame
#' @returns A data frame with lots of confusing info
#' @importFrom lubridate as.duration
#' @export


flag_rtk <- function(x) {


   # first, compare plot time (from photo) with RTK time

   x$delta <- as.numeric(as.duration(difftime(x$photo_date, x$date))) / 60  # how long RTK was taken before photo (minutes)
   b <- abs(x$delta) > 10                                                     # ignore deltas > 10 min
   x$delta[b] <- NA
   hist(x$delta, nclass = 25)

   x$delta_lag_1 <- as.numeric(as.duration(difftime(x$photo_date, c(0, x$date[-nrow(x)])))) /60  # how long RTK was taken before previous photo (min)
   b <- abs(x$delta_lag_1) > 25                                                     # ignore deltas > 10 min
   x$delta_lag_1[b] <- NA
   hist(x$delta_lag_1, nclass = 25)

   x$time_lag_better <- abs(x$delta_lag_1) < abs(x$delta)



   # second, compare tablet GPS with RTK GPS

   tab <- st_as_sf(x, coords = c('tablet_latitude', 'tablet_longitude')[c(2, 1)])
   st_crs(tab) <- 'EPSG:4326'
   tab <- st_transform(tab, crs = 'EPSG:6491')
   x[, c('tablet_easting', 'tablet_northing')] <- st_coordinates(tab)

   x$dist <-       sqrt((x$easting - x$tablet_easting)^2 + (x$northing - x$tablet_northing)^2)
   x$dist_lag_1 <- sqrt((x$easting - c(0, x$tablet_easting[-nrow(x)]))^2 + (x$northing - c(0, x$tablet_northing[-nrow(x)]))^2)

   # x$dist[x$dist > 30] <- NA
   hist(x$dist, nclass = 25)
   summary(x$dist)

   # x$dist_lag_1[x$dist_lag_1 > 30] <- NA
   hist(x$dist_lag_1, nclass = 25)
   summary(x$dist_lag_1)

   x$dist_lag_better <- abs(x$dist_lag_1) < abs(x$dist)

   x
}
