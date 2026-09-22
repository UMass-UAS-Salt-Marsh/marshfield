#' Try to find RTK errors
#'
#' Flag plots with RTK that may be off by one. The only error I expect is when an RTK was not
#' taken for a plot, so the plot is auto-assigned the RTK for the next plot. RTK ids will lag
#' until field crew notices and readjusts (or traces down the error and fixes it in the field).
#'
#' Note that a handful of RTKs were taken out of sequence, as in RTK failures and flagging on
#' day 1.
#'
#' @param x everything data frame
#' @param rtk RTK data frame
#' @returns A data frame with lots of confusing info
#' @importFrom lubridate as.duration
#' @export


flag_rtk <- function(x, rtk, path) {


# first, compare plot time (from photo) with RTK time
   x <- x[order(x$plot_id), ]
   d <- x[, c('plot_id', 'rtk_id', 'photo_date', 'date', 'easting', 'northing', 'tablet_latitude', 'tablet_longitude', 'observers', 'notes')]

   d <- sort_plots(d, path)


   d$delta <- as.numeric(as.duration(difftime(d$photo_date, d$date))) / 60  # how long RTK was taken before photo (minutes)
   b <- abs(d$delta) > 10                                                     # ignore deltas > 10 min
   d$delta[b] <- NA
   hist(d$delta, nclass = 25)

   d$delta_lag_1 <- as.numeric(as.duration(difftime(d$photo_date, c(0, d$date[-nrow(d)])))) /60  # how long RTK was taken before previous photo (min)
   b <- abs(d$delta_lag_1) > 25                                                     # ignore deltas > 10 min
   d$delta_lag_1[b] <- NA
   hist(d$delta_lag_1, nclass = 25)

   d$time_lag_better <- abs(d$delta_lag_1) < abs(d$delta)



   # second, compare tablet GPS with RTK GPS

   tab <- st_as_sf(d, coords = c('tablet_latitude', 'tablet_longitude')[c(2, 1)])
   st_crs(tab) <- 'EPSG:4326'
   tab <- st_transform(tab, crs = 'EPSG:6491')
   d[, c('tablet_easting', 'tablet_northing')] <- st_coordinates(tab)

   d$dist <-       sqrt((d$easting - d$tablet_easting)^2 + (d$northing - d$tablet_northing)^2)
   d$dist_lag_1 <- sqrt((d$easting - c(0, d$tablet_easting[-nrow(d)]))^2 + (d$northing - c(0, d$tablet_northing[-nrow(d)]))^2)

  # d$dist[d$dist > 30] <- NA
   hist(d$dist, nclass = 25)
   summary(d$dist)

  # d$dist_lag_1[d$dist_lag_1 > 30] <- NA
   hist(d$dist_lag_1, nclass = 25)
   summary(d$dist_lag_1)

   d$dist_lag_better <- abs(d$dist_lag_1) < abs(d$dist)

   d
}
