#' Try to find RTK errors
#'
#' ...............this is a stub. also want to bring time differences in, come up with a way to flag iffy-looking RTKs
#'
#' @param plots Plots data frame
#' @parem rtk RTK data frame
#' @export


flag_rtk <- function(plots, rtk) {


   x <- merge(plots, rtk, by.x = 'rtk_point_number', by.y = 'Name', all.y = FALSE)
   d <- data.frame(plotid = x$plot_id, rtk_id = x$rtk_point_number, photo_date = x$photo_date, rtk_date = x$date.y, delta = difftime(x$photo_date, x$date.y), delta_lag_1 = difftime(x$photo_date, c(0, x$date.y[-nrow(x)])))
   d$lag_better <- abs(d$delta_lag_1) < abs(d$delta)
   tab <- st_as_sf(x, coords = c('latitude', 'longitude')[c(2, 1)])
   st_crs(tab) <- 'EPSG:4326'
   tab <- st_transform(tab, crs = 'EPSG:6491')
   x[, c('tablet_easting', 'tablet_northing')] <- st_coordinates(tab)

   d$dist <- sqrt((x$Easting - x$tablet_easting)^2 + (x$Northing - x$tablet_northing)^2)
   ####### d$dist_lag_1 <- sqrt(((x$Easting - c(0, x$tablet_easting[-nrow(x)]))^2) + (x$Northing - c(0, x$tablet_northing[-nrow(x)])^2))   # this is wrong

   d$dist[d$dist > 100] <- NA
   hist(d$dist, nclass = 25)
   summary(d$dist)

}
