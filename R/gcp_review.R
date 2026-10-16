#' Read GCP review table
#'
#' Reads `gcp_review.txt` if it exists. Rows are keyed on `gcp_name` and `image`, so each GCP
#' has one measurement per image.
#'
#' @param file Path to gcp_review.txt
#' @returns Data frame of measurements (zero rows if the file doesn't exist)
#' @importFrom utils read.table
#' @keywords internal
#' @noRd


gcp_read_review <- function(file) {

   empty <- data.frame(gcp_name = character(0), image = character(0),
                       gcp_easting = numeric(0), gcp_northing = numeric(0), gcp_elevation = numeric(0),
                       ortho_easting = numeric(0), ortho_northing = numeric(0), ortho_elevation = numeric(0),
                       xy_error = numeric(0), xy_direction = character(0), z_error = numeric(0))

   if(!file.exists(file))
      return(empty)

   x <- read.table(file, sep = '\t', header = TRUE, quote = '', comment.char = '',
                   colClasses = 'character', na.strings = c('', 'NA'))

   for(f in names(empty)[!names(empty) %in% names(x)])                     # tolerate older files missing columns
      x[[f]] <- NA
   for(f in names(empty)[sapply(empty, is.numeric)])
      x[[f]] <- suppressWarnings(as.numeric(x[[f]]))
   x$xy_direction[is.na(x$xy_direction)] <- ''

   x[, names(empty)]
}



#' Write GCP review table
#'
#' @param review Data frame of measurements
#' @param file Path to gcp_review.txt
#' @returns TRUE if successful
#' @importFrom utils write.table
#' @keywords internal
#' @noRd


gcp_write_review <- function(review, file) {

   review <- review[order(review$image, review$gcp_name), ]

   for(f in c('gcp_easting', 'gcp_northing', 'ortho_easting', 'ortho_northing'))       # mm is plenty, and keeps the file readable
      review[[f]] <- sprintf('%.3f', review[[f]])
   for(f in c('gcp_elevation', 'ortho_elevation', 'xy_error', 'z_error'))
      review[[f]] <- ifelse(is.na(review[[f]]), NA, sprintf('%.3f', review[[f]]))

   tryCatch({
      write.table(review, file, sep = '\t', row.names = FALSE, quote = FALSE, na = '')
      TRUE
   }, error = function(e) FALSE, warning = function(w) FALSE)
}



#' Compass direction for an offset
#'
#' @param dx Offset east (m)
#' @param dy Offset north (m)
#' @returns 16-point compass direction, or '' for a zero offset
#' @keywords internal
#' @noRd


gcp_direction <- function(dx, dy) {

   if(is.na(dx) | is.na(dy) | (dx == 0 & dy == 0))
      return('')
   pts <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE',
            'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')
   pts[round(((atan2(dx, dy) * 180 / pi) %% 360) / 22.5) %% 16 + 1]
}



#' Summary statistics for the measurements on one image
#'
#' Bias is the mean signed offset, which is the statistic that distinguishes a registration
#' error (all the GCPs pushed the same way) from RTK or click scatter (offsets in every
#' direction, averaging to nothing). It can't come from the stored `xy_error`, which is an
#' unsigned distance whose mean is always positive; the signed components are recovered from the
#' coordinate columns instead.
#'
#' The standard error is reported next to the bias on purpose. With seven GCPs, SE is about
#' SD/2.6, so a bias smaller than roughly that isn't distinguishable from scatter -- and reading
#' a "bias" off a handful of noisy points is exactly the mistake this is meant to prevent.
#'
#' Z is summarized only over rows measured against the same vertical datum, since mixing NAVD88
#' and ellipsoidal heights would be meaningless, and rows whose datum couldn't be determined are
#' left out altogether.
#'
#' @param review Review table, already subset to one image
#' @param n_gcp Number of GCPs at the site
#' @param vdatum Vertical datum in use for this image ('navd88', 'ellipsoidal', 'unknown'), or
#'   NULL for an ortho
#' @returns List of n, n_gcp, and (when there are measurements) bias_x, bias_y, bias, bias_dir,
#'   bias_se, rmse, lo, hi, and for DEMs z_bias, z_se, z_rmse, z_lo, z_hi, z_n
#' @importFrom stats sd
#' @keywords internal
#' @noRd


gcp_stats <- function(review, n_gcp, vdatum = NULL) {

   out <- list(n = nrow(review), n_gcp = n_gcp)
   if(out$n == 0)
      return(out)

   dx <- review$ortho_easting - review$gcp_easting                        # signed, unlike the stored xy_error
   dy <- review$ortho_northing - review$gcp_northing
   ok <- is.finite(dx) & is.finite(dy)
   dx <- dx[ok]
   dy <- dy[ok]
   if(length(dx) == 0)
      return(out)

   mx <- mean(dx)
   my <- mean(dy)
   d <- sqrt(dx ^ 2 + dy ^ 2)

   out$bias_x <- mx
   out$bias_y <- my
   out$bias <- sqrt(mx ^ 2 + my ^ 2)
   out$bias_dir <- gcp_direction(mx, my)
   out$bias_se <- if(length(dx) > 1)                                      # SE of the mean offset vector
      sqrt((sd(dx) ^ 2 + sd(dy) ^ 2) / length(dx)) else NA_real_
   out$rmse <- sqrt(mean(d ^ 2))
   out$lo <- min(d)
   out$hi <- max(d)

   if(!is.null(vdatum) && vdatum != 'unknown') {                          # Z only where the datum is known
      z <- review$z_error[is.finite(review$z_error)]
      if(length(z) > 0) {
         out$z_n <- length(z)
         out$z_bias <- mean(z)
         out$z_se <- if(length(z) > 1) sd(z) / sqrt(length(z)) else NA_real_
         out$z_rmse <- sqrt(mean(z ^ 2))
         out$z_lo <- min(z)
         out$z_hi <- max(z)
      }
   }

   out
}
