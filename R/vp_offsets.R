#' Read ortho offsets for view_plots
#'
#' @param file Path to offsets.txt
#' @returns Data frame of plot_id, ortho, dx, dy
#' @importFrom utils read.table
#' @keywords internal
#' @noRd


vp_read_offsets <- function(file) {

   if(!file.exists(file))
      return(data.frame(plot_id = character(0), ortho = character(0), dx = numeric(0), dy = numeric(0)))
   read.table(file, sep = '\t', header = TRUE, quote = '', comment.char = '',
              colClasses = c('character', 'character', 'numeric', 'numeric'))
}



#' Write ortho offsets for view_plots
#'
#' @param offsets Data frame of plot_id, ortho, dx, dy
#' @param file Path to offsets.txt
#' @returns TRUE if successful
#' @importFrom utils write.table
#' @keywords internal
#' @noRd


vp_write_offsets <- function(offsets, file) {

   offsets <- offsets[order(offsets$ortho, offsets$plot_id), ]
   tryCatch({
      write.table(offsets, file, sep = '\t', row.names = FALSE, quote = FALSE)
      TRUE
   }, error = function(e) FALSE, warning = function(w) FALSE)
}



#' Summarize plot offsets measured in view_plots
#'
#' In `view_plots`, double-clicking on the ortho where the plot center actually appears records
#' the offset (`dx`, `dy`, in m, east and north) from where the plot is drawn to where it
#' appears, in `pars/offsets.txt`. This summarizes offsets for each ortho, to help distinguish
#' ortho registration error from RTK (e.g., pole tilt) error:
#'
#' - A **systematic** offset (mean offset large relative to its standard error, similar
#'   direction for all plots) suggests ortho registration error.
#' - **Random** offsets (mean near zero, scattered directions) suggest RTK error at each plot.
#' - A **trend** across the site (offsets varying with easting or northing) suggests the ortho
#'   is warped.
#'
#' @param path Path to field data
#' @returns Data frame of offsets joined with plot coordinates, invisibly
#' @importFrom sf st_read st_coordinates st_drop_geometry
#' @importFrom stats sd lm
#' @export


offset_summary <- function(path = 'C:/Work/saltmarsh/data/uas2026/field') {

   x <- vp_read_offsets(file.path(path, 'pars/offsets.txt'))
   if(nrow(x) == 0) {
      message('No offsets recorded yet')
      return(invisible(x))
   }

   plots <- st_read(file.path(path, 'prelim/plots.gpkg'), quiet = TRUE)
   xy <- st_coordinates(plots)
   i <- match(x$plot_id, plots$plot_id)
   x$easting <- xy[i, 1]
   x$northing <- xy[i, 2]
   x$dist <- sqrt(x$dx ^ 2 + x$dy ^ 2)
   x$bearing <- round((atan2(x$dx, x$dy) * 180 / pi) %% 360)

   for(o in unique(x$ortho)) {
      z <- x[x$ortho == o, ]
      n <- nrow(z)
      cat('\n', o, ': ', n, ' plots\n', sep = '')
      print(z[, c('plot_id', 'dx', 'dy', 'dist', 'bearing')], row.names = FALSE, digits = 3)

      mx <- mean(z$dx)
      my <- mean(z$dy)
      cat(sprintf('\n  Mean offset:  dx = %.3f, dy = %.3f  (%.3f m toward %d deg)\n', mx, my,
                  sqrt(mx ^ 2 + my ^ 2), round((atan2(mx, my) * 180 / pi) %% 360)))
      cat(sprintf('  Mean distance: %.3f m\n', mean(z$dist)))
      if(n > 1) {
         cat(sprintf('  SD:            dx = %.3f, dy = %.3f\n', sd(z$dx), sd(z$dy)))
         cat(sprintf('  SE of mean:    dx = %.3f, dy = %.3f\n', sd(z$dx) / sqrt(n), sd(z$dy) / sqrt(n)))
      }
      if(n >= 6) {                                                                    # trend across site
         p <- sapply(c('dx', 'dy'), function(v)                                     # coefficient tests (not sequential, since E & N may be correlated)
            summary(lm(z[[v]] ~ z$easting + z$northing))$coefficients[2:3, 4])
         cat(sprintf('  Trend p-values: dx ~ E %.3f, N %.3f;  dy ~ E %.3f, N %.3f\n', p[1, 1], p[2, 1], p[1, 2], p[2, 2]))
      }
   }

   invisible(x)
}
