#' Assign number of plots to a polygon shapefile
#'
#' @param shp Path to sections shapefile
#' @param nplots Number of plots to assign
#' @importFrom sf st_read st_write
#' @importFrom units set_units
#' @export


assign_plots <- function(shp = 'C:/Work/saltmarsh/data/uas2026/result/sections.shp', nplots = 100) {


   x <- st_read(shp)
   x$section[is.na(x$section)] <- 0                                        # null sections are supposed to be zero, but one can't trust ESRI
   x$acres <- as.numeric(units::set_units(st_area(x), "acres"))
   x$plots <- round((x$section != 0) * x$acres * nplots / sum(x$acres[x$section != 0]), 0)


   x$proportion <- (x$section != 0) * x$acres * nplots / sum(x$acres[x$section != 0])  # diagnostics
   z <- as.data.frame(x)

   zzz <<- z[order(z$section),]


   st_write(x, shp, append = FALSE)
   message(nrow(x), ' sections (', round(sum(x$acres)), ' total acres), ', sum(x$section != 0),
           ' visitable sections (', round(sum(x$acres[x$section != 0])), ' acres), ', sum(x$plots), ' total plots')
   message('Results written to ', shp)
}
