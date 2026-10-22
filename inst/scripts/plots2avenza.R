# quick and dirty to get selected columns of plots into Avenza
# B. Compton, 21 Oct 2026



library(sf)
library(zip)


zip_layer <- function(base) {
   parts <- list.files(source,
                       pattern = paste0('^', base, '\\.(shp|shx|dbf|prj|cpg)$'))
   out <- file.path(source, paste0(base, '.zip'))
   zip::zipr(zipfile = out, files = parts, root = source)  # root= stores files flat at the zip root
   file.copy(out, dest, overwrite = TRUE)
   out
}



fields <- c('plot_id', 'date', 'subclass', 'lateral_rms', 'section', 'observers', 'notes', 'comments')
source <- 'C:/Work/saltmarsh/data/uas2026/field/results'
pl <- 'plots.gpkg'
dest <- '//marsh01.ecs.umass.edu/web/uas/uas2026/'

plots <- st_read(file.path(source, pl))
plots <- plots[, fields]
plots <- st_zm(plots, drop = TRUE)                  # shapefiles choke on 3D points; Avenza doesn't need Z
plots <- st_transform(plots, 4326)                   # compound CRS (+ NAVD88) doesn't write cleanly to .prj; WGS84 is safest for Avenza
names(plots)[names(plots) == 'lateral_rms'] <- 'lat_rms'   # shapefile field names max 10 chars
plots$name <- sprintf('%s: subclass %02d', plots$plot_id, as.integer(plots$subclass))  # title shown when tapping a plot in Avenza
plots <- plots[, c('name', setdiff(names(plots), 'name'))]

st_write(plots, file.path(source, 'plots_min.shp'), delete_layer = TRUE)
zip_layer('plots_min')
