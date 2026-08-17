# Package shapefiles to a gpkg for Avenza
# fire up VPN before running!



library(sf)
library(zip)


source <- 'C:/Work/saltmarsh/data/uas2026/result'
dest <- '//marsh01.ecs.umass.edu/web/uas/uas2026/'


zip_layer <- function(base) {
   parts <- list.files(source,
                       pattern = paste0("^", base, "\\.(shp|shx|dbf|prj|cpg)$"))
   out <- file.path(source, paste0(base, ".zip"))
   zip::zipr(zipfile = out, files = parts, root = source)  # root= stores files flat at the zip root
   file.copy(out, dest, overwrite = TRUE)
   out
}



# clip open space to our area of interest
open <- st_read("C:/GIS/GIS/openspace/OPENSPACE_POLY.shp")
clip <- st_read("C:/GIS/Orthophotos/2025/Essex2025.shp")
open <- st_crop(open, clip)
st_write(open, "C:/Work/saltmarsh/data/uas2026/result/open_space.shp", layer = "open_space", append = FALSE)




zip_layer("open_space")
zip_layer("castle_sections")
zip_layer("work_areas")
