# Format sections shapefile -> Avenza
# for 2026 UAS field work


library(sf)

v <- st_read("C:/Work/saltmarsh/data/uas2026/result/sections.shp")
v$acres <- round(v$acres, 1)
v$name <- sprintf("Sec %02d (target %d)", v$section, v$plots)
st_write(v, "C:/Work/saltmarsh/data/uas2026/result/castle_sections.shp", append = FALSE)

