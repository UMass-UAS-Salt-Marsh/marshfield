# MassGIS 2025 orthos -> geoTIFF for Avenza
# Pick out tiles here: https://maps.massgis.digital.mass.gov/MassMapper/MassMapper.html?bl=2025%20Aerial%20Imagery__100&l=massgis:GISDATA.COQ2025INDEX_POLY__GISDATA.COQ2025INDEX_POLY::Labels__ON__100,massgis:GISDATA.COQ2025INDEX_POLY__GISDATA.COQ2025INDEX_POLY::Default__ON__100&b=-71.51206970214845,42.235635122140614,-70.7292938232422,42.538409837545586


library(terra)

# 1. Virtual mosaic of the 6 tiles — no memory load, no intermediate file
tiles <- list.files("C:/GIS/Orthophotos/2025/", pattern = "\\.jp2$", full.names = TRUE)
m <- vrt(tiles)

# 2. Drop NIR -> RGB only (bands are 1=R 2=G 3=B 4=NIR)
m <- m[[1:3]]

# 3. Clip to your study area's bounding box
aoi <- vect("C:/GIS/Orthophotos/2025/Essex2025.shp")
aoi <- project(aoi, m)          # reproject AOI to the imagery CRS if needed
clipped <- crop(m, ext(aoi))    # rectangular basemap; use mask(clipped, aoi) if you want it cut to the polygon

# 4. Write an Avenza-friendly COG
writeRaster(clipped, "C:/GIS/Orthophotos/2025/essex2025.tif",
            filetype = "COG",
            gdal = c("COMPRESS=JPEG", "QUALITY=85", "BLOCKSIZE=512"),
            overwrite = TRUE)