# make_site
# Make a site shapefile
# Start with polygons for selected parcels
# Buffers roads, structures and golf courses by 50 m for Wingtra safety
# produces final shapefile and reports are in acres
# for 2026 UAS field work
# B. Compton, 2 Jul 2026



library(sf)



# study area: selected parcels for field site
target <- 'castle.shp'                                      # all TTOR-owned open space, minimal southern contiguous section


source <- 'C:/Work/saltmarsh/data/uas2026/source'
result <- 'C:/Work/saltmarsh/data/uas2026/result'

area <- file.path(source, target)                     
roads <- file.path(source, 'roads.shp')
structures <- file.path(source, 'structures_poly_92.shp')
golf <- file.path(source, 'golf.shp')                       # golf courses and other excluded polygons
dist <- 50                                                  # exclusion distance (m)   
#### dist <- 150                                                  # exclusion distance (m)   


a <- st_union(st_read(area))                                # dissolve parcels, drop attributes
r <- st_geometry(st_read(roads))
s <- st_geometry(st_read(structures))
g <- st_geometry(st_read(golf))

biga <- st_buffer(a, dist)
excl <- suppressWarnings(
   c(st_buffer(st_intersection(r, biga), dist),
     st_buffer(st_intersection(s, biga), dist),
     st_buffer(st_intersection(g, biga), dist)) |>
   st_union())
z <- st_sf(geometry = st_difference(a, excl))

z <- z |>                                                   # clean up slivers
   st_set_precision(1e6) |> 
   st_make_valid()

#### target <- 'castle_150_buffer.shp'
st_write(z, f <- file.path(result, target), append = FALSE)


acres <- as.numeric(sum(units::set_units(st_area(z), "acres")))
message('Project area is ', round(acres, 1), ' acres, written to ', f)

