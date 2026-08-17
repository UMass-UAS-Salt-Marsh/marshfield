#' Make a site shapefile
#'
#' Start with polygons for selected parcels. Buffers roads, structures and golf courses by specified
#' distance for drone safety (use 50 m for Wingtra One). Produces final shapefile and reports area in
#' acres.
#' @param target Target shapefile name
#' @param source Source path
#' @param result Result path
#' @param roads Name of roads shapefile (edit out non-traveled roads)
#' @param structures Name of structures shapefile (edit out unoccupied buildings)
#' @param golf Name of golf course shapefile (or other areas to avoid)
#' @param dist Buffer around occupied areas to avoid (m)
#' @importFrom sf st_read st_geometry st_buffer st_intersection st_sf st_difference st_set_precision st_make_valid st_write st_area st_union
#' @importFrom units set_units
#' @export


make_site <- function(target = 'castle.shp',
                      source = 'C:/Work/saltmarsh/data/uas2026/source',
                      result = 'C:/Work/saltmarsh/data/uas2026/result',
                      roads = 'roads.shp',
                      structures = 'structures_poly_92.shp',
                      golf = 'golf.shp',
                      dist = 50) {


   area <- file.path(source, target)
   roads <- file.path(source, roads)
   structures <- file.path(source, structures)
   golf <- file.path(source, golf)                          # golf courses and other excluded polygons


   a <- st_union(st_read(area))                             # dissolve parcels, drop attributes
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

   z <- z |>                                                # clean up slivers
      st_set_precision(1e6) |>
      st_make_valid()

   st_write(z, f <- file.path(result, target), append = FALSE)


   acres <- as.numeric(sum(units::set_units(st_area(z), "acres")))
   message('Project area is ', round(acres, 1), ' acres, written to ', f)
}
