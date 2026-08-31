A grab-bag package for planning a vegetation sampling field season for
the UMass UAS project (specifically, UMass UAS 2026), including formatting tide
tables, preparing a site shapefile, preparing maps for Avenza, summarizing field
samples on the fly, and processing collected data.

Companion package to [marshmap](https://github.com/UMass-UAS-Salt-Marsh/marshmap).

# Functions to prepare for field season

tides - Summarize NOAA tide predictions for field work planning
make_site - Make final site shapefile from draft shapefile
assign_plots - Assign number of plots to a polygon shapefile
format_sections - Format sections shapefile for Avenza
shapefiles_to_gpkg - Package shapefiles to a gpkg for Avenza
orthos_to_tiff - Process MassGIS 2025 orthos into a geoTIFF for Avenza
avenza_qr - Make QR codes for downloading map files in Avenza

# Functions to summarize data during field season

subclass_freq - Summzarize subclass frequency for within-field season updates

# Functions to process field data

get_plots - Top-level function to clean up and process field data
get_photos - Download plot photos from the server
valid_plotids - Return TRUE for fields where the plot_id is valid for UMass UAS 2026
clean_plotids - Clean plot ids for UMass UAS 2026
gather_rtk - Gather RTK GPS points for UMass UAS 2026
reproj_rtk - Reproject RTK points from Emlid to EPSG:6491+5703
