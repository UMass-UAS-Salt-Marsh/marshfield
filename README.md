A grab-bag package for planning a vegetation sampling field season for
the UMass UAS project (specifically, UMass UAS 2026), including formatting tide
tables, preparing a site shapefile, preparing maps for Avenza, summarizing field
samples on the fly, and processing collected data.

Companion package to [marshmap](https://github.com/UMass-UAS-Salt-Marsh/marshmap).

## Functions to prepare for field season

- `tides` Summarize NOAA tide predictions for field work planning
- `make_site` Make final site shapefile from draft shapefile
- `assign_plots` Assign number of plots to a polygon shapefile
- `format_sections` Format sections shapefile for Avenza
- `shapefiles_to_gpkg` Package shapefiles to a gpkg for Avenza
- `orthos_to_tiff` Process MassGIS 2025 orthos into a geoTIFF for Avenza
- `avenza_qr` Make QR codes for downloading map files in Avenza

## Functions to summarize data during field season

- `subclass_freq` Summzarize subclass frequency for within-field season updates

## Functions to process field data

- `check_pct_cover` Check percent cover for out-of-range values
- `check_plot_rtk_dups` Check for duplicated RTK ids in plot data and flag them
- `check_plot_rtkids` Check for valid RTK ids in plots data and report errors
- `check_plotid_dates` Check for wrong dates in plot ids
- `check_plotid_dups` Check for plot ids shared by more than one vegetation record and throw an error
- `check_plotids` Check plot ids, reporting invalid ids
- `check_rtk_dups` Check for duplicate RTK ids and flag them
- `check_rtk_rtkids` Check for valid RTK ids in RTK data and report errors
- `clean_plotids` Clean plot ids for UMass UAS 2026
- `clean_rtkids` Clean RTK ids for UMass UAS 2026
- `deal_with_photos` Rename photos to plot numbers and pull plot date & time from EXIF data
- `drop_plots` Drop bad plots designated in drop_plots.txt
- `fix_observers` Fix observer initials for correctness and consistency
- `fix_pctcover` Fix errors in percent cover
- `fix_plot_rtk` Correct bad RTKs in plots from parameter files
- `fix_plotid_in_sitename` Fix missing plot ids when plot ids were in site_name
- `fix_plots` Clean up incorrect plot ids
- `fix_rtkids` Rename duplicated RTK ids according to dedup_rtk.txt
- `fix_sections` Clean up incorrect section numbers
- `fix_species` Fix incorrect species names in data file (bogus specific for genera)
- `gather_rtk` Gather RTK GPS points for UMass UAS 2026
- `get_photos` Download plot photos from the server
- `get_plots` Top-level function to clean up and process field data
- `process_reviews` If review data are available, drop rejected plots
- `rename_cols` Rename and delete columns in data frame from parameter file
- `rename_cols` Rename and delete columns in data frame from parameter file
- `reproj_rtk` Reproject RTK points from Emlid to EPSG:6491+5703
- `rtk_sequence` Check for agreement in plot id and RTK id sequences
- `sort_plots` Sort plots in canonical order
- `sort_plots` Sort plots in canonical order
- `subclasses` Print frequency table of subclasses
- `valid_plotids` Return TRUE for fields where the plot_id is valid for UMass UAS 2026
- `valid_rtkids` Return TRUE for fields where the RTK id is valid for UMass UAS 2026

## Processing field data

- `get_plots` Process plot data. This is iterative--run it again after fixing errors and reviewing.
- `view_plots` Show plot data, including photos and orthophotos, and enter review flags and comments.


### Field data processing parameters

- `sessions_names.txt` Fields to drop (use `-`) or rename in sessions: `old`, `new`
- `plots_names.txt` Fields to drop (use `-`) or rename in plots: `old`, `new`
- `rtk_names.txt` Fields to drop (use `-`) or rename in RTK: `old`, `new`
- `field_days.txt` Day abbreviations (J21, J22, ..., A07) in order, for sorting data: `day`

- `fix_plots.txt` Fix bad plot ids: `old`, `new`, `fix_plots_reason`
- `drop_plots.txt` Plots to drop: `plot_id`, `drop_confirmed`, and `drop_reason`
- `fix_observers.txt` Fix errors and inconsistencies in observers: `old`, `new`, `reason`.
   Full observer names are in observers.txt.
- `fix_sections.txt` Split field days into sections: `start`, `end`, `is_section`, `reason`

- `fix_rtk.txt` RTK ids to reassign (usually thanks to off-by-one errors): `plot_id`,
- `new_rtk_id`, `fix_rtk_confirmed`, `fix_rtk_reason`
- `dedup_rtk.txt` Drop duplicated RTKs with same rtk_id: `rtk_id`, `seq`, `new`, `reason`

- `fix_species.txt` Change genera incorrectly listed as species by app in percent cover
- `fix_pct_cover.txt` Changes to percent cover fields: `plot_id`, `species_code`, `pct_cover`, `reason`

- `review.txt` Written by `view_plots`, used to drop rejected plots. DO NOT EDIT!
