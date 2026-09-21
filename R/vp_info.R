#' Build plot data display for view_plots
#'
#' Displays plot id, date, observers, RTK id, coordinates, lateral RMS, subclass, and a table of
#' percent cover. Percent cover always lists the 8 primary species in a fixed order (blank for
#' 0%), followed by any other species recorded for the plot. Notes are displayed if present.
#'
#' @param p One row of plots
#' @param cover Rows of pct_cover for this plot
#' @returns HTML
#' @importFrom shiny tags
#' @keywords internal
#' @noRd


vp_info <- function(p, cover) {

   fixed <- data.frame(label = c('S. alterniflora', 'S. patens', 'D. spicata', 'J. gerardii',
                                 'Limonium sp.', 'Salicornia sp.', 'Suaeda sp.', 'bare ground'),
                       pattern = c('^Spartina alterniflora', '^Spartina patens', '^Distichlis',
                                   '^Juncus', '^Limonium', '^Salicornia', '^Suaeda', '^Bare'),
                       italic = c(rep(TRUE, 7), FALSE))

   cover <- cover[!is.na(cover$species) & cover$species != '', ]
   hit <- matrix(vapply(fixed$pattern, grepl, logical(nrow(cover)), x = cover$species, ignore.case = TRUE),
                 nrow = nrow(cover), ncol = nrow(fixed))
   fixed$pct <- colSums(hit * cover$pct_cover, na.rm = TRUE)
   fixed$pct[colSums(hit) == 0] <- NA

   other <- cover[rowSums(hit) == 0, ]                                            # species that aren't in the fixed list
   if(nrow(other) > 0)
      fixed <- rbind(fixed, data.frame(label = sub('^(\\w)\\w* ', '\\1. ', other$species), pattern = '',
                                       italic = TRUE, pct = other$pct_cover))

   cell <- function(label, italic) if(italic) tags$td(tags$i(label)) else tags$td(label)
   rows <- lapply(seq_len(nrow(fixed)), function(i)
      tags$tr(cell(fixed$label[i], fixed$italic[i]),
              tags$td(class = 'pct', if(is.na(fixed$pct[i])) '' else paste0(fixed$pct[i], '%'))))

   field <- function(label, value) tags$tr(tags$td(class = 'label', label), tags$td(value))

   tags$div(
      tags$table(class = 'vp-table',
                 field('Plot', tags$b(p$plot_id)),
                 field('Date', format(p$date, '%Y-%m-%d %H:%M')),
                 field('Observers', p$observers),
                 field('RTK id', p$rtk_id),
                 field('Easting', sprintf('%.2f', p$easting)),
                 field('Northing', sprintf('%.2f', p$northing)),
                 field('Lateral RMS', sprintf('%.3f m', p$lateral_rms))),
      if(isTRUE(p$plotid_err) | isTRUE(p$rtkid_err))
         tags$div(class = 'vp-error', paste(c('Bad plot id', 'Bad RTK id')[c(isTRUE(p$plotid_err), isTRUE(p$rtkid_err))],
                                            collapse = '; ')),
      tags$div(class = 'vp-subclass', paste0('Subclass ', p$subclass,
                                                 if(!is.na(p$subclass_name) && p$subclass_name != '') paste0(' (', p$subclass_name, ')'))),
      tags$h6('Percent cover'),
      if(nrow(cover) == 0)
         tags$div(class = 'vp-note', 'No percent cover recorded'),
      tags$table(class = 'vp-table', rows),
      if(!is.na(p$notes) && p$notes != '')
         tags$div(tags$h6('Notes'), p$notes)
   )
}
