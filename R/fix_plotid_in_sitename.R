#' fix missing plot ids when plot ids were in site_name
#'
#' @param plots Plots data frame
#' @returns Plots data frame
#' @export


fix_plotid_in_sitename <- function(plots) {


   plotno <- suppressWarnings(as.numeric(plots$plot_id))
   bad <- !is.na(plotno)                                                   # plot ids that weren't set, and thus were assigned 1, 2, 3, ...
   fixable <- valid_plotids(plots$site)                                    # some plot ids ended up in site, which is otherwise variable and useless
   x <- strsplit(plots$site_name[bad & fixable], '-')
   plots$old_plot_id <- plots$plot_id
   plots$plot_id[bad & fixable] <- paste(sapply(x, '[[', 1), sapply(x, '[[', 2), sprintf('%03d', plotno[bad & fixable]), sep = '-')

   plots
}
