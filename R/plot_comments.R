#' Show comments from all plots that have them
#'
#' @param plots Data frame with plot data
#' @returns Data frame of selected plot info for plots that have populated notes field
#' @export


plot_comments <- function(x = plots) {


   z <- x[x$notes != '', c('plot_id', 'subclass', 'rtk_point_number', 'observers', 'photo_date', 'plotid_err', 'rtkid_err', 'notes')]
   print(z, right = FALSE)
   invisible(z)

}
