#' Fix incorrect subclasses
#'
#' Parameter file:
#' - `fix_subclass.txt` Changes to subclass: `plot_id`, `subclass`, `reason`
#'
#' @param plots Plots data frame
#' @param path Base path
#' @returns Plots data frame, with corrected `subclass` and new column `fix_subclass_reason`
#' @export


fix_subclass <- function(plots, path) {


   fix <- read.table(file.path(path, 'pars/fix_subclass.txt'), sep = '\t', header = TRUE, quote = '', fill = TRUE)
   fix <- fix[!is.na(fix$plot_id) & trimws(fix$plot_id) != '', ]           # drop blank lines

   missing <- fix$plot_id[!fix$plot_id %in% plots$plot_id]
   if(length(missing) > 0)
      warning('Plot ids in fix_subclass.txt not found in plots: ', paste(missing, collapse = ', '))

   b <- match(plots$plot_id, fix$plot_id)
   plots$fix_subclass_reason <- NA
   plots[!is.na(b), c('subclass', 'fix_subclass_reason')] <- fix[b[!is.na(b)], c('subclass', 'reason')]

   plots
}
