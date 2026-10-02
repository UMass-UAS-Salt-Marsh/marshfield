#' Clean up incorrect section numbers
#'
#' Section number errors are generally from a crew not closing the app at the end of a section
#'
#' Parameter file:
#' #' - `fix_sections.txt` Split field days into sections: `start`, `end`, `is_section`, `reason`
#'
#' @param plots Plots data frame
#' @param path Base path
#' @returns Plots data frame
#' @export


fix_sections <- function(plots, path) {


   fix <- read.table(file.path(path, 'pars/fix_sections.txt'), sep = '\t', header = TRUE, quote = '')
   y <- strsplit(fix$start, '-')
   for(i in 1:length(y)) {                                                                                     # For each row in fix_sections,
      seq <- as.numeric(y[[i]][3]):as.numeric(fix$end[i])   # plot number sequence                             #    plot number sequence
      old <- paste(y[[i]][1], y[[i]][2], sprintf('%03d', seq), sep = '-')                                      #    old plot ids
      new <- paste(y[[i]][1], sprintf('%02d', fix$is_section[i]), sprintf('%03d', seq), sep = '-')             #    new plot ids
      b <- match(plots$plot_id, old)                                                                           #    for every row in plots (including duplicate plot ids), index into old
      plots$plot_id[!is.na(b)] <- new[b[!is.na(b)]]                                                            #    update section in plot id
   }

   plots
}
