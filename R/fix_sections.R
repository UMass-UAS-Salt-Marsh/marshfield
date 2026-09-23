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


   fix <- read.table(file.path(path, 'pars/fix_sections.txt'), sep = '\t', header = TRUE)
   y <- strsplit(fix$start, '-')
   for(i in 1:length(y)) {                                                                                     # For each row in fix_sections,
      seq <- as.numeric(y[[i]][3]):as.numeric(fix$end[i])   # plot number sequence                             #    plot number sequence
      b <- match(paste(y[[i]][1], y[[i]][2], sprintf('%03d', seq), sep = '-'), plots$plot_id)                  #    matching rows in plots
      res <- paste(y[[i]][1], sprintf('%02d', fix$is_section[i]), sprintf('%03d', seq), sep = '-')[!is.na(b)]  #    new plot ids (omitting missing plots)
      b <- b[!is.na(b)]                                                                                        #    drop missing plots in index
      plots$plot_id[b] <- res                                                                                  #    update section in plot id
   }

   plots
}


