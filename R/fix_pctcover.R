#' Fix errors in percent cover
#'
#' Parameter file:
#' - `fix_pct_cover.txt` Changes to percent cover fields: `plot_id`, `species_code`, `pct_cover`, `reason`
#'
#' @param pct_cover Percent cover data frame
#' @param path Base path
#' @returns Percent cover data frame
#' @export


fix_pctcover <- function() {


   x <- read.table(file.path(path, 'pars/fix_pct_cover.txt'), sep = '\t', header = TRUE, quote = '')
   key_pc <- paste(pct_cover$plot_id, pct_cover$species_code)
   key_x <- paste(x$plot_id, x$species_code)

   new <- x[, c('plot_id', 'species_code', 'pct_cover')]
   new$species <- pct_cover$species[match(new$species_code, pct_cover$species_code)]   # look up names

   pct_cover <- rbind(pct_cover[!key_pc %in% key_x, ], new[, names(pct_cover)])
   pct_cover <- pct_cover[order(pct_cover$plot_id, pct_cover$species_code), ]
   rownames(pct_cover) <- NULL

   pct_cover
}
