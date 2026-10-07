#' Fix incorrect species names in data file (bogus specific for genera)
#'
#' Parameter file:
#' - `fix_species.txt` Change genera incorrectly listed as species by app in percent cover
#'
#' @param pct_cover Percent cover data frame
#' @param path Base path
#' @returns Percent cover data frame
#' @export


fix_species <- function(pct_cover, path) {


   sp <- read.table(file.path(path, 'pars/fix_species.txt'), sep = '\t', header = TRUE, quote = '')
   b <- match(pct_cover$species, sp$old)
   pct_cover$species[!is.na(b)] <- sp$new[b[!is.na(b)]]

   pct_cover
}
