#' Drop identified bad plots
#'
#' This is for plots that are truly bad, identified in `bad_plots.txt`. These
#' plots are removed from the final database, rather than being flagged.
#'
#' Parameter file:
#' - `bad_plots.txt` `plot_id`, `confirmed`, `bad_plot_reason`
#'
#' @param plots Plots data frame
#' @param path Base path
#' @returns Plots data frame
#' @export


drop_bad_plots <- function(plots, path) {


   fix <- read.table(file.path(path, 'pars/bad_plots.txt'), sep = '\t', header = TRUE, quote = '')
   i <- match(fix$plot_id, plots$plot_id)
   i[is.na(i)] <- FALSE
   b <- seq_len(nrow(plots)) %in% i

   bad <- unique(plots$plot_id[b])
   cat(paste0('Dropping ', length(bad), ' bad plots:'), sep = ' ')
   print(bad)


   bbb<<-b

   plots <- plots[!b, ]                         # simply drop all of these plots--they're bad news!

   plots
}
