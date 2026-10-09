#' Check for agreement in plot id and RTK id sequences
#'
#' Goal is to find potential errors where sequences diverge, possibly from skipped RTK. Each
#' tablet/day/section is treated separately.
#'
#' Within each tablet/day/section, RTK point numbers are expected to increment with plot
#' numbers, so `rtk point - plot number` should be constant. Gaps in plot numbers are fine as
#' long as the RTK points skip by the same amount. The expected offset is set by the first plot
#' in the tablet/day/section. When the RTK unit/date changes partway through, the expected offset
#' for the new RTK run is taken as its lowest point number minus its lowest plot number (i.e., we
#' assume the first RTK point from the new unit went with the first plot).
#'
#' The first plot in each tablet/day/section is always FALSE. A change in RTK unit/date partway
#' through a tablet/day/section is TRUE, as is any plot whose RTK point is out of sequence.
#'
#' Plots without a valid RTK id are skipped (and are FALSE); they're reported by `check_plot_rtkids`.
#'
#' Plots must be sorted in plot id order (see `sort_plots`).
#'
#' @param plots Plots data frame, sorted by `plot_id`
#' @returns Plots data frame with new column, `rtk_nonseq`, TRUE if `rtk_id` is out of sequence
#' @export


rtk_sequence <- function(plots) {


   ok <- valid_rtkids(plots$rtk_id)             # skip plots without a valid RTK id
   all_plots <- plots
   plots <- plots[ok, ]

   y <- strsplit(toupper(plots$plot_id), '-')
   y1 <- sapply(y, '[[', 1)                     # part 1: tablet and date
   y2 <- sapply(y, '[[', 2)                     # part 2: section
   y3 <- sapply(y, '[[', 3)                     # part 3: plot number

   r <- strsplit(toupper(plots$rtk_id), '-')
   r1 <- sapply(r, '[[', 1)                     # part 1: RTK unit and date
   r2 <- sapply(r, '[[', 2)                     # part 2: RTK point number


   tds <- paste(y1, y2, sep = '-')
   pn <- as.numeric(y3)
   new <- tds != c('xxx', tds[-length(tds)])    # starts of new plot sequences

   new_rtk <- r1 != c('xxx', r1[-length(r1)])   # starts of new RTK sequences
   rn <- as.numeric(r2)

   seg <- cumsum(new | new_rtk)                 # runs with the same tablet/day/section and RTK unit/date
   first <- which(new | new_rtk)                # first row of each run

   offset <- rep(NA, max(seg))
   for(i in seq_along(first)) {
      s <- seg == i
      if(new[first[i]])                         # run starts a tablet/day/section: anchor on first plot
         offset[i] <- rn[first[i]] - pn[first[i]]
      else                                      # RTK changed midstream: anchor on lowest RTK point
         offset[i] <- min(rn[s]) - min(pn[s])
   }

   all_plots$rtk_nonseq <- FALSE
   all_plots$rtk_nonseq[ok] <- !new & (new_rtk | (rn - pn) != offset[seg])
   all_plots
}
