# get percent of plots with each species present
# quickie for Ethan, 10 Sep 2026


if(FALSE) {
   x <- as.data.frame(table(pct_cover$species_scientific_name))
   names(x) <- c('species', 'freq')
   x <- x[x$species != '',]

   total <- length(unique(pct_cover$plot_id))
   x$pct <- round(x$freq / total * 100, 1)
   z <- x[order(x$freq, decreasing = TRUE), c('species', 'pct')]
   print(z, row.names = FALSE)
}
