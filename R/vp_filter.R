#' Select plots for view_plots using plot id patterns and keywords
#'
#' **Plot filter**: one or more wildcard patterns (separated by spaces or commas; a plot matching
#' any of them is selected). `*` matches anything, `?` matches any single character, and a
#' trailing `*` is implied, so `B` is all plots on the blue tablet, `?A04` is all plots on
#' August 4, and `*-03-` is all plots in section 3. Case doesn't matter.
#'
#' **Keyword filter**: one or more words (separated by spaces or commas); plots must match all
#' of them. Precede a word with `!` to negate it. Reserved words are `reviewed`, `problems`,
#' `rejected`, `comments` (has comments), `keywords` (has any keywords), `photo` (has a
#' photo), and `rotated` (photo has been rotated). Any other word matches plots with that
#' word in their review keywords.
#'
#' @param plots Plots data frame
#' @param review Review table, aligned with plots
#' @param site Selected site
#' @param pattern Plot filter
#' @param keywords Keyword filter
#' @returns Vector of selected rows in plots
#' @importFrom utils glob2rx
#' @keywords internal
#' @noRd


vp_filter <- function(plots, review, site, pattern, keywords) {

   sel <- plots$site == site

   pats <- strsplit(toupper(trimws(pattern)), '[ ,]+')[[1]]
   if(length(pats) > 0) {
      pats <- ifelse(endsWith(pats, '*'), pats, paste0(pats, '*'))
      rx <- paste0('(', sapply(pats, glob2rx), ')', collapse = '|')
      sel <- sel & grepl(rx, toupper(plots$plot_id))
   }

   words <- strsplit(tolower(trimws(keywords)), '[ ,]+')[[1]]
   kw <- strsplit(tolower(review$keywords), '[ ,;]+')
   for(w in words) {
      neg <- startsWith(w, '!')
      w <- sub('^!', '', w)
      if(w == '')
         next
      v <- switch(w,
                  reviewed = review$reviewed,
                  problems = review$problems,
                  rejected = review$rejected,
                  comments = review$comments != '',
                  keywords = review$keywords != '',
                  photo = plots$photo,
                  rotated = !is.na(review$rotation),
                  sapply(kw, function(k) w %in% k))
      sel <- sel & (if(neg) !v else v)
   }

   which(sel)
}
