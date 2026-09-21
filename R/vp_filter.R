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
#' photo), `rotated` (photo has been rotated), and `offset` (has an ortho offset recorded for
#' any ortho). `subclass=6` selects plots of subclass 6, and `subclass=6|7` plots of
#' subclass 6 or 7 (no spaces). `dz>0.8`, `dz<-0.5`, and `|dz|>0.8` select plots by DEM - RTK
#' height (in m). Any other word matches plots with that word in their review keywords.
#'
#' @param plots Plots data frame
#' @param review Review table, aligned with plots
#' @param site Selected site
#' @param pattern Plot filter
#' @param keywords Keyword filter
#' @param has_offset Logical vector, aligned with plots: does the plot have an ortho offset?
#' @param dz DEM - RTK height (m), aligned with plots (NA if unavailable)
#' @returns Vector of selected rows in plots
#' @keywords internal
#' @noRd


vp_filter <- function(plots, review, site, pattern, keywords, has_offset = rep(FALSE, nrow(plots)),
                      dz = rep(NA_real_, nrow(plots))) {

   sel <- plots$site == site

   pats <- strsplit(toupper(trimws(pattern)), '[ ,]+')[[1]]
   if(length(pats) > 0) {
      pats <- gsub('([.+^$|(){}\\[\\]\\\\])', '\\\\\\1', pats, perl = TRUE)                      # escape regex characters, so stray ones match literally
      rx <- paste0('^', gsub('?', '.', gsub('*', '.*', pats, fixed = TRUE), fixed = TRUE)) # wildcards; trailing * is implied
      sel <- sel & grepl(paste0('(', rx, ')', collapse = '|'), toupper(plots$plot_id))
   }

   words <- strsplit(tolower(trimws(keywords)), '[ ,]+')[[1]]
   kw <- strsplit(tolower(review$keywords), '[ ,;]+')
   for(w in words) {
      neg <- startsWith(w, '!')
      w <- sub('^!', '', w)
      if(w == '')
         next
      if(startsWith(w, 'subclass=')) {                                                # subclass=6, or subclass=6|7 for either
         vals <- suppressWarnings(as.numeric(strsplit(sub('^subclass=', '', w), '|', fixed = TRUE)[[1]]))
         v <- plots$subclass %in% vals[!is.na(vals)]
      }
      else if(grepl('^(\\|dz\\||dz)[<>]', w)) {                                        # dz>0.8, dz<-0.5, |dz|>0.8 (m)
         t <- suppressWarnings(as.numeric(sub('^[^<>]*[<>]', '', w)))
         z <- if(startsWith(w, '|')) abs(dz) else dz
         v <- !is.na(z) & !is.na(t) & (if(grepl('>', w, fixed = TRUE)) z > t else z < t)
      }
      else v <- switch(w,
                  reviewed = review$reviewed,
                  problems = review$problems,
                  rejected = review$rejected,
                  comments = review$comments != '',
                  keywords = review$keywords != '',
                  photo = plots$photo,
                  rotated = !is.na(review$rotation),
                  offset = has_offset,
                  sapply(kw, function(k) w %in% k))
      sel <- sel & (if(neg) !v else v)
   }

   which(sel)
}
