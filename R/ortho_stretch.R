#' Stretch limits for an ortho, from its statistics if they're there
#'
#' `finish_orthos` writes exact full-image histograms into a sidecar `<file>.aux.xml`, as part
#' of the mandatory post-processing run. Reading percentiles from those is instant and exact,
#' while sampling the raster to estimate them takes tens of seconds over the network for a
#' multi-gigabyte ortho -- which was the entire cost of switching images in `gcp` and
#' `view_plots`.
#'
#' Falls back to sampling for files with no statistics (older sites, other projects, and DEMs
#' until those are run through `finish_orthos` too).
#'
#' @param r SpatRaster, already subset to the bands wanted
#' @param probs Lower and upper percentiles
#' @param n Sample size for the fallback
#' @returns List of lo and hi, one per band, and from ('stats' or 'sample')
#' @importFrom terra sources nlyr spatSample
#' @importFrom stats quantile complete.cases
#' @keywords internal
#' @noRd


ortho_stretch <- function(r, probs = c(0.01, 0.99), n = 2000) {

   q <- ortho_stats_quantiles(r, probs)
   if(!is.null(q))
      return(list(lo = q[1, ], hi = q[2, ], from = 'stats'))

   s <- spatSample(r, n, method = 'regular', as.df = TRUE)
   s[s == 65535] <- NA                                                        # MicaSense nodata
   s <- s[complete.cases(s), , drop = FALSE]
   if(nrow(s) == 0)
      return(list(lo = rep(0, nlyr(r)), hi = rep(1, nlyr(r)), from = 'sample'))

   list(lo = sapply(s, quantile, probs[1]), hi = sapply(s, quantile, probs[2]), from = 'sample')
}



#' Read percentiles from a raster's statistics sidecar
#'
#' Parses the histograms GDAL writes to `<file>.aux.xml` and interpolates the percentiles from
#' the cumulative counts. The band order in the sidecar is the file's, so bands are matched by
#' their position in the source file, not by their position in `r`.
#'
#' @param r SpatRaster
#' @param probs Percentiles
#' @returns 2-row matrix (lo, hi) with a column per layer of `r`, or NULL if there are no usable
#'   statistics
#' @importFrom terra sources nlyr
#' @keywords internal
#' @noRd


ortho_stats_quantiles <- function(r, probs) {

   f <- unique(sources(r))
   if(length(f) != 1 || !file.exists(aux <- paste0(f, '.aux.xml')))
      return(NULL)

   x <- tryCatch(readLines(aux, warn = FALSE), error = function(e) NULL)
   if(is.null(x))
      return(NULL)
   x <- paste(x, collapse = '')

   bands <- regmatches(x, gregexpr('<PAMRasterBand band="[0-9]+">.*?</PAMRasterBand>', x))[[1]]
   if(length(bands) == 0)
      return(NULL)

   one <- function(b) {                                                       # percentiles for one band's histogram
      g <- function(tag) {
         m <- regmatches(b, regexpr(paste0('<', tag, '>[^<]*</', tag, '>'), b))
         if(length(m) == 0) NA else as.numeric(gsub('<[^>]*>', '', m))
      }
      lo <- g('HistMin')
      hi <- g('HistMax')
      cnt <- regmatches(b, regexpr('<HistCounts>[^<]*</HistCounts>', b))
      if(is.na(lo) || is.na(hi) || length(cnt) == 0)
         return(NULL)

      cnt <- as.numeric(strsplit(gsub('<[^>]*>', '', cnt), '\\|')[[1]])
      if(length(cnt) < 2 || sum(cnt) == 0)
         return(NULL)

      w <- (hi - lo) / length(cnt)                                            # bucket centers
      mid <- lo + w * (seq_along(cnt) - 0.5)
      cum <- cumsum(cnt) / sum(cnt)
      k <- !duplicated(cum)                                                   # flat runs of the CDF (empty buckets)
      if(sum(k) < 2)
         return(NULL)
      suppressWarnings(approx(cum[k], mid[k], xout = probs, rule = 2)$y)      # interpolate, clamping at the ends
   }

   nm <- names(r)
   src <- ortho_band_names(aux)                                               # band order in the file
   i <- if(is.null(src)) seq_len(nlyr(r)) else match(nm, src)
   if(anyNA(i) || any(i > length(bands)))
      return(NULL)

   q <- lapply(bands[i], one)
   if(any(sapply(q, is.null)))
      return(NULL)

   matrix(unlist(q), nrow = 2, dimnames = list(NULL, nm))
}



#' Band descriptions from a statistics sidecar, in file order
#'
#' @param aux Path to the .aux.xml
#' @returns Character vector of band names, or NULL
#' @keywords internal
#' @noRd


ortho_band_names <- function(aux) {

   x <- tryCatch(paste(readLines(aux, warn = FALSE), collapse = ''), error = function(e) NULL)
   if(is.null(x))
      return(NULL)
   b <- regmatches(x, gregexpr('<PAMRasterBand band="[0-9]+">.*?</PAMRasterBand>', x))[[1]]
   d <- sapply(b, function(z) {
      m <- regmatches(z, regexpr('<Description>[^<]*</Description>', z))
      if(length(m) == 0) NA_character_ else gsub('<[^>]*>', '', m)
   })
   if(all(is.na(d))) NULL else unname(d)
}
