#' Download plot photos from the server
#'
#' @param urls Vector of photo URLs
#' @param dest_dir Directory to save photos to
#' @importFrom curl curl_download
#' @export


get_photos <- function(urls, dest_dir = 'C:/Work/saltmarsh/data/uas2026/field/photos/downloads') {


   dests <- file.path(dest_dir, basename(urls))

   # strip query strings if the URLs have them (e.g. ...jpg?token=abc)
   dests <- file.path(dest_dir, sub("\\?.*$", "", basename(urls)))

   if(any(duplicated(dests)))
      stop('There are duplicate URLs!')

   new <- which(!file.exists(dests))            # resumable: only get photos we don't already have
   message('Downloading ', length(new), ' new photos from the server (', length(dests) - length(new), ' already downloaded)...')

   for (i in new) {
      tryCatch(
         curl::curl_download(urls[i], dests[i], mode = "wb"),
         error = function(e) message("Failed: ", urls[i], " - ", conditionMessage(e))
      )
   }

   message('Photos downloaded')
}
