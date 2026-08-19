#' Download plot photos from the server
#'
#' @param urls Vector of photo URLs
#' @param dest_dir Directory to save photos to
#' @importFrom curl curl_download
#' @export


get_photos <- function(urls, dest_dir = 'C:/Work/saltmarsh/data/uas2026/field/photos') {


   dests <- file.path(dest_dir, basename(urls))

   # strip query strings if the URLs have them (e.g. ...jpg?token=abc)
   dests <- file.path(dest_dir, sub("\\?.*$", "", basename(urls)))

   if(any(duplicated(dests)))
      stop('There are duplicate URLs!')

   message('Downloading ', length(dests), ' photos from the server...')

   for (i in seq_along(urls)) {
      if (file.exists(dests[i])) next          # resumable
      tryCatch(
         curl::curl_download(urls[i], dests[i], mode = "wb"),
         error = function(e) message("Failed: ", urls[i], " — ", conditionMessage(e))
      )
   }

   message('Photos downloaded')
}
