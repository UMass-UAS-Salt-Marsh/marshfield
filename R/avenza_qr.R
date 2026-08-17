#' Make QR codes for downloading map files in Avenza
#'
#' @param base base path for resulting QR codes
#' @param urls named vector of URLs
#' @importFrom qrcode qr_code
#' @importFrom grDevices png dev.off
#' @export


avenza_qr <- function(base = 'C:/Work/R/marshfield/inst',
                      urls = c(
                         ortho           = 'https://marsh01.ecs.umass.edu/share/uas/uas2026/essex2025.tif',
                         open_space      = 'https://marsh01.ecs.umass.edu/share/uas/uas2026/open_space.zip',
                         target_sections = 'https://marsh01.ecs.umass.edu/share/uas/uas2026/castle_sections.zip',
                         work_areas      = 'https://marsh01.ecs.umass.edu/share/uas/uas2026/work_areas.zip'
                      )) {


   for (nm in names(urls)) {
      png(file.path(base, paste0('qr_', nm, '.png')), width = 600, height = 600)
      plot(qr_code(urls[[nm]]))
      dev.off()
   }

}
