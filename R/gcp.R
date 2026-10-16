#' Assess orthophotos against ground control points, via a web app
#'
#' Steps through the ground control points at a site, showing a clip of an orthophoto or DEM
#' centered on each surveyed GCP position. Double-click where the target actually appears and
#' the XY error is recorded; on DEMs, the Z error is recorded too. Results go to
#' `gcp_review.txt` in the site folder every time something changes, so there's nothing to
#' save.
#'
#' Measurements made earlier, in this session or a previous one, are drawn and displayed when
#' you navigate back to a GCP, so the app can be used to review results as well as collect
#' them.
#'
#' GCPs are read from a GeoPackage (or shapefile) in `<site>/gcp_placement/vector`. This app
#' never writes it: if the raw RTK files in `<site>/gcp_placement/raw` are newer, it says so
#' and leaves converting them to `gather_rtk`. Currently the GCPs are 30 cm white targets
#' placed in the field, but anything RTK'd works, including natural features (boulders and
#' other distinctive permanent things), which have the advantage of being usable
#' retrospectively.
#'
#' Images are whatever is in `<site>/orthos` and `<site>/dem`. micap and panchro products are
#' listed and recorded separately, as they're processed independently. Note that a 30 cm target
#' is only about 4 pixels across on a 7.8 cm micap ortho, and about 7 on a 4.1 cm panchro one,
#' which bounds how precisely a center can be placed no matter how far you zoom in.
#'
#' Coordinate reference systems are checked and anything suspect is reported under the
#' dropdowns, rather than quietly corrected: a NAD83 vs. NAD83(2011) datum mismatch is about a
#' meter in Massachusetts, which is the same size as the registration error being measured. The
#' same goes for DEM heights, which are often ellipsoidal rather than NAVD88 and off by the
#' geoid separation (about 27 m here); see `gcp_vdatum`.
#'
#' `gcp` includes the following controls:
#'
#' - **Project**, **Site**, **Image** select what to assess. Projects come from
#'   `projects.txt`, sites from the project's `sites/sites.txt`, and images from the site's
#'   `orthos` and `dem` folders.
#' - **Navigation buttons** jump to the first, previous, next, or last GCP, and **Next** does
#'   the same as `>`. Arrow keys, Home, and End work too. Skipping a GCP just means navigating
#'   past it; nothing is recorded. GCPs are often moved mid-season, so expect to skip.
#' - **Zoom** sets the width of the clip. The orange cross is the surveyed position; once
#'   you've measured, a cyan cross shows where the target really is, with a line between them.
#' - Double-click the center of the target to record the error. Double-click again to move it;
#'   **Clear** removes the measurement.
#'
#' On a DEM there's nothing to see -- a white target doesn't show up in a surface model -- so Z
#' is probed at whatever position you recorded on the matching orthophoto, and the DEM can't be
#' measured until you've done that.
#'
#' A site is locked while someone is recording, so several people can work on different sites
#' at once. If someone else holds the lock you can look but not record, and **Force unlock** is
#' there for the case where they left the app open and went home.
#'
#' `gcp_review.txt` is tab-delimited with columns `gcp_name`, `image`, `gcp_easting`,
#' `gcp_northing`, `gcp_elevation`, `ortho_easting`, `ortho_northing`, `ortho_elevation`,
#' `xy_error`, `xy_direction`, and `z_error`, one row per GCP per image.
#'
#' @param basepath Path holding `projects.txt` and the project folders
#' @param site_path Template for a site path, with `<basepath>`, `<project>`, and `<site>`
#' @import shiny
#' @importFrom bslib bs_theme card
#' @importFrom shinybusy add_busy_spinner
#' @importFrom sf st_crs st_coordinates st_drop_geometry
#' @importFrom terra extract crs
#' @importFrom stats setNames
#' @export


gcp <- function(basepath = 'U:/projects',
                site_path = '<basepath>/<project>/sites/<site>') {


   projects <- gcp_projects(basepath)
   if(length(projects) == 0)
      stop('No projects found in ', file.path(basepath, 'projects.txt'))

   image_cache <- new.env()                                                       # loaded images, with stretches


   # User interface ---------------------
   ui <- fluidPage(

      title = 'GCP assessment',

      theme = bs_theme(bootswatch = 'cerulean', version = 5),
      tags$head(tags$style(HTML(gcp_css)), tags$script(HTML(gcp_js))),

      div(class = 'container-fluid',
          add_busy_spinner(spin = 'fading-circle', position = 'bottom-right', onstart = FALSE, timeout = 500),

          titlePanel(HTML('<H4>GCP assessment</H4>')),

          fluidRow(

             column(6,
                    card(
                       selectInput('project', HTML('<h6 style="display: inline-block;">Project</h6>'),
                                   choices = projects, selectize = FALSE, width = '100%'),
                       uiOutput('site_select'),
                       uiOutput('image_select')
                    ),
                    uiOutput('messages'),

                    card(
                       textOutput('checkpoint'),
                       span(
                          actionButton('first', '<<'),
                          actionButton('previous', '<'),
                          actionButton('next_', '>'),
                          actionButton('last', '>>')
                       ),
                       div(class = 'gcp-next',
                           actionButton('next2', 'Next', class = 'btn-primary'))
                    ),

                    card(
                       uiOutput('errors'),
                       div(class = 'gcp-clear',
                           actionButton('clear', 'Clear', class = 'btn-sm'))
                    ),

                    card(
                       HTML('<h6 style="display: inline-block;">This image</h6>'),
                       uiOutput('stats')
                    )
             ),

             column(6,
                    imageOutput('image', height = 'auto'),
                    radioButtons('zoom', HTML('<h6 style="display: inline-block;">Zoom</h6>'),
                                 choices = c('1 m' = 1, '2 m' = 2, '5 m' = 5, '20 m' = 20),
                                 selected = 2, inline = TRUE)
             )
          )
      )
   )



   # Server -----------------------------
   server <- function(input, output, session) {

      rv <- reactiveValues(idx = 1, tick = 0, lock_tick = 0)

      st <- reactiveValues(path = NULL, gcp = NULL, names = NULL, xyz = NULL, crs = NULL,
                           ellipsoidal = NULL, images = NULL, msg = character(0),
                           review = NULL, file = NULL)

      the_lock <- NULL                                                            # plain, not reactive: onStop has to
                                                                                  # read it outside any reactive context,
                                                                                  # and rv$lock_tick drives the display
      claim_path <- NULL                                                          # site we have an outstanding claim on
      waited_since <- NULL                                                        # when we started waiting for a claim
      waiting <- reactiveVal(FALSE)                                               # asked another session for the lock
      yielded <- reactiveVal(NULL)                                                # gave our lock up to another session


      # --- site: load GCPs, images, reviews, and take the lock
      load_site <- function(project, site) {

         if(!is.null(claim_path)) {                                               # don't leave a claim on the old site
            gcp_clear_claim(claim_path)
            claim_path <<- NULL
         }
         gcp_unlock(the_lock)
         the_lock <<- NULL
         yielded(NULL)                                                            # lock state is per-site
         waiting(FALSE)

         p <- gcp_site_path(site_path, basepath, project, site)
         st$path <- p
         st$images <- gcp_images(p)

         x <- gcp_read_points(p)
         st$msg <- x$msg
         st$gcp <- x$gcp

         if(is.null(x$gcp)) {
            st$names <- character(0)
            st$xyz <- NULL
            return()
         }

         st$names <- gcp_names(x$gcp)
         st$crs <- st_crs(x$gcp)
         xyz <- st_coordinates(x$gcp)
         if(ncol(xyz) < 3)                                                        # 2D vector file: no heights to compare
            xyz <- cbind(xyz[, 1:2, drop = FALSE], NA_real_)
         st$xyz <- xyz

         d <- st_drop_geometry(x$gcp)
         st$ellipsoidal <- d$ellipsoidal_height                                   # NULL unless the file has it

         st$file <- file.path(p, 'gcp_review.txt')
         st$review <- gcp_read_review(st$file)

         the_lock <<- gcp_lock(p)
         rv$lock_tick <- rv$lock_tick + 1
         rv$idx <- 1
      }


      observeEvent(input$project, {
         s <- gcp_sites(basepath, input$project)
         output$site_select <- renderUI(
            if(length(s) == 0)
               div(class = 'gcp-empty', 'No sites.txt for this project')
            else
               selectInput('site', HTML('<h6 style="display: inline-block;">Site</h6>'),
                           choices = s, selectize = FALSE, width = '100%')
         )
         if(length(s) == 0) {
            st$path <- NULL
            st$gcp <- NULL
            st$images <- NULL
            st$msg <- character(0)
         }
      })


      observeEvent(input$site, load_site(input$project, input$site))


      output$image_select <- renderUI({
         im <- st$images
         if(is.null(im) || nrow(im) == 0)
            return(div(class = 'gcp-empty', 'No orthophotos or DEMs for this site'))
         selectInput('image', HTML('<h6 style="display: inline-block;">Image</h6>'),
                     choices = setNames(im$name, paste0(im$name, ' (', im$type, ')')), selectize = FALSE,
                     width = '100%')
      })


      get_image <- function(name) {                                               # load (and cache) an image
         im <- st$images
         i <- match(name, im$name)
         key <- im$file[i]
         if(is.null(image_cache[[key]]))
            image_cache[[key]] <- gcp_load_image(key, im$type[i])
         image_cache[[key]]
      }


      # --- GCP positions in an image's CRS (orthos may be EPSG:6491 or UTM)
      image_xy <- function(name) {
         o <- get_image(name)
         project_xy(st$xyz[, 1], st$xyz[, 2], st$crs, o$rast)
      }

      img_xy <- reactive({
         req(st$xyz, input$image)
         image_xy(input$image)
      })


      # --- vertical datum for a DEM, inferred and reported
      image_vdatum <- function(name) {
         o <- get_image(name)
         if(o$mode != 'dem')
            return(NULL)
         gcp_vdatum(extract(o$rast, image_xy(name))[, 1], st$xyz[, 3], st$ellipsoidal,
                    st_crs(crs(o$rast)))
      }

      vdatum <- reactive({
         req(st$xyz, input$image)
         image_vdatum(input$image)
      })


      locked <- reactive({                                                        # can't record: someone else holds the
         rv$lock_tick                                                             # lock, or we gave it up to them
         !is.null(yielded()) || (!is.null(the_lock) && is.null(the_lock$lock))
      })


      n_gcp <- reactive(length(st$names))
      cur <- reactive(if(n_gcp() == 0) NA else min(rv$idx, n_gcp()))


      # --- the measurement for the current GCP and image, or NULL
      measurement <- reactive({
         rv$tick
         i <- cur()
         req(!is.na(i), input$image, st$review)
         j <- which(st$review$gcp_name == st$names[i] & st$review$image == input$image)
         if(length(j) == 0) NULL else st$review[j[1], ]
      })


      # --- record a measurement
      record <- function(i, name, e, n) {

         o <- get_image(name)
         im <- st$images
         type <- im$type[match(name, im$name)]

         z <- NA_real_                                                            # Z comes only from DEMs
         zerr <- NA_real_
         if(type == 'dem') {
            v <- image_vdatum(name)
            z <- extract(o$rast, cbind(e, n))[, 1]
            zerr <- z - v$z[i]
         }

         gxy <- image_xy(name)                                                    # error is measured in the image's CRS
         dx <- e - gxy[i, 1]
         dy <- n - gxy[i, 2]

         row <- data.frame(gcp_name = st$names[i], image = name,
                           gcp_easting = st$xyz[i, 1], gcp_northing = st$xyz[i, 2],
                           gcp_elevation = st$xyz[i, 3],
                           ortho_easting = st$xyz[i, 1] + dx, ortho_northing = st$xyz[i, 2] + dy,
                           ortho_elevation = z,
                           xy_error = sqrt(dx ^ 2 + dy ^ 2), xy_direction = gcp_direction(dx, dy),
                           z_error = zerr)

         r <- st$review
         r <- r[!(r$gcp_name == row$gcp_name & r$image == row$image), ]
         st$review <- rbind(r, row)
         save_review()
      }


      save_review <- function() {
         if(!gcp_write_review(st$review, st$file))
            showNotification(paste0('Could not write ', st$file, '. Is it open in another program?'),
                             type = 'error', duration = NULL)
         rv$tick <- rv$tick + 1
      }


      observeEvent(input$image_dblclick, {                                        # --- double-click records a measurement
         e <- input$image_dblclick
         i <- cur()
         req(!is.na(i), input$image)
         if(locked())
            return()
         if(!identical(e$image, input$image) || !identical(e$gcp, st$names[i]))    # stale click, from before navigating
            return()

         im <- st$images
         if(im$type[match(input$image, im$name)] == 'dem') {
            showNotification('Measure this GCP on the orthophoto; a DEM is probed at that position.',
                             type = 'warning')
            return()
         }

         gxy <- img_xy()
         record(i, input$image, gxy[i, 1] + as.numeric(e$dx), gxy[i, 2] + as.numeric(e$dy))
      })


      observeEvent(input$clear, {                                                 # --- clear the measurement
         i <- cur()
         req(!is.na(i), input$image)
         if(locked())
            return()
         st$review <- st$review[!(st$review$gcp_name == st$names[i] & st$review$image == input$image), ]
         save_review()
      })


      # --- a DEM is probed wherever its orthophoto was measured
      dem_source <- reactive({
         req(input$image, st$images)
         im <- st$images
         if(im$type[match(input$image, im$name)] != 'dem')
            return(NULL)
         gcp_pair(input$image, im)
      })


      observe({                                                                   # --- DEM: carry the ortho's position over
         i <- cur()
         req(!is.na(i), input$image, st$review)
         src <- dem_source()
         req(!is.null(src), !is.na(src))
         if(locked())
            return()

         j <- which(st$review$gcp_name == st$names[i] & st$review$image == src)    # measured on the ortho?
         if(length(j) == 0)
            return()
         k <- which(st$review$gcp_name == st$names[i] & st$review$image == input$image)
         if(length(k) > 0)                                                         # already done
            return()

         o <- get_image(input$image)
         e <- project_xy(st$review$ortho_easting[j[1]], st$review$ortho_northing[j[1]], st$crs, o$rast)
         isolate(record(i, input$image, e[1, 1], e[1, 2]))
      })


      output$image <- renderImage({                                               # --- image clip
         i <- cur()
         req(!is.na(i), input$image, input$zoom)
         o <- get_image(input$image)
         xy <- img_xy()
         m <- measurement()

         meas <- NULL
         if(!is.null(m) && !is.na(m$ortho_easting)) {
            p <- project_xy(m$ortho_easting, m$ortho_northing, st$crs, o$rast)
            meas <- c(p[1, 1], p[1, 2])
         }

         png <- tempfile(fileext = '.png')
         gcp_render(o, xy[i, 1], xy[i, 2], as.numeric(input$zoom), png, measured = meas)
         list(src = png, contentType = 'image/png', width = '100%', alt = st$names[i],
              'data-gcp' = st$names[i], 'data-image' = input$image, 'data-width' = input$zoom)
      }, deleteFile = TRUE)


      output$checkpoint <- renderText({
         if(n_gcp() == 0) 'No GCPs' else paste0('Checkpoint ', cur(), ' of ', n_gcp(), ': ', st$names[cur()])
      })


      output$stats <- renderUI({                                                  # --- summary over this image's GCPs
         rv$tick
         req(st$review, input$image)
         s <- gcp_stats(st$review[st$review$image == input$image, ], n_gcp(),
                        vdatum = if(is.null(v <- vdatum())) NULL else v$datum)

         row <- function(label, value, note = '')
            tags$tr(tags$td(label), tags$td(class = 'val', value), tags$td(class = 'note', note))

         if(s$n == 0 || is.null(s$bias))
            return(tags$table(class = 'gcp-errors', row('Reviewed', paste0('0 of ', s$n_gcp))))

         tags$table(class = 'gcp-errors',
            row('Reviewed', paste0(s$n, ' of ', s$n_gcp)),
            row('Bias', sprintf('%.1f cm', 100 * s$bias),
                paste0(s$bias_dir, if(!is.na(s$bias_se)) sprintf('  (SE %.1f cm)', 100 * s$bias_se) else '')),
            row('RMSE', sprintf('%.1f cm', 100 * s$rmse)),
            row('Range', sprintf('%.1f to %.1f cm', 100 * s$lo, 100 * s$hi)),
            if(!is.null(s$z_bias))
               row('Z bias', sprintf('%.1f cm', 100 * s$z_bias),
                   if(!is.na(s$z_se)) sprintf('(SE %.1f cm)', 100 * s$z_se) else ''),
            if(!is.null(s$z_rmse))
               row('Z RMSE', sprintf('%.1f cm', 100 * s$z_rmse)),
            if(!is.null(s$z_lo))
               row('Z range', sprintf('%.1f to %.1f cm', 100 * s$z_lo, 100 * s$z_hi))
         )
      })


      output$errors <- renderUI({                                                 # --- XY and Z error
         i <- cur()
         if(is.na(i))
            return(NULL)
         m <- measurement()
         src <- dem_source()

         if(is.null(m)) {
            if(!is.null(src))
               return(div(class = 'gcp-hint',
                          if(is.na(src)) 'This DEM has no matching orthophoto, so there is no position to probe.'
                          else paste0('Measure this GCP on ', src, ' first.')))
            return(div(class = 'gcp-hint', 'Double-click the center of the target'))
         }

         v <- vdatum()
         tags$table(class = 'gcp-errors',
            tags$tr(tags$td('XY error'),
                    tags$td(class = 'val', sprintf('%.1f cm', 100 * m$xy_error)),
                    tags$td(m$xy_direction)),
            if(!is.na(m$z_error))
               tags$tr(tags$td('Z error'),
                       tags$td(class = 'val', sprintf('%.1f cm', 100 * m$z_error)),
                       tags$td(if(is.null(v)) '' else paste0('vs. ', v$datum)))
         )
      })


      output$messages <- renderUI({                                               # --- CRS, staleness, and lock messages
         rv$lock_tick
         msg <- st$msg

         if(!is.null(input$image) && !is.null(st$gcp) && nrow(st$images) > 0) {
            o <- get_image(input$image)
            im <- st$images
            msg <- c(msg, gcp_check_crs(st$crs, o$rast, im$type[match(input$image, im$name)]))
            v <- vdatum()
            if(!is.null(v))
               msg <- c(msg, v$msg)
         }

         y <- yielded()
         lk <- if(!is.null(y))                                                    # we gave the lock up to someone
            paste0('Lock taken by ', y$user, ' on ', y$host, '; viewing only')
         else if(waiting())
            'Waiting for the other session to give up the lock...'
         else if(locked())
            gcp_lock_who(the_lock$info)
         else
            NULL

         if(length(msg) == 0 && is.null(lk))
            return(NULL)

         div(
            if(!is.null(lk))
               div(class = 'gcp-locked', lk,
                   if(is.null(y) && !waiting())                                   # nothing to force if we already asked
                      actionButton('force', 'Force unlock', class = 'btn-sm btn-warning')),
            lapply(msg, function(m) div(class = 'gcp-warn', m))
         )
      })


      observeEvent(input$force, {                                                 # --- force unlock
         showModal(modalDialog(
            title = 'Force unlock?',
            p('Another session holds the lock on this site. Normally that means someone else is ',
              'recording measurements right now, and you should leave it alone.'),
            p('Force unlock is for when someone left the app open and went home. If their ',
              'session has ended, you get the lock straight away. If it is still running, they ',
              'are asked to give it up, which takes a few seconds, and their app drops to ',
              'viewing only so nothing they do can collide with you.'),
            p('Their measurements are saved as they go, so nothing is lost either way.'),
            footer = tagList(modalButton('Cancel'),
                             actionButton('force_ok', 'Force unlock', class = 'btn-warning'))
         ))
      })


      observeEvent(input$force_ok, {
         removeModal()
         req(st$path)
         lk <- gcp_force_unlock(st$path)
         the_lock <<- lk
         rv$lock_tick <- rv$lock_tick + 1
         st$review <- gcp_read_review(st$file)                                    # whatever they recorded before leaving
         rv$tick <- rv$tick + 1

         if(isTRUE(lk$forced)) {
            gcp_clear_claim(st$path)
            claim_path <<- NULL
            showNotification('Lock taken; you can record now.', type = 'message')
         }
         else {                                                                   # held by a live session: ask it to yield
            gcp_claim(st$path)
            claim_path <<- st$path
            waited_since <<- Sys.time()
            waiting(TRUE)
            showNotification('The other session is still running, so it has been asked to give up the lock. This takes a few seconds.',
                             type = 'warning', duration = 10)
         }
      })


      # --- One poll handles both sides of a handover. It runs for the life of the session rather
      #     than being armed and disarmed, because `the_lock` is a plain variable and so can't be
      #     a reactive dependency: an observer gated on it would never re-run when it changed.
      observe({
         invalidateLater(2000)
         p <- isolate(st$path)
         req(p)

         if(!is.null(the_lock) && !is.null(the_lock$lock)) {                      # we hold it: yield if asked
            cl <- gcp_check_claim(p)
            req(!is.null(cl))
            gcp_unlock(the_lock)
            the_lock <<- NULL
            yielded(cl)
            rv$lock_tick <- rv$lock_tick + 1
            showNotification(paste0(cl$user, ' on ', cl$host, ' has asked for the lock on this site, so it has been ',
                                    'given up. Your measurements are saved; you are now viewing only.'),
                             type = 'warning', duration = NULL)
            return()
         }

         if(isolate(waiting())) {                                                 # we asked for it: take it when free
            lk <- gcp_lock(p)

            if(is.null(lk$lock)) {
               if(difftime(Sys.time(), waited_since, units = 'secs') > 30) {      # don't wait forever
                  waiting(FALSE)
                  gcp_clear_claim(p)
                  claim_path <<- NULL
                  rv$lock_tick <- rv$lock_tick + 1
                  showNotification(paste0('The other session hasn\'t given up the lock. It may be on a machine that ',
                                          'has gone to sleep. Try again in a few minutes, or work on another site.'),
                                   type = 'error', duration = NULL)
               }
               return()
            }

            the_lock <<- lk
            waiting(FALSE)
            gcp_clear_claim(p)
            claim_path <<- NULL
            st$review <- gcp_read_review(isolate(st$file))
            rv$lock_tick <- rv$lock_tick + 1
            rv$tick <- rv$tick + 1
            showNotification('Lock released by the other session; you can record now.', type = 'message')
         }
      })


      observeEvent(input$first, rv$idx <- 1)                                      # --- navigation
      observeEvent(input$previous, rv$idx <- max(rv$idx - 1, 1))
      observeEvent(input$next_, rv$idx <- min(rv$idx + 1, max(n_gcp(), 1)))
      observeEvent(input$next2, rv$idx <- min(rv$idx + 1, max(n_gcp(), 1)))
      observeEvent(input$last, rv$idx <- max(n_gcp(), 1))
      observeEvent(input$key_nav, {
         n <- max(n_gcp(), 1)
         rv$idx <- switch(input$key_nav,
                          first = 1,
                          previous = max(rv$idx - 1, 1),
                          'next' = min(rv$idx + 1, n),
                          last = n)
      })


      onStop(function() {                                                          # plain variables only: onStop runs
         if(!is.null(claim_path))                                                  # outside any reactive context
            gcp_clear_claim(claim_path)                                            # don't leave a claim nobody will collect
         gcp_unlock(the_lock)                                                      # don't leave the site locked
      })
   }


   shinyApp(ui = ui, server = server)
}



gcp_css <- '
#image img { width: 100%; height: auto; max-height: 80vh; object-fit: contain; cursor: crosshair;
   user-select: none; }
.card { margin-bottom: 6px; }
.card-body { padding: 6px 12px; gap: 4px; }
.card h6 { margin-bottom: 2px; }
.card .shiny-input-container:not(.shiny-input-container-inline) { margin-bottom: 4px; }
.card select { width: 100%; max-width: 100%; }                 /* image names are long; let them use the pane */
.gcp-next { margin-top: 6px; }
.gcp-clear { margin-top: 4px; }
.gcp-errors td { padding: 0 14px 2px 0; font-size: 1.1em; }
.gcp-errors td.val { text-align: right; font-weight: bold; font-variant-numeric: tabular-nums; }
.gcp-errors td.note { padding-left: 6px; }
.gcp-hint { color: #777; font-style: italic; }
.gcp-warn { color: #a05000; background: #fff6e5; border-left: 3px solid #e8a33d;
   padding: 4px 8px; margin-bottom: 4px; font-size: 0.9em; }
.gcp-locked { color: #8a0000; background: #ffecec; border-left: 3px solid #c00;
   padding: 4px 8px; margin-bottom: 4px; font-size: 0.9em; display: flex;
   justify-content: space-between; align-items: center; gap: 8px; }
.gcp-empty { color: #777; font-style: italic; padding: 4px 0; }
'



# The double-click carries the GCP and image it was made on, so a click cannot land on the
# wrong one if you navigate while it is in flight.
gcp_js <- '
$(document).on("dblclick", "#image img", function(e) {
   var r = this.getBoundingClientRect();
   var s = Math.min(r.width / this.naturalWidth, r.height / this.naturalHeight);   // may be letterboxed
   var w = this.naturalWidth * s, h = this.naturalHeight * s;
   var fx = (e.clientX - r.left - (r.width - w) / 2) / w;
   var fy = (e.clientY - r.top - (r.height - h) / 2) / h;
   if (fx < 0 || fx > 1 || fy < 0 || fy > 1) return;
   var width = Number(this.dataset.width);
   Shiny.setInputValue("image_dblclick", {gcp: this.dataset.gcp, image: this.dataset.image,
      dx: (fx - 0.5) * width, dy: (0.5 - fy) * width}, {priority: "event"});
});

$(document).on("keydown", function(e) {
   if (e.key === "Escape") { e.target.blur(); return; }
   if ($(e.target).is("input[type=text], input[type=number], textarea, select")) return;
   var k = {ArrowRight: "next", ArrowLeft: "previous", Home: "first", End: "last"}[e.key];
   if (k) {
      e.preventDefault();
      Shiny.setInputValue("key_nav", k, {priority: "event"});
   }
});
'
