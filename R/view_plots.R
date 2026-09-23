#' Screen field plots, comparing field photos with orthoimages, via a web app
#'
#' Steps through field plots, displaying the field photo for each plot on the left and a clip
#' of an orthoimage or DEM centered on the plot on the right, with plot data in the center.
#' Used to check RTK positions (a missed RTK shot shows up as subsequent plots being off by
#' one) and to look for image anomalies. Reviews (reviewed, problems, rejected, keywords, and
#' comments) are written to `pars/review.txt` every time something changes, so there's no
#' need to save.
#'
#' Plot data come from `prelim/plots.gpkg` and `prelim/pct_cover.txt`, photos from
#' `photos/plots/<plot_id>.jpg`, and the list of orthos for each site from `pars/orthos.txt`.
#' `orthos.txt` is a tab-delimited table with columns `site`, `name` (displayed in the image
#' selector), `type` (`rgb` for orthophotos or `dem` for DEMs, which are displayed as shaded
#' relief), `file` (relative to `ortho_path`, or a full path), and optionally `vdatum` for DEMs
#' (`navd88`, the default, or `ellipsoidal`, in which case RTK ellipsoidal heights are read
#' from `prelim/everything.gpkg`). DEM - RTK height at plot center is displayed below the ortho
#' for each DEM at the site. If `plots` doesn't have a
#' `site` column, all plots are assigned to the first site in `orthos.txt`.
#'
#' **TEMPORARY:** plot centers are reprojected on the fly into each ortho's CRS (see
#' `vp_temp_reproject`), because the July 2026 orthos are in NAD83(1986) rather than
#' NAD83(2011). Remove this once orthos are delivered in EPSG:6491.
#'
#' `view_plots` includes the following controls:
#'
#' - **Site** select the site. The number of plots and percent reviewed are displayed.
#' - **Plot filter** one or more wildcard patterns separated by spaces or commas (plots
#'   matching any pattern are selected). `*` matches anything, `?` matches any single
#'   character, and a trailing `*` is implied. Plot ids are
#'   `<tablet><month><day>-<section>-<plot>`, so `B` selects all plots on the blue tablet,
#'   `BA04` all blue tablet plots on August 4, `?A04` all plots on August 4, `PJ21-01` all
#'   pink tablet plots on July 21 in section 1, and `*-03-` all plots in section 3.
#' - **Keyword filter** one or more words, all of which must match. Precede a word with `!` to
#'   negate it. Reserved words are `reviewed`, `problems`, `rejected`, `comments` (has comments),
#'   `keywords` (has any keywords), `photo` (plot has a photo), `notes` (plot has notes), `rotated`
#'   (photo has #'   been rotated), and `offset` (has an ortho offset recorded). `subclass=6` selects
#'   subclass 6, and `subclass=6|7` subclass 6 or 7 (no spaces around `=`). `dz>0.8`, `dz<-0.5`, and
#'   `|dz|>0.8` select plots by DEM - RTK height, in m (using the selected image if it's a DEM,
#'   otherwise the site's first DEM). Large differences can flag RTK errors. Any other word matches
#'   plots with that word in their review keywords. For example, `!reviewed` shows plots that haven't
#'   been reviewed, and `problems macroalgae` shows plots flagged with problems that have the keyword
#'   macroalgae. Changes to reviews don't hide the current plot until the filter is changed.
#' - **Navigation buttons** jump to the first, previous, next, or last selected plot. You can
#'   also use the left and right arrow keys, Home, and End (when you're not typing in a text
#'   field).
#' - **Review** check **Reviewed** once you've reviewed a plot (percent reviewed is based on
#'   this), **Problems** to flag problems, and **Rejected** for plots that shouldn't be used.
#'   **Keywords** takes words separated by spaces or commas, which can be used in the keyword
#'   filter. **Comments** can be anything.
#' - **Rotation** field photos aren't oriented, so you can rotate them to match the ortho.
#'   Double-click the orange bead at plot center to center the photo on it, then use the
#'   slider (or type degrees) to rotate the photo around plot center. Once you've clicked on
#'   the slider, arrow keys fine-tune the rotation; press Escape (or click elsewhere) to get
#'   back to navigating with arrow keys. **Clear** resets rotation and center.
#' - **Image** selects the ortho or DEM to display for this site.
#' - **Zoom** sets the width of the ortho clip. The white circle is the plot perimeter (0.7 m
#'   radius) and the orange dot is plot center.
#' - **Offset** to measure how far off a plot is on the ortho, double-click where the plot
#'   center actually appears on the ortho image (the 2 m zoom gives the most precise
#'   placement). The offset from the drawn plot center (`dx`, `dy`, in m east and north) is
#'   recorded in `pars/offsets.txt` for this plot and ortho, and the actual location is drawn
#'   as a cyan cross and dashed circle. Double-click again to move it; **Clear offset** removes
#'   it. Use `offset_summary` to summarize offsets (systematic vs. random vs. trending across
#'   the site).
#' - **Exit** exits the app (reviews are already saved).
#'
#' `review.txt` is tab-delimited with columns `plot_id`, `reviewed`, `problems`, `rejected`,
#' `keywords`, `comments` (newlines are stored as `\n`), `rotation` (degrees clockwise the photo
#' is rotated to be north-up, so the top of the unrotated photo faces `-rotation`), and
#' `center_x` and `center_y` (plot center, as fractions of the width and height of the photo
#' from the top left). Only plots with something entered are included. Rows for plots that are
#' no longer in `plots` (e.g., dropped plots) are kept.
#'
#' @param path Path to field data
#' @param ortho_path Path to orthoimages and DEMs
#' @param classes Path to classes table, used for subclass names
#' @import shiny
#' @importFrom bslib bs_theme card
#' @importFrom shinybusy add_busy_spinner
#' @importFrom sf st_read st_coordinates st_drop_geometry st_crs
#' @importFrom terra extract
#' @importFrom utils read.table
#' @importFrom stats setNames
#' @export


view_plots <- function(path = 'C:/Work/saltmarsh/data/uas2026/field',
                       ortho_path = 'C:/Work/saltmarsh/data/uas2026/orthos',
                       classes = 'C:/Work/saltmarsh/pars/classes.txt') {


   plots <- st_read(file.path(path, 'prelim/plots.gpkg'), quiet = TRUE)
   plot_crs <- st_crs(plots)
   xy <- st_coordinates(plots)
   plots <- st_drop_geometry(plots)
   plots$easting <- xy[, 1]
   plots$northing <- xy[, 2]
   plots$elevation <- xy[, 3]                                                             # NAVD88

   cl <- read.table(classes, sep = '\t', header = TRUE, quote = '', comment.char = '')    # subclass names
   plots$subclass_name <- cl$subclass_name[match(plots$subclass, cl$subclass)]

   orthos <- read.table(file.path(path, 'pars/orthos.txt'), sep = '\t', header = TRUE, quote = '',
                        comment.char = '', fill = TRUE)                                   # fill, as optional vdatum may be blank
   absolute <- grepl('^([A-Za-z]:)?[/\\\\]', orthos$file)
   orthos$file[!absolute] <- file.path(ortho_path, orthos$file[!absolute])
   if(is.null(orthos$vdatum))
      orthos$vdatum <- ''
   orthos$vdatum[is.na(orthos$vdatum) | orthos$vdatum == ''] <- 'navd88'
   sites <- unique(orthos$site)
   ellipsoidal <- NULL                                                                    # RTK ellipsoidal heights, read if needed
   if(is.null(plots$site))
      plots$site <- sites[1]

   cover <- read.table(file.path(path, 'prelim/pct_cover.txt'), sep = '\t', header = TRUE, quote = '',
                       comment.char = '', na.strings = c('', 'NA'))

   photo_dir <- file.path(path, 'photos/plots')
   plots$photo <- file.exists(file.path(photo_dir, paste0(plots$plot_id, '.jpg')))
   addResourcePath('vp_photos', photo_dir)

   review_file <- file.path(path, 'pars/review.txt')
   x <- vp_read_review(review_file, plots$plot_id)
   review <- x$review                                                                     # modified by the server
   extra <- x$extra

   offsets_file <- file.path(path, 'pars/offsets.txt')
   offsets <- vp_read_offsets(offsets_file)                                               # modified by the server

   ortho_cache <- new.env()                                                               # loaded orthos, with stretches



   # User interface ---------------------
   ui <- fluidPage(

      title = 'Plot viewer',

      theme = bs_theme(bootswatch = 'cerulean', version = 5),
      tags$head(tags$style(HTML(vp_css)), tags$script(HTML(vp_js))),

      div(class = 'container-fluid',
          add_busy_spinner(spin = 'fading-circle', position = 'bottom-right', onstart = FALSE, timeout = 500),

          fluidRow(
             class = 'fullheight',

             column(5, class = 'col-fullheight',
                    div(id = 'photo_wrap', uiOutput('photo'), div(id = 'photo_mark')),
                    div(class = 'vp-rotate',
                        tags$label('Rotation', `for` = 'rot_slider'),
                        tags$input(id = 'rot_slider', type = 'range', min = -180, max = 180, step = 1, value = 0),
                        tags$input(id = 'rot_number', type = 'number', min = -180, max = 180, step = 1, value = 0),
                        tags$button(id = 'rot_clear', class = 'btn btn-default btn-sm', 'Clear'),
                        tags$span(class = 'vp-hint', 'Double-click the orange bead to center')
                    )
             ),

             column(3, class = 'col-fullheight',

                    titlePanel(HTML('<H4>Plot viewer</H4>')),

                    card(
                       selectInput('site', label = HTML('<h5 style="display: inline-block;">Site</h5>'),
                                   choices = sites, selectize = FALSE),
                       textOutput('site_info')
                    ),

                    card(
                       HTML('<h5 style="display: inline-block;">Navigate</h5>'),
                       textOutput('plot_no'),
                       vp_clearable(textInput('filter', HTML('<h6 style="display: inline-block;">Plot filter</h6>'), value = '',
                                              width = '100%', placeholder = 'e.g., BJ21  ?A04  *-03-')),
                       vp_clearable(textInput('keyfilter', HTML('<h6 style="display: inline-block;">Keyword filter</h6>'), value = '',
                                              width = '100%', placeholder = 'e.g., !reviewed  subclass=6')),
                       span(
                          actionButton('first', '<<'),
                          actionButton('previous', '<'),
                          actionButton('next_', '>'),
                          actionButton('last', '>>')
                       )
                    ),

                    card(
                       HTML('<h5 style="display: inline-block;">Review</h5>'),
                       div(id = 'review_fields',
                           div(class = 'vp-checks',
                               checkboxInput('reviewed', 'Reviewed'),
                               checkboxInput('problems', 'Problems'),
                               checkboxInput('rejected', 'Rejected')),
                           textInput('keywords', HTML('<h6 style="display: inline-block;">Keywords</h6>'), value = '',
                                     width = '100%'),
                           textAreaInput('comments', HTML('<h6 style="display: inline-block;">Review comments</h6>'),
                                         value = '', width = '100%', rows = 3)
                       )
                    ),

                    card(
                       uiOutput('plot_info')
                    ),

                    card(
                       actionButton('exit', 'Exit', width = '60px')
                    )
             ),

             column(4, class = 'col-fullheight',
                    imageOutput('ortho', height = 'auto'),
                    uiOutput('dz_info', class = 'vp-dz'),
                    div(class = 'vp-offset',
                        textOutput('offset_info', inline = TRUE),
                        actionButton('offset_clear', 'Clear offset', class = 'btn-sm')),
                    card(
                       uiOutput('ortho_select'),
                       radioButtons('zoom', HTML('<h6 style="display: inline-block;">Zoom</h6>'),
                                    choices = c('2 m' = 2, '4 m' = 4, '10 m' = 10), selected = 4, inline = TRUE)
                    )
             )
          )
      )
   )



   # Server -----------------------------
   server <- function(input, output, session) {

      rv <- reactiveValues(sel = integer(0), idx = 1, tick = 0, otick = 0)

      get_offset <- function(i, f) {                                                      # offset for plot row i and ortho file f, or NULL
         j <- which(offsets$plot_id == plots$plot_id[i] & offsets$ortho == basename(f))
         if(length(j) == 0) NULL else c(offsets$dx[j[1]], offsets$dy[j[1]])
      }

      set_offset <- function(plot_id, f, d) {                                             # set (or clear, if d is NULL) an offset and save
         offsets <<- offsets[!(offsets$plot_id == plot_id & offsets$ortho == basename(f)), ]
         if(!is.null(d))
            offsets <<- rbind(offsets, data.frame(plot_id = plot_id, ortho = basename(f), dx = d[1], dy = d[2]))
         if(!vp_write_offsets(offsets, offsets_file))
            showNotification(paste0('Could not write ', offsets_file, '. Is it open in another program?'),
                             type = 'error', duration = NULL)
         rv$otick <- rv$otick + 1
      }

      get_ortho <- function(f) {                                                          # load (and cache) ortho or DEM
         if(is.null(ortho_cache[[f]])) {
            o <- vp_load_ortho(f, orthos$type[match(f, orthos$file)])
            o$xy <- vp_temp_reproject(plots$easting, plots$northing, plot_crs, o$rast)    # TEMPORARY: plot centers in ortho's CRS
            ortho_cache[[f]] <- o
         }
         ortho_cache[[f]]
      }

      get_dz <- function(f) {                                                             # DEM - RTK height (m) for all plots, for DEM file f
         o <- get_ortho(f)
         if(is.null(o$dz)) {
            if(orthos$vdatum[match(f, orthos$file)] == 'ellipsoidal') {
               if(is.null(ellipsoidal)) {
                  e <- st_drop_geometry(st_read(file.path(path, 'prelim/everything.gpkg'), quiet = TRUE))
                  ellipsoidal <<- e$ellipsoidal_height[match(plots$plot_id, e$plot_id)]
               }
               h <- ellipsoidal
            }
            else
               h <- plots$elevation
            o$dz <- extract(o$rast, o$xy)[, 1] - h
            ortho_cache[[f]] <- o
         }
         o$dz
      }

      dz_dem <- function() {                                                              # DEM for dz filter: selected image if it's a DEM, else site's first DEM
         f <- isolate(input$ortho)
         if(!is.null(f) && orthos$type[match(f, orthos$file)] == 'dem')
            return(f)
         d <- orthos$file[orthos$site == input$site & orthos$type == 'dem']
         if(length(d) == 0) NULL else d[1]
      }

      cur <- reactive({                                                                   # current row in plots, or NA
         if(length(rv$sel) == 0) NA else rv$sel[min(rv$idx, length(rv$sel))]
      })


      observeEvent(c(input$site, input$filter, input$keyfilter), {                        # --- filter plots
         was <- cur()
         dz <- rep(NA_real_, nrow(plots))
         if(grepl('dz', input$keyfilter) && !is.null(f <- dz_dem()))                      #    only load DEM if we need it
            dz <- get_dz(f)
         rv$sel <- vp_filter(plots, review, input$site, input$filter, input$keyfilter,
                             has_offset = plots$plot_id %in% offsets$plot_id, dz = dz)
         rv$idx <- match(was, rv$sel, nomatch = 1)                                        #    stay on current plot if it's still selected
      })


      observe({                                                                           # --- new plot: send its review to the browser
         i <- cur()
         nxt <- isolate(if(rv$idx < length(rv$sel)) rv$sel[rv$idx + 1] else NA)
         session$sendCustomMessage('vp_set_review', list(
            plot_id = if(is.na(i)) NA else plots$plot_id[i],
            reviewed = !is.na(i) && review$reviewed[i],
            problems = !is.na(i) && review$problems[i],
            rejected = !is.na(i) && review$rejected[i],
            keywords = if(is.na(i)) '' else review$keywords[i],
            comments = if(is.na(i)) '' else review$comments[i],
            rotation = if(is.na(i)) NA else review$rotation[i],
            center_x = if(is.na(i)) NA else review$center_x[i],
            center_y = if(is.na(i)) NA else review$center_y[i],
            next_photo = if(!is.na(nxt) && plots$photo[nxt]) paste0('vp_photos/', plots$plot_id[nxt], '.jpg') else NA
         ))
      })


      observeEvent(input$review_edit, {                                                   # --- review edited in the browser
         e <- input$review_edit
         i <- match(e$plot_id, plots$plot_id)
         if(is.na(i) | !e$field %in% c('reviewed', 'problems', 'rejected', 'keywords', 'comments', 'rotation', 'center'))
            return()
         if(e$field == 'center') {                                                        #    center is sent as {x, y} or null
            review$center_x[i] <<- if(is.null(e$value)) NA else as.numeric(e$value$x)
            review$center_y[i] <<- if(is.null(e$value)) NA else as.numeric(e$value$y)
         }
         else if(e$field == 'rotation')
            review$rotation[i] <<- if(is.null(e$value)) NA else as.numeric(e$value)
         else
            review[i, e$field] <<- e$value
         if(!vp_write_review(review, extra, review_file))
            showNotification(paste0('Could not write ', review_file, '. Is it open in another program?'),
                             type = 'error', duration = NULL)
         rv$tick <- rv$tick + 1
      })


      output$site_info <- renderText({
         rv$tick
         s <- plots$site == input$site
         pct <- 100 * mean(review$reviewed[s])
         paste0(sum(s), ' plots, ', if(pct > 0 & pct < 1) '<1' else round(pct), '% reviewed')
      })

      output$plot_no <- renderText({
         if(length(rv$sel) == 0) 'No plots selected' else
            paste0('Plot ', rv$idx, ' of ', length(rv$sel),
                   if(trimws(input$filter) != '' | trimws(input$keyfilter) != '') ' (filtered)')
      })


      output$photo <- renderUI({                                                          # --- field photo
         i <- cur()
         if(is.na(i))
            return(div(class = 'vp-placeholder', 'No plots selected'))
         if(!plots$photo[i])
            return(div(class = 'vp-placeholder', paste0('No photo for ', plots$plot_id[i])))
         tags$img(id = 'photo_img', src = paste0('vp_photos/', plots$plot_id[i], '.jpg'))
      })


      output$ortho_select <- renderUI({
         o <- orthos[orthos$site == input$site, ]
         radioButtons('ortho', HTML('<h6 style="display: inline-block;">Image</h6>'),
                      choices = setNames(o$file, o$name))
      })


      output$ortho <- renderImage({                                                       # --- ortho clip
         i <- cur()
         req(!is.na(i), input$ortho, input$zoom)
         f <- input$ortho
         o <- get_ortho(f)
         rv$otick
         png <- tempfile(fileext = '.png')
         vp_render_ortho(o, o$xy[i, 1], o$xy[i, 2], as.numeric(input$zoom), png, offset = get_offset(i, f))
         list(src = png, contentType = 'image/png', width = '100%', alt = plots$plot_id[i],
              'data-plot' = plots$plot_id[i], 'data-ortho' = f, 'data-width' = input$zoom)   # so double-clicks know what they're on
      }, deleteFile = TRUE)


      observeEvent(input$ortho_dblclick, {                                                # --- double-click on ortho records offset
         e <- input$ortho_dblclick
         if(!e$plot_id %in% plots$plot_id | !e$ortho %in% orthos$file)
            return()
         set_offset(e$plot_id, e$ortho, c(as.numeric(e$dx), as.numeric(e$dy)))
      })


      observeEvent(input$offset_clear, {
         i <- cur()
         req(!is.na(i), input$ortho)
         set_offset(plots$plot_id[i], input$ortho, NULL)
      })


      output$dz_info <- renderUI({                                                        # --- DEM - RTK height, for each DEM at site
         i <- cur()
         req(!is.na(i))
         d <- orthos[orthos$site == input$site & orthos$type == 'dem', ]
         lapply(seq_len(nrow(d)), function(j) {
            dz <- get_dz(d$file[j])[i]
            div(paste0('DEM − RTK height: ', if(is.na(dz)) 'no data' else sprintf('%.1f cm', 100 * dz),
                       if(nrow(d) > 1) paste0(' (', d$name[j], ')')))
         })
      })


      output$offset_info <- renderText({
         rv$otick
         i <- cur()
         req(!is.na(i), input$ortho)
         d <- get_offset(i, input$ortho)
         if(is.null(d))
            return('Double-click the plot center on the image to record offset')
         sprintf('Offset: dx = %.2f, dy = %.2f  (%.2f m toward %d°)', d[1], d[2], sqrt(sum(d ^ 2)),
                 round((atan2(d[1], d[2]) * 180 / pi) %% 360))
      })


      output$plot_info <- renderUI({                                                      # --- plot data
         i <- cur()
         req(!is.na(i))
         vp_info(plots[i, ], cover[cover$plot_id == plots$plot_id[i], ])
      })


      observeEvent(input$first, rv$idx <- 1)                                              # --- navigation
      observeEvent(input$previous, rv$idx <- max(rv$idx - 1, 1))
      observeEvent(input$next_, rv$idx <- min(rv$idx + 1, max(length(rv$sel), 1)))
      observeEvent(input$last, rv$idx <- max(length(rv$sel), 1))
      observeEvent(input$key_nav, {
         n <- max(length(rv$sel), 1)
         rv$idx <- switch(input$key_nav,
                          first = 1,
                          previous = max(rv$idx - 1, 1),
                          'next' = min(rv$idx + 1, n),
                          last = n)
      })


      observeEvent(input$exit, {                                                          # --- Exit
         message('Reviews are saved in ', review_file)
         stopApp()
      })
   }


   shinyApp(ui = ui, server = server)
}



# Add a clear (x) button to a textInput
vp_clearable <- function(x) {
   id <- x$children[[2]]$attribs$id
   x$children[[2]] <- div(class = 'vp-clearable', x$children[[2]],
                          tags$button(type = 'button', class = 'vp-clear', `data-target` = id, title = 'Clear', HTML('&times;')))
   x
}



vp_css <- '
html, body, .container-fluid, .row.fullheight { height: 100%; }
.col-fullheight { height: 100vh; overflow-y: auto; padding-top: 10px; }
#photo_wrap { position: relative; height: calc(100vh - 70px); overflow: hidden; background: #222;
   border-radius: 6px; user-select: none; }
#photo, #photo > div { height: 100%; }
#photo_img { position: absolute; left: 0; top: 0; width: 100%; max-width: none; }
#photo_mark { display: none; position: absolute; left: 50%; top: 50%; width: 22px; height: 22px;
   margin: -11px 0 0 -11px; border: 2px solid #FF7F00; border-radius: 50%; pointer-events: none; }
.vp-rotate { display: flex; align-items: center; gap: 8px; padding-top: 8px; flex-wrap: wrap; }
#rot_slider { flex: 1 1 150px; }
#rot_number { width: 70px; }
.vp-hint { color: #777; font-size: 0.85em; }
#ortho img { width: 100%; height: auto; max-height: 85vh; object-fit: contain; cursor: crosshair; user-select: none; }
.vp-clearable { position: relative; }
.vp-clearable input { padding-right: 28px; }
.vp-clear { position: absolute; right: 6px; top: 50%; transform: translateY(-50%); border: none;
   background: none; color: #999; font-size: 1.3em; line-height: 1; padding: 0 4px; }
.vp-clear:hover { color: #333; }
.vp-clearable input:placeholder-shown + .vp-clear { display: none; }
.vp-dz { font-size: 0.9em; color: #555; margin-top: 4px; }
.vp-offset { display: flex; justify-content: space-between; align-items: center; gap: 8px;
   font-size: 0.9em; color: #555; margin: 4px 0 6px; }
.vp-placeholder { height: 100%; display: flex; align-items: center; justify-content: center;
   background: #eee; color: #666; font-size: 1.4em; }
.vp-table td { padding: 0 14px 0 0; vertical-align: top; }
.vp-table td.label { color: #777; }
.vp-table td.pct { text-align: right; }
.vp-subclass { font-size: 1.3em; font-weight: bold; margin: 0.7em 0; }
.vp-error { color: #c00; font-weight: bold; margin-top: 0.5em; }
.vp-note { color: #777; font-style: italic; }
.card { margin-bottom: 6px; }
.card-body { padding: 6px 12px; gap: 4px; }
.card h5 { margin-bottom: 0; }
.card .shiny-input-container:not(.shiny-input-container-inline) { margin-bottom: 4px; }
.card .form-label, .card .control-label { margin-bottom: 2px; }
.vp-checks { display: flex; gap: 18px; }
.vp-checks .shiny-input-container { width: auto !important; margin-bottom: 0 !important; }
.vp-checks .checkbox { margin: 0; }
'



# Review fields are handled in the browser so each edit is sent along with the plot id it
# belongs to. This way, edits can't land on the wrong plot when navigating quickly. Photo
# rotation is done in the browser too, so it's live.
vp_js <- '
var vpPlot = null;
var vpTimers = {};
var vpRot = 0;                                                    // photo rotation, degrees clockwise
var vpCenter = null;                                              // plot center {x, y} as fractions of photo width & height
var vpGeom = null;

$(document).on("shiny:connected", function() {
   Shiny.addCustomMessageHandler("vp_set_review", function(m) {
      vpPlot = m.plot_id;
      ["reviewed", "problems", "rejected"].forEach(function(f) { $("#" + f).prop("checked", m[f] === true); });
      $("#keywords").val(m.keywords || "");
      $("#comments").val(m.comments || "");
      $("#review_fields :input").prop("disabled", vpPlot === null);
      vpRot = (typeof m.rotation === "number") ? m.rotation : 0;
      vpCenter = (typeof m.center_x === "number") ? {x: m.center_x, y: m.center_y} : null;
      $("#rot_slider, #rot_number").val(vpRot);
      vpLayout();
      if (m.next_photo) (new Image()).src = m.next_photo;         // preload next photo
   });
});

function vpLayout() {                                             // fit photo in frame, center on plot center, and rotate
   var img = document.getElementById("photo_img");
   var wrap = document.getElementById("photo_wrap");
   var mark = document.getElementById("photo_mark");
   vpGeom = null;
   if (!img || !img.naturalWidth) { mark.style.display = "none"; return; }
   var W = wrap.clientWidth, H = wrap.clientHeight;
   var s = Math.min(W / img.naturalWidth, H / img.naturalHeight);
   var w = img.naturalWidth * s, h = img.naturalHeight * s;
   var cx = w / 2, cy = h / 2, ox = (W - w) / 2, oy = (H - h) / 2;
   if (vpCenter) {
      cx = vpCenter.x * w; cy = vpCenter.y * h;
      ox = W / 2 - cx; oy = H / 2 - cy;
   }
   img.style.width = w + "px"; img.style.height = h + "px";
   img.style.left = ox + "px"; img.style.top = oy + "px";
   img.style.transformOrigin = cx + "px " + cy + "px";
   img.style.transform = "rotate(" + vpRot + "deg)";
   mark.style.display = vpCenter ? "block" : "none";
   vpGeom = {w: w, h: h, ox: ox, oy: oy, cx: cx, cy: cy};
}

document.addEventListener("load", function(e) {                   // lay out each photo as it loads
   if (e.target.id === "photo_img") vpLayout();
}, true);
$(window).on("resize", vpLayout);

$(document).on("dblclick", "#photo_wrap", function(e) {           // double-click sets plot center
   if (!vpGeom) return;
   var r = this.getBoundingClientRect(), g = vpGeom;
   var dx = e.clientX - r.left - (g.ox + g.cx), dy = e.clientY - r.top - (g.oy + g.cy);
   var t = vpRot * Math.PI / 180;                                 // undo rotation to get position on photo
   var x = g.cx + dx * Math.cos(t) + dy * Math.sin(t);
   var y = g.cy - dx * Math.sin(t) + dy * Math.cos(t);
   vpCenter = {x: Math.min(Math.max(x / g.w, 0), 1), y: Math.min(Math.max(y / g.h, 0), 1)};
   vpLayout();
   vpSend("center", vpCenter, 0);
});

function vpSetRot(v, delay) {
   v = Math.round(Number(v));
   if (isNaN(v)) return;
   if (v > 180 || v < -180) v = ((v + 180) % 360 + 360) % 360 - 180;   // wrap typed values to -180 to 180
   vpRot = v;
   $("#rot_slider").val(v);
   if (Number($("#rot_number").val()) !== v) $("#rot_number").val(v);
   vpLayout();
   if (delay !== null) vpSend("rotation", v, delay);
}

$(document).on("input", "#rot_slider", function() { vpSetRot(this.value, null); });
$(document).on("change", "#rot_slider", function() { vpSetRot(this.value, 300); });
$(document).on("change", "#rot_number", function() { vpSetRot(this.value, 0); });
$(document).on("click", "#rot_clear", function() {
   vpCenter = null;
   vpSetRot(0, null);
   vpSend("rotation", null, 0);
   vpSend("center", null, 0);
});

function vpSend(field, value, delay) {
   var p = vpPlot;
   if (p === null) return;
   var key = field + "|" + p;
   clearTimeout(vpTimers[key]);
   vpTimers[key] = setTimeout(function() {
      delete vpTimers[key];
      Shiny.setInputValue("review_edit", {plot_id: p, field: field, value: value}, {priority: "event"});
   }, delay);
}

$(document).on("change", "#reviewed, #problems, #rejected", function() { vpSend(this.id, this.checked, 0); });
$(document).on("input", "#keywords, #comments", function() { vpSend(this.id, this.value, 400); });

$(document).on("dblclick", "#ortho img", function(e) {            // double-click on ortho records offset of plot center
   var r = this.getBoundingClientRect();
   var s = Math.min(r.width / this.naturalWidth, r.height / this.naturalHeight);   // image may be letterboxed (object-fit)
   var w = this.naturalWidth * s, h = this.naturalHeight * s;
   var fx = (e.clientX - r.left - (r.width - w) / 2) / w;
   var fy = (e.clientY - r.top - (r.height - h) / 2) / h;
   if (fx < 0 || fx > 1 || fy < 0 || fy > 1) return;
   var width = Number(this.dataset.width);
   Shiny.setInputValue("ortho_dblclick", {plot_id: this.dataset.plot, ortho: this.dataset.ortho,
      dx: (fx - 0.5) * width, dy: (0.5 - fy) * width}, {priority: "event"});
});

$(document).on("click", ".vp-clear", function() {                 // clear (x) buttons on filters
   $("#" + this.dataset.target).val("").trigger("change");
   this.blur();
});

$(document).on("keydown", function(e) {                           // arrow keys, Home, End navigate
   if (e.key === "Escape") { e.target.blur(); return; }           // Escape leaves a field so arrows navigate again
   if ($(e.target).is("input[type=text], input[type=number], input[type=range], textarea, select")) return;
   var k = {ArrowRight: "next", ArrowLeft: "previous", Home: "first", End: "last"}[e.key];
   if (k) {
      e.preventDefault();
      Shiny.setInputValue("key_nav", k, {priority: "event"});
   }
});
'
