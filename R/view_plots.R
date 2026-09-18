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
#' relief), and `file` (relative to `ortho_path`, or a full path). If `plots` doesn't have a
#' `site` column, all plots are assigned to the first site in `orthos.txt`.
#'
#' Plot coordinates are used as-is on orthos, with no reprojection. **Note that orthos may
#' be in a different CRS (or NAD83 realization) than the plots.**
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
#'   negate it. Reserved words are `reviewed`, `problems`, `rejected`, `comments` (has
#'   comments), `keywords` (has any keywords), `photo` (has a photo), and `rotated` (photo has
#'   been rotated). Any other word matches plots with that word in their review keywords. For
#'   example, `!reviewed` shows plots that haven't been reviewed, and `problems macroalgae`
#'   shows plots flagged with problems that have the keyword macroalgae. Changes to reviews
#'   don't hide the current plot until the filter is changed.
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
#' @import shiny
#' @importFrom bslib bs_theme card
#' @importFrom shinybusy add_busy_spinner
#' @importFrom sf st_read st_coordinates st_drop_geometry
#' @importFrom utils read.table
#' @importFrom stats setNames
#' @export


view_plots <- function(path = 'C:/Work/saltmarsh/data/uas2026/field',
                       ortho_path = 'C:/Work/saltmarsh/data/uas2026/orthos') {


   plots <- st_read(file.path(path, 'prelim/plots.gpkg'), quiet = TRUE)
   xy <- st_coordinates(plots)
   plots <- st_drop_geometry(plots)
   plots$easting <- xy[, 1]
   plots$northing <- xy[, 2]

   orthos <- read.table(file.path(path, 'pars/orthos.txt'), sep = '\t', header = TRUE, quote = '',
                        comment.char = '')
   absolute <- grepl('^([A-Za-z]:)?[/\\\\]', orthos$file)
   orthos$file[!absolute] <- file.path(ortho_path, orthos$file[!absolute])
   sites <- unique(orthos$site)
   if(is.null(plots$site))
      plots$site <- sites[1]

   cover <- read.table(file.path(path, 'prelim/pct_cover.txt'), sep = '\t', header = TRUE, quote = '',
                       comment.char = '', na.strings = c('', 'NA'))

   photo_dir <- file.path(path, 'photos/plots')
   plots$photo <- file.exists(file.path(photo_dir, paste0(plots$plot_id, '.jpg')))
   addResourcePath('vp_photos', photo_dir)

   review_file <- file.path(path, 'pars/review.txt')
   x <- vp_read_review(review_file, plots$plot_id)
   review <- x$review                                                                   # modified by the server
   extra <- x$extra

   ortho_cache <- new.env()                                                             # loaded orthos, with stretches



   # User interface ---------------------
   ui <- fluidPage(

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
                       textInput('filter', HTML('<h6 style="display: inline-block;">Plot filter</h6>'), value = '',
                                 width = '100%', placeholder = 'e.g., BJ21  ?A04  *-03-'),
                       textInput('keyfilter', HTML('<h6 style="display: inline-block;">Keyword filter</h6>'), value = '',
                                 width = '100%', placeholder = 'e.g., !reviewed  problems'),
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

      rv <- reactiveValues(sel = integer(0), idx = 1, tick = 0)

      cur <- reactive({                                                                 # current row in plots, or NA
         if(length(rv$sel) == 0) NA else rv$sel[min(rv$idx, length(rv$sel))]
      })


      observeEvent(c(input$site, input$filter, input$keyfilter), {                      # --- filter plots
         was <- cur()
         rv$sel <- vp_filter(plots, review, input$site, input$filter, input$keyfilter)
         rv$idx <- match(was, rv$sel, nomatch = 1)                                      #    stay on current plot if it's still selected
      })


      observe({                                                                         # --- new plot: send its review to the browser
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


      observeEvent(input$review_edit, {                                                 # --- review edited in the browser
         e <- input$review_edit
         i <- match(e$plot_id, plots$plot_id)
         if(is.na(i) | !e$field %in% c('reviewed', 'problems', 'rejected', 'keywords', 'comments', 'rotation', 'center'))
            return()
         if(e$field == 'center') {                                                      #    center is sent as {x, y} or null
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
         if(length(rv$sel) == 0) 'No plots selected' else paste0('Plot ', rv$idx, ' of ', length(rv$sel))
      })


      output$photo <- renderUI({                                                        # --- field photo
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


      output$ortho <- renderImage({                                                     # --- ortho clip
         i <- cur()
         req(!is.na(i), input$ortho, input$zoom)
         f <- input$ortho
         if(is.null(ortho_cache[[f]]))
            ortho_cache[[f]] <- vp_load_ortho(f, orthos$type[match(f, orthos$file)])
         png <- tempfile(fileext = '.png')
         vp_render_ortho(ortho_cache[[f]], plots$easting[i], plots$northing[i], as.numeric(input$zoom), png)
         list(src = png, contentType = 'image/png', width = '100%', alt = plots$plot_id[i])
      }, deleteFile = TRUE)


      output$plot_info <- renderUI({                                                    # --- plot data
         i <- cur()
         req(!is.na(i))
         vp_info(plots[i, ], cover[cover$plot_id == plots$plot_id[i], ])
      })


      observeEvent(input$first, rv$idx <- 1)                                            # --- navigation
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


      observeEvent(input$exit, {                                                        # --- Exit
         message('Reviews are saved in ', review_file)
         stopApp()
      })
   }


   shinyApp(ui = ui, server = server)
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
#ortho img { width: 100%; height: auto; max-height: 85vh; object-fit: contain; }
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
