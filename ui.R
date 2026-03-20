# ============================================================
# ui.R
# User interface definition — bslib / Bootstrap 5
# ============================================================

page_sidebar(
  
  title = div(
    style = "display:flex; align-items:center; gap:12px;",
    span("California Bat Observations Explorer",
         style = "font-size:1.4rem; font-weight:700; color:#ffffff;"),
    span("GBIF \u00b7 10 km grid \u00b7 environmental covariates",
         style = "font-size:0.85rem; color:#d4e0d4; font-weight:400;"),
    # Info button — opens About modal
    tags$button(
      type = "button",
      class = "btn btn-link p-0 ms-auto",
      style = "color:#d4e0d4; font-size:1.1rem; text-decoration:none;",
      `data-bs-toggle` = "modal",
      `data-bs-target` = "#aboutModal",
      icon("circle-info")
    )
  ),
  
  theme = bs_theme(
    version    = 5,
    bg         = "#fafaf7",
    fg         = "#2c3e2d",
    primary    = "#3a6b4c",
    secondary  = "#7a8b72",
    success    = "#5a8c5a",
    info       = "#5b8fa8",
    "font-size-base" = "0.88rem",
    base_font  = font_google("Lato"),
    heading_font = font_google("Lato"),
    "sidebar-width" = "280px",
    "sidebar-bg"    = "#e4e6de",
    "navbar-bg"     = "#3e5e47"
  ),
  
  # Map interaction — hover effect + persistent highlight while popup is open
  tags$head(
    tags$style(HTML(
      "/* Base transition for all interactive elements */
       .leaflet-interactive {
         transition: stroke-width 0.15s, stroke 0.15s, filter 0.15s, transform 0.15s;
         transform-origin: center;
         transform-box: fill-box;
       }

       /* Default hover for all: subtle dark outline (grid cells) */
       .leaflet-interactive:hover,
       .leaflet-interactive.popup-active-grid {
         stroke-width: 2 !important;
         stroke: #333333 !important;
         stroke-opacity: 0.8 !important;
       }

       /* Points override: glow + scale (more specific selector wins) */
       .leaflet-interactive.point-marker:hover,
       .leaflet-interactive.popup-active-point {
         stroke-width: 3 !important;
         stroke: #ffffff !important;
         stroke-opacity: 1 !important;
         filter: drop-shadow(0 0 5px rgba(255,255,255,0.8)) !important;
         transform: scale(1.8);
       }"
    )),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        var check = setInterval(function() {
          var container = document.querySelector('.leaflet-container');
          if (!container || !container._leaflet_id) return;
          clearInterval(check);

          var widget = HTMLWidgets.find('.leaflet-container');
          if (!widget) return;
          var map = widget.getMap();
          if (!map) return;

          // Tag point markers with a CSS class so we can style them differently
          function tagLayer(layer) {
            if (!layer._path || layer._tagged) return;
            if (layer.options && layer.options.pane === 'pointPane') {
              layer._path.classList.add('point-marker');
            }
            layer._tagged = true;
          }

          map.eachLayer(function(layer) { tagLayer(layer); });
          map.on('layeradd', function(e) { tagLayer(e.layer); });

          // Popup open/close: add class based on which pane the layer belongs to
          map.on('popupopen', function(e) {
            var layer = e.popup._source;
            if (layer && layer._path) {
              if (layer.options && layer.options.pane === 'pointPane') {
                layer._path.classList.add('popup-active-point');
              } else {
                layer._path.classList.add('popup-active-grid');
              }
            }
          });

          map.on('popupclose', function(e) {
            var layer = e.popup._source;
            if (layer && layer._path) {
              layer._path.classList.remove('popup-active-point');
              layer._path.classList.remove('popup-active-grid');
            }
          });

          // Opacity handler: updates grid cell fill opacity without redrawing
          Shiny.addCustomMessageHandler('updateGridOpacity', function(opacity) {
            map.eachLayer(function(layer) {
              if (layer.options && layer.options.pane === 'gridPane' && layer.setStyle) {
                layer.setStyle({ fillOpacity: opacity });
              }
            });
          });
        }, 300);
      });
    "))
  ),
  
  # ---- Sidebar ----
  sidebar = sidebar(
    width = 280,
    
    # Compact spacing overrides
    tags$style(HTML("
      .sidebar { padding-top: 0.45rem !important; }
      .sidebar > .sidebar-content { gap: 0.65rem !important; }
      .sidebar .form-group { margin-bottom: 0.35rem; }
      .sidebar .control-label { margin-bottom: 4px; font-size: 0.82rem; font-weight: 600; color: #2c3e2d; }
      .sidebar .shiny-input-container { margin-bottom: 0.3rem; }
      .sidebar .radio { margin-top: 0; margin-bottom: 0; }
      .sidebar hr { margin: 14px 0; border-color: #b0b5a8; border-width: 1.5px; }
      .sidebar .selectize-input { min-height: 0; padding: 3px 6px; font-size: 0.82rem;
        max-height: 105px; overflow-y: auto; }
      .sidebar .selectize-input .item { padding: 1px 5px; margin: 1px; font-size: 0.78rem; }
      .sidebar .selectize-dropdown { font-size: 0.82rem; }
      .sidebar .checkbox { margin-top: 0; margin-bottom: 0.3rem; }
      .sidebar .checkbox label { font-size: 0.8rem; }
      .year-group .shiny-input-container { margin-bottom: 0; }
      .year-group .form-group { margin-bottom: 0; }
      .year-group .year-toggle { margin-bottom: 10px; }
      .section-label { font-weight:700; font-size:0.82rem; color:#3e5e47;
        text-transform:uppercase; letter-spacing:0.5px; margin-bottom:3px; }
      #species_filter_wrapper .shiny-input-container { margin-top: 0 !important; margin-bottom: 0.15rem !important; }
      #species_filter_wrapper .form-group { margin-top: 0; margin-bottom: 0; }
      #species_filter_wrapper { margin-bottom: 0.2rem !important; }
    ")),
    
    # -- Observation filters --
    tags$div(class = "section-label", "Observation filters"),
    
    tags$div(
      id = "species_filter_wrapper",
      style = "margin-top:0; margin-bottom:0;",
      tags$label(style = "font-size:0.82rem; font-weight:600; color:#2c3e2d; margin:0 0 4px 0; padding:0; display:block; line-height:1.3;",
                 "Species"),
      selectizeInput(
        inputId  = "species_filter",
        label    = NULL,
        choices  = species_choices,
        selected = species_choices,
        multiple = TRUE,
        options  = list(
          plugins     = list("remove_button"),
          placeholder = "Select species..."
        )
      )
    ),
    
    # Year controls — label and mode on separate lines, tight
    tags$div(
      class = "year-group",
      style = "margin-bottom:0.2rem;",
      tags$div(
        class = "year-toggle",
        style = "display:flex; align-items:center; justify-content:space-between;",
        tags$span(style = "font-size:0.82rem; font-weight:600; color:#2c3e2d;", "Years"),
        tags$div(
          style = "display:flex; align-items:center; gap:2px; font-size:0.78rem;",
          radioButtons(
            inputId  = "year_mode",
            label    = NULL,
            choices  = c("Range" = "range", "Animate" = "animate"),
            selected = "range",
            inline   = TRUE
          )
        )
      ),
      conditionalPanel(
        condition = "input.year_mode == 'range'",
        sliderInput(
          inputId = "year_range",
          label   = NULL,
          min     = year_range[1],
          max     = year_range[2],
          value   = year_range,
          step    = 1,
          sep     = "",
          ticks   = FALSE
        )
      ),
      conditionalPanel(
        condition = "input.year_mode == 'animate'",
        tags$div(
          style = "font-size:0.72rem; color:#6c7a6c; margin-bottom:2px; font-style:italic;",
          "Shows cumulative observations"
        ),
        sliderInput(
          inputId = "year_animate",
          label   = NULL,
          min     = year_range[1],
          max     = year_range[2],
          value   = year_range[1],
          step    = 1,
          sep     = "",
          ticks   = FALSE,
          animate = animationOptions(
            interval = 1200,
            loop     = TRUE
          )
        )
      )
    ),
    
    hr(),
    
    # -- Map display --
    tags$div(class = "section-label", "Map display"),
    
    checkboxInput(
      inputId = "show_points",
      label   = "Show observation points",
      value   = TRUE
    ),
    
    selectInput(
      inputId  = "covariate_layer",
      label    = "Background layer",
      choices  = layer_choices,
      selected = "pop_density"
    ),
    
    sliderInput(
      inputId = "grid_opacity",
      label   = "Background opacity",
      min     = 0,
      max     = 1,
      value   = 0.7,
      step    = 0.05,
      ticks   = FALSE
    ),
    
    actionButton(
      inputId = "reset_filters",
      label   = "Reset all filters",
      icon    = icon("rotate-left"),
      width   = "100%",
      class   = "btn-outline-secondary btn-sm"
    )
  ),
  
  # ---- Main content ----
  leafletOutput("map", height = "calc(100vh - 80px)"),
  
  # ---- About modal (triggered by info button in title bar) ----
  tags$div(
    class = "modal fade", id = "aboutModal", tabindex = "-1",
    tags$div(
      class = "modal-dialog",
      tags$div(
        class = "modal-content",
        style = "background:#fafaf7;",
        tags$div(
          class = "modal-header",
          style = "border-bottom:1px solid #d5d8d0;",
          tags$h5(class = "modal-title",
                  style = "color:#2c3e2d; font-weight:700;",
                  "About this app"),
          tags$button(type = "button", class = "btn-close",
                      `data-bs-dismiss` = "modal")
        ),
        tags$div(
          class = "modal-body",
          style = "font-size:0.88rem; color:#3a4a3a; line-height:1.6;",
          p("Interactive map of GBIF bat occurrence records across",
            "California, overlaid on a 10 km statewide grid with",
            "precomputed environmental covariates."),
          tags$div(
            style = "margin:12px 0;",
            tags$strong("Data sources"),
            tags$ul(
              style = "padding-left:20px; margin-top:4px; margin-bottom:0;",
              tags$li("Bat occurrences: GBIF"),
              tags$li("Light pollution: VIIRS nighttime radiance"),
              tags$li("Developed land: NLCD 2019"),
              tags$li("Population density: 2020 raster"),
              tags$li("Protected areas: PAD-US GAP 1\u20133")
            )
          ),
          p(style = "margin-bottom:0; color:#6c7a6c;",
            "Built with R, Shiny, Leaflet, and sf.")
        )
      )
    )
  )
)