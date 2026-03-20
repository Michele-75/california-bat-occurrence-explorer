# ============================================================
# ui.R
# User interface definition — bslib / Bootstrap 5
# ============================================================

page_sidebar(
  
  title = div(
    style = "display:flex; align-items:baseline; gap:12px;",
    span("California Bat Observations Explorer",
         style = "font-size:1.4rem; font-weight:700; color:#ffffff;"),
    span("GBIF \u00b7 10 km grid \u00b7 environmental covariates",
         style = "font-size:0.85rem; color:#d4e0d4; font-weight:400;")
  ),
  
  theme = bs_theme(
    version    = 5,
    bg         = "#fafaf7",       # warm off-white background
    fg         = "#2c3e2d",       # dark forest charcoal text
    primary    = "#3a6b4c",       # deep pine green — buttons, accents
    secondary  = "#7a8b72",       # sage gray — muted secondary elements
    success    = "#5a8c5a",       # earthy green
    info       = "#5b8fa8",       # slate blue — subtle info accents
    "font-size-base" = "0.92rem",
    base_font  = font_google("Lato"),
    heading_font = font_google("Lato"),
    "sidebar-width" = "300px",
    "sidebar-bg"    = "#e4e6de",  # slightly darker warm gray sidebar
    "navbar-bg"     = "#3e5e47"   # mid-tone forest green title bar
  ),
  
  # ---- Sidebar ----
  sidebar = sidebar(
    width = 300,
    
    # Tighten default Shiny input spacing
    tags$style(HTML("
      .sidebar .form-group { margin-bottom: 0.5rem; }
      .sidebar .control-label { margin-bottom: 0.15rem; font-size: 0.85rem; }
      .sidebar .shiny-input-container { margin-bottom: 0.4rem; }
      .sidebar .radio { margin-top: 0.1rem; margin-bottom: 0.1rem; }
      .sidebar hr { margin: 8px 0; }
    ")),
    
    # -- Observation filters --
    tags$div(
      style = "font-weight:600; font-size:0.8rem; color:#5a6b52; text-transform:uppercase; letter-spacing:0.5px; margin-bottom:4px;",
      "Observation filters"
    ),
    
    selectizeInput(
      inputId  = "species_filter",
      label    = "Species",
      choices  = species_choices,
      selected = species_choices,
      multiple = TRUE,
      options  = list(
        plugins     = list("remove_button"),
        placeholder = "Select species..."
      )
    ),
    
    radioButtons(
      inputId  = "year_mode",
      label    = "Year filter mode",
      choices  = c("Range" = "range", "Animate" = "animate"),
      selected = "range",
      inline   = TRUE
    ),
    
    conditionalPanel(
      condition = "input.year_mode == 'range'",
      sliderInput(
        inputId = "year_range",
        label   = "Year range",
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
      sliderInput(
        inputId = "year_animate",
        label   = "Observations through",
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
    ),
    
    hr(),
    
    # -- Map display --
    tags$div(
      style = "font-weight:600; font-size:0.8rem; color:#5a6b52; text-transform:uppercase; letter-spacing:0.5px; margin-bottom:4px;",
      "Map display"
    ),
    
    checkboxInput(
      inputId = "show_points",
      label   = "Show bat observation points",
      value   = TRUE
    ),
    
    radioButtons(
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
      step    = 0.05
    ),
    
    actionButton(
      inputId = "reset_filters",
      label   = "Reset all filters",
      icon    = icon("rotate-left"),
      width   = "100%",
      class   = "btn-outline-secondary btn-sm"
    ),
    
    hr(style = "margin:12px 0;"),
    
    # -- About --
    tags$details(
      tags$summary(
        style = "font-weight:600; font-size:0.85rem; color:#5a6b52; text-transform:uppercase; letter-spacing:0.5px; cursor:pointer;",
        "About this app"
      ),
      tags$div(
        style = "font-size:0.82rem; color:#4a5a44; margin-top:8px; line-height:1.5;",
        p("Interactive map of GBIF bat occurrence records across California,",
          "overlaid on a 10 km statewide grid with precomputed",
          "environmental covariates."),
        tags$ul(
          style = "padding-left:18px; margin-bottom:6px;",
          tags$li("Light pollution: VIIRS nighttime radiance"),
          tags$li("Developed land: NLCD 2019"),
          tags$li("Population density: 2020 raster"),
          tags$li("Protected areas: PAD-US GAP 1\u20133")
        ),
        p(style = "margin-bottom:0;",
          "Built with R, Shiny, Leaflet, and sf.")
      )
    )
  ),
  
  # ---- Main content ----
  leafletOutput("map", height = "calc(100vh - 80px)")
)