# ============================================================
# ui.R
# User interface definition — bslib / Bootstrap 5
# ============================================================

page_sidebar(
  
  title = div(
    style = "display:flex; align-items:baseline; gap:12px;",
    span("California Bat Observations Explorer",
         style = "font-size:1.4rem; font-weight:700;"),
    span("GBIF \u00b7 10 km grid \u00b7 environmental covariates",
         style = "font-size:0.85rem; color:#6c757d; font-weight:400;")
  ),
  
  theme = bs_theme(
    version   = 5,
    bootswatch = "flatly",
    base_font = font_google("Source Sans Pro"),
    "sidebar-width" = "300px"
  ),
  
  # ---- Sidebar ----
  sidebar = sidebar(
    width = 300,
    
    # -- Observation filters --
    tags$div(
      style = "font-weight:600; font-size:0.85rem; color:#6c757d; text-transform:uppercase; letter-spacing:0.5px; margin-bottom:8px;",
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
    
    hr(style = "margin:12px 0;"),
    
    # -- Map display --
    tags$div(
      style = "font-weight:600; font-size:0.85rem; color:#6c757d; text-transform:uppercase; letter-spacing:0.5px; margin-bottom:8px;",
      "Map display"
    ),
    
    radioButtons(
      inputId  = "covariate_layer",
      label    = "Background layer",
      choices  = layer_choices,
      selected = "pop_density"
    ),
    
    checkboxInput(
      inputId = "show_points",
      label   = "Show bat observation points",
      value   = TRUE
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
        style = "font-weight:600; font-size:0.85rem; color:#6c757d; text-transform:uppercase; letter-spacing:0.5px; cursor:pointer;",
        "About this app"
      ),
      tags$div(
        style = "font-size:0.82rem; color:#555; margin-top:8px; line-height:1.5;",
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