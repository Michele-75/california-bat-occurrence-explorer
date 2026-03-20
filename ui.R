# ============================================================
# ui.R
# User interface definition
# ============================================================

fluidPage(
  titlePanel("California Bat Observations Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Species filter
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
      
      # Year filter mode toggle
      radioButtons(
        inputId  = "year_mode",
        label    = "Year filter mode",
        choices  = c("Range" = "range", "Animate" = "animate"),
        selected = "range",
        inline   = TRUE
      ),
      
      # Range slider (shown in range mode)
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
      
      # Animated cumulative slider (shown in animate mode)
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
      
      # Background covariate layer
      radioButtons(
        inputId  = "covariate_layer",
        label    = "Background layer",
        choices  = layer_choices,
        selected = "pop_density"
      ),
      
      # Toggle observation points
      checkboxInput(
        inputId = "show_points",
        label   = "Show bat observation points",
        value   = TRUE
      ),
      
      # Grid opacity
      sliderInput(
        inputId = "grid_opacity",
        label   = "Background opacity",
        min     = 0,
        max     = 1,
        value   = 0.7,
        step    = 0.05,
        ticks   = FALSE
      ),
      
      hr(),
      p(
        "This map shows GBIF bat observations for three focal",
        "California species with one selected landscape layer",
        "displayed on a 10 km statewide grid."
      )
    ),
    mainPanel(
      width = 9,
      leafletOutput("map", height = 720)
    )
  )
)