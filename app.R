library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(htmltools)
library(scales)

# ============================================================
# app.R
# California Bat Observations Explorer — MVP shell
# ============================================================

# -----------------------------
# File paths
# -----------------------------
GRID_PATH   <- file.path("data", "processed", "app", "ca_grid_10km.gpkg")
POINTS_PATH <- file.path("data", "processed", "app", "bat_points_app.rds")
COVS_PATH   <- file.path("data", "processed", "app", "grid_covariates_app.csv")

# -----------------------------
# Checks
# -----------------------------
if (!file.exists(GRID_PATH)) stop("Missing grid file: ", GRID_PATH)
if (!file.exists(POINTS_PATH)) stop("Missing bat points file: ", POINTS_PATH)
if (!file.exists(COVS_PATH)) stop("Missing covariates file: ", COVS_PATH)

# -----------------------------
# Load data once at app startup
# -----------------------------
grid <- st_read(GRID_PATH, layer = "ca_grid_10km", quiet = TRUE) |>
  st_transform(4326) |>
  mutate(cell_id = as.character(cell_id))

bat_points <- readRDS(POINTS_PATH)
if (!inherits(bat_points, "sf")) {
  stop("bat_points_app.rds must contain an sf object.")
}

if (!"species_label" %in% names(bat_points)) {
  bat_points$species_label <- bat_points$species
}

# Convert spatial data to WGS84 lat/long and standardize fields
bat_points <- bat_points |>
  st_transform(4326) |>
  mutate(
    gbif_id = as.character(gbif_id),
    species = as.character(species),
    species_label = as.character(species_label),
    year = as.integer(year)
  )

covs <- read_csv(COVS_PATH, show_col_types = FALSE) |>
  mutate(
    cell_id = as.character(cell_id),
    mean_radiance = as.numeric(mean_radiance),
    pct_developed = as.numeric(pct_developed),
    pct_protected = as.numeric(pct_protected),
    pop_density = as.numeric(pop_density)
  )

# Join grid + covariates once
app_grid <- grid |>
  left_join(covs, by = "cell_id")

# -----------------------------
# UI choices
# -----------------------------

# Choices in the species filter
species_choices <- bat_points |>
  st_drop_geometry() |>
  distinct(species_label) |>
  arrange(species_label) |>
  pull(species_label)

# Choices in the year filter
year_choices <- bat_points |>
  st_drop_geometry() |>
  distinct(year) |>
  arrange(year) |>
  pull(year)

# Choices for background layer
layer_choices <- c(
  "Light pollution" = "mean_radiance",
  "Developed land (%)" = "pct_developed",
  "Population density" = "pop_density",
  "Protected area (%)" = "pct_protected"
)

# Set colors for different species points
species_palette_values <- c(
  "#FF4E3A",  # vivid red-orange
  "#00B0F6",  # bright blue
  "#FFD23F",  # golden yellow
  "#E76BF3",  # magenta
  "#39B600",  # bright green
  "#F98400"   # orange
)

species_palette <- setNames(
  species_palette_values[seq_along(species_choices)],
  species_choices
)

# -----------------------------
# Helper functions
# -----------------------------

# Metadata for each map layer.
#
# For log-transformed variables we use colorBin with explicit breaks
# defined in ORIGINAL (untransformed) units. This avoids the misleading
# legend that colorNumeric + back-transformed labels produces — where
# the smooth gradient implies even spacing but the labels are uneven.
#
# With colorBin, each discrete colour block maps to one explicit range
# of values, so the legend is honest about the non-linear scale.
#
# Percentage variables keep colorNumeric — no transform needed.

layer_meta <- function(var) {
  switch(
    var,
    
    # ----- LOG-TRANSFORMED: use colorBin with explicit breaks -----
    
    "mean_radiance" = list(
      label        = "Light pollution",
      legend_title = "Light pollution (nW/cm\u00B2/sr)",
      digits       = 2,
      pal_type     = "bin",
      # Breaks in original units; spans nighttime background (~0)
      # through heavily lit urban areas.
      # Adjust the upper break if your data max is very different.
      breaks       = c(0, 0.5, 1, 2, 5, 10, 25, 50, 100, Inf),
      transform    = log1p,
      bin_labels   = c(
        "0 – 0.5", "0.5 – 1", "1 – 2", "2 – 5",
        "5 – 10", "10 – 25", "25 – 50", "50 – 100", "> 100"
      )
    ),
    
    "pop_density" = list(
      label        = "Population density",
      legend_title = "Population density (people/km\u00B2)",
      digits       = 0,
      pal_type     = "bin",
      # One colour per order of magnitude
      breaks       = c(0, 1, 10, 100, 1000, 5000, Inf),
      transform    = log1p,
      bin_labels   = c(
        "0 – 1", "1 – 10", "10 – 100",
        "100 – 1,000", "1,000 – 5,000", "> 5,000"
      )
    ),
    
    # ----- UNTRANSFORMED: colorNumeric, no breaks needed -----
    
    "pct_developed" = list(
      label        = "Developed land (%)",
      legend_title = "Developed land (%)",
      digits       = 1,
      pal_type     = "numeric",
      transform    = identity
    ),
    
    "pct_protected" = list(
      label        = "Protected area (%)",
      legend_title = "Protected area (%)",
      digits       = 1,
      pal_type     = "numeric",
      transform    = identity
    ),
    
    stop("Unknown layer: ", var)
  )
}

# Used for legends and popups
pretty_layer_name <- function(var) {
  layer_meta(var)$label
}

# How values appear in popups (always original units)
format_layer_value <- function(x, var) {
  if (is.na(x)) return("NA")
  
  if (var %in% c("pct_developed", "pct_protected")) {
    return(sprintf("%.1f%%", x))
  }
  
  if (var == "mean_radiance") {
    return(paste0(sprintf("%.2f", x), " nW/cm\u00B2/sr"))
  }
  
  if (var == "pop_density") {
    return(paste0(comma(round(x, 0)), " people/km\u00B2"))
  }
  
  as.character(x)
}

# Build the leaflet palette function for a given variable.
# Returns a list with:
#   pal  — the colour function (pass it raw untransformed values)
#   type — "bin" or "numeric" (needed so the legend builder knows
#          which addLegend signature to use)

make_palette <- function(x, var) {
  meta <- layer_meta(var)
  
  if (meta$pal_type == "bin") {
    # colorBin: breaks are in original units.
    # We log-transform the data AND the breaks so that colours are
    # assigned in log-space (giving visual weight to low-value
    # differences), but the breaks still correspond to the round
    # numbers we defined.
    transformed_breaks <- meta$transform(meta$breaks)
    
    pal_fn <- colorBin(
      palette  = "viridis",
      domain   = meta$transform(x),
      bins     = transformed_breaks,
      na.color = "transparent"
    )
    
    # Wrap so callers pass raw values and get back colours
    wrapped <- function(raw_vals) pal_fn(meta$transform(raw_vals))
    
    return(list(pal = wrapped, pal_fn = pal_fn, type = "bin"))
    
  } else {
    # colorNumeric for percentage layers (no transform)
    pal_fn <- colorNumeric(
      palette  = "viridis",
      domain   = x,
      na.color = "transparent"
    )
    
    return(list(pal = pal_fn, pal_fn = pal_fn, type = "numeric"))
  }
}

# Create popup text for grid cells
make_grid_popup <- function(dat, var) {
  vals <- vapply(dat[[var]], format_layer_value, character(1), var = var)
  
  HTML(sprintf(
    "<strong>Cell ID:</strong> %s<br/><strong>%s:</strong> %s",
    dat$cell_id,
    pretty_layer_name(var),
    vals
  ))
}

# Create popup text for bat points
make_point_popup <- function(dat) {
  HTML(sprintf(
    "<strong>Species:</strong> %s<br/><strong>Year:</strong> %s",
    dat$species_label,
    dat$year
  ))
}

# Make species key custom html
make_species_key_html <- function(species_counts, species_palette) {
  rows <- lapply(seq_len(nrow(species_counts)), function(i) {
    sp <- species_counts$species_label[i]
    n  <- species_counts$n[i]
    col <- species_palette[[sp]]
    
    tags$div(
      style = "display:flex; align-items:center; margin-bottom:6px;",
      tags$div(
        style = paste0(
          "width:14px; height:14px; border-radius:50%; margin-right:8px; ",
          "background:", col, "; border:1px solid #333; flex-shrink:0;"
        )
      ),
      tags$span(paste0(sp, " (", comma(n), ")"))
    )
  })
  
  as.character(
    tags$div(
      style = paste0(
        "background: rgba(255,255,255,0.95); ",
        "padding: 8px 10px; border-radius: 4px; ",
        "box-shadow: 0 1px 5px rgba(0,0,0,0.3); font-size: 13px;"
      ),
      tags$div(
        style = "font-weight: 600; margin-bottom: 6px;",
        "Species key"
      ),
      rows
    )
  )
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("California Bat Observations Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Species filter
      selectInput(
        inputId = "species_filter",
        label = "Species",
        choices = species_choices,
        selected = species_choices,
        multiple = TRUE
      ),
      # Year filter
      selectInput(
        inputId = "year_filter",
        label = "Year(s)",
        choices = year_choices,
        selected = year_choices,
        multiple = TRUE
      ),
      # Radio button — select one covariate layer at a time
      radioButtons(
        inputId = "covariate_layer",
        label = "Background layer",
        choices = layer_choices,
        selected = "mean_radiance"
      ),
      # Checkbox — show bat points or not
      checkboxInput(
        inputId = "show_points",
        label = "Show bat observation points",
        value = TRUE
      ),
      # Background opacity slider
      sliderInput(
        inputId = "grid_opacity",
        label = "Background opacity",
        min = 0,
        max = 1,
        value = 0.7,
        step = 0.05
      ),
      # Explanatory text
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

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  filtered_points <- reactive({
    req(input$species_filter, input$year_filter)
    
    bat_points |>
      filter(
        species_label %in% input$species_filter,
        year %in% input$year_filter
      )
  })
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addMapPane("gridPane", zIndex = 410) |>
      addMapPane("pointPane", zIndex = 420) |>
      setView(lng = -119.5, lat = 37.2, zoom = 6)
  })
  
  # ---- Grid / background observer ----
  observe({
    req(input$covariate_layer)
    
    var  <- input$covariate_layer
    meta <- layer_meta(var)
    pal_obj <- make_palette(app_grid[[var]], var)
    
    # Get fill colours for every grid cell
    fill_vals <- pal_obj$pal(app_grid[[var]])
    
    proxy <- leafletProxy("map", data = app_grid) |>
      clearGroup("grid") |>
      removeControl("bg_legend") |>
      addPolygons(
        fillColor   = fill_vals,
        fillOpacity = input$grid_opacity,
        color       = "#666666",
        weight      = 0.3,
        popup       = make_grid_popup(app_grid, var),
        group       = "grid",
        options     = pathOptions(pane = "gridPane")
      )
    
    # --- Add the appropriate legend type ---
    if (pal_obj$type == "bin") {
      # Discrete legend with human-readable labels in original units.
      # Each colour block = one bin, labelled with the range we defined.
      proxy |>
        addLegend(
          position = "bottomright",
          colors   = vapply(
            seq_len(length(meta$breaks) - 1),
            function(i) {
              # Pick midpoint of each bin (in transformed space) to
              # sample the correct colour from the palette
              mid <- (meta$transform(meta$breaks[i]) +
                        meta$transform(meta$breaks[i + 1])) / 2
              pal_obj$pal_fn(mid)
            },
            character(1)
          ),
          labels   = meta$bin_labels,
          title    = meta$legend_title,
          opacity  = 1,
          layerId  = "bg_legend"
        )
    } else {
      # Continuous legend for percentage variables (no transform)
      proxy |>
        addLegend(
          position  = "bottomright",
          pal       = pal_obj$pal_fn,
          values    = app_grid[[var]],
          title     = meta$legend_title,
          opacity   = 1,
          labFormat = labelFormat(digits = meta$digits),
          layerId   = "bg_legend"
        )
    }
  })
  
  # ---- Points observer ----
  observe({
    pts <- filtered_points()
    
    leafletProxy("map") |>
      clearGroup("points") |>
      removeControl("species_key_control")
    
    if (isTRUE(input$show_points) && nrow(pts) > 0) {
      point_colors <- unname(species_palette[pts$species_label])
      
      species_counts <- pts |>
        st_drop_geometry() |>
        count(species_label, name = "n") |>
        arrange(match(species_label, input$species_filter))
      
      species_key_html <- make_species_key_html(species_counts, species_palette)
      
      leafletProxy("map", data = pts) |>
        addCircleMarkers(
          radius      = 4,
          stroke      = TRUE,
          color       = "#222222",
          weight      = 1,
          fillColor   = point_colors,
          fillOpacity = 0.95,
          popup       = make_point_popup(pts),
          group       = "points",
          options     = pathOptions(pane = "pointPane")
        ) |>
        addControl(
          html     = species_key_html,
          position = "topright",
          layerId  = "species_key_control"
        )
    }
  })
}

shinyApp(ui = ui, server = server)