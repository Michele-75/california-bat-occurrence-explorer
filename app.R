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

#Convert grid to WGS84 lat/long and standardize points and covariates

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

#Choices in the species filter
species_choices <- bat_points |>
  st_drop_geometry() |>
  distinct(species_label) |>
  arrange(species_label) |>
  pull(species_label)

#Choices in the year filter
year_choices <- bat_points |>
  st_drop_geometry() |>
  distinct(year) |>
  arrange(year) |>
  pull(year)

#Choices for background layer
layer_choices <- c(
  "Light pollution" = "mean_radiance",
  "Developed land (%)" = "pct_developed",
  "Population density" = "pop_density",
  "Protected area (%)" = "pct_protected"
)

#Set colors for different species points
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

#Used for legends and popups
pretty_layer_name <- function(var) {
  dplyr::case_when(
    var == "mean_radiance" ~ "Light pollution",
    var == "pct_developed" ~ "Developed land (%)",
    var == "pop_density" ~ "Population density",
    var == "pct_protected" ~ "Protected area (%)",
    TRUE ~ var
  )
}

#How values appear in popups
format_layer_value <- function(x, var) {
  if (is.na(x)) return("NA")
  
  if (var %in% c("pct_developed", "pct_protected")) {
    return(sprintf("%.1f%%", x))
  }
  
  if (var == "mean_radiance") {
    return(sprintf("%.2f", x))
  }
  
  if (var == "pop_density") {
    return(comma(round(x, 1)))
  }
  
  as.character(x)
}

#Create popup text for grid cells
make_grid_popup <- function(dat, var) {
  vals <- vapply(dat[[var]], format_layer_value, character(1), var = var)
  HTML(sprintf(
    "<strong>Cell ID:</strong> %s<br/><strong>%s:</strong> %s",
    dat$cell_id,
    pretty_layer_name(var),
    vals
  ))
}

#Create popup text for bat points
make_point_popup <- function(dat) {
  HTML(sprintf(
    "<strong>Species:</strong> %s<br/><strong>Year:</strong> %s",
    dat$species_label,
    dat$year
  ))
}

#Leaflet color function- creates a mapping from numeric values to colors
make_palette <- function(x) {
  colorNumeric(
    palette = "viridis",
    domain = x,
    na.color = "transparent"
  )
}

#Make species key custon html
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
      #Species filter
      selectInput(
        inputId = "species_filter",
        label = "Species",
        choices = species_choices,
        selected = species_choices, #Default selection
        multiple = TRUE #User can select more than one
      ),
      #Year filter
      selectInput(
        inputId = "year_filter",
        label = "Year(s)",
        choices = year_choices,
        selected = year_choices,
        multiple = TRUE
      ),
      #Radio button- select one covariate layer at a time
      radioButtons( 
        inputId = "covariate_layer",
        label = "Background layer",
        choices = layer_choices,
        selected = "mean_radiance"
      ),
      #Checkbox- show bat points or not
      checkboxInput(
        inputId = "show_points",
        label = "Show bat observation points",
        value = TRUE
      ),
      #Background opacity slider
      sliderInput(
        inputId = "grid_opacity",
        label = "Background opacity",
        min = 0,
        max = 1,
        value = 0.7,
        step = 0.05
      ),
      #Explanatory text
      hr(), 
      p("This map shows GBIF bat observations for three focal California species with one selected landscape layer displayed on a 10 km statewide grid.")
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
  
  observe({
    req(input$covariate_layer)
    
    var <- input$covariate_layer
    pal <- make_palette(app_grid[[var]])
    fill_vals <- pal(app_grid[[var]])
    
    leafletProxy("map", data = app_grid) |>
      clearShapes() |>
      removeControl("bg_legend") |>
      addPolygons(
        fillColor = fill_vals,
        fillOpacity = input$grid_opacity,
        color = "#666666",
        weight = 0.3,
        popup = make_grid_popup(app_grid, var),
        group = "grid",
        options = pathOptions(pane = "gridPane")
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = app_grid[[var]],
        title = pretty_layer_name(var),
        opacity = 1,
        layerId = "bg_legend"
      )
  })
  
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
          radius = 4,
          stroke = TRUE,
          color = "#222222",
          weight = 1,
          fillColor = point_colors,
          fillOpacity = 0.95,
          popup = make_point_popup(pts),
          group = "points",
          options = pathOptions(pane = "pointPane")
        ) |>
        addControl(
          html = species_key_html,
          position = "topright",
          layerId = "species_key_control"
        )
    }
  })
}



shinyApp(ui = ui, server = server)