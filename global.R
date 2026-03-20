# ============================================================
# global.R
# Libraries, data loading, constants, species palette
# Runs once at app startup (before ui.R and server.R).
# Files in R/ are auto-sourced by Shiny before this file runs.
# ============================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(htmltools)
library(scales)

# -----------------------------
# File paths
# -----------------------------
GRID_PATH   <- file.path("data", "processed", "app", "ca_grid_10km.gpkg")
POINTS_PATH <- file.path("data", "processed", "app", "bat_points_app.rds")
COVS_PATH   <- file.path("data", "processed", "app", "grid_covariates_app.csv")

# -----------------------------
# Validate inputs
# -----------------------------
if (!file.exists(GRID_PATH))   stop("Missing grid file: ", GRID_PATH)
if (!file.exists(POINTS_PATH)) stop("Missing bat points file: ", POINTS_PATH)
if (!file.exists(COVS_PATH))   stop("Missing covariates file: ", COVS_PATH)

# -----------------------------
# Load & prepare data
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

bat_points <- bat_points |>
  st_transform(4326) |>
  mutate(
    gbif_id       = as.character(gbif_id),
    species       = as.character(species),
    species_label = as.character(species_label),
    year          = as.integer(year)
  )

covs <- read_csv(COVS_PATH, show_col_types = FALSE) |>
  mutate(
    cell_id       = as.character(cell_id),
    mean_radiance = as.numeric(mean_radiance),
    pct_developed = as.numeric(pct_developed),
    pct_protected = as.numeric(pct_protected),
    pop_density   = as.numeric(pop_density)
  )

# Join grid + covariates (done once, used by every session)
app_grid <- grid |>
  left_join(covs, by = "cell_id")

# -----------------------------
# UI filter choices
# -----------------------------
species_choices <- bat_points |>
  st_drop_geometry() |>
  distinct(species_label) |>
  arrange(species_label) |>
  pull(species_label)

year_range <- range(bat_points$year, na.rm = TRUE)

# layer_choices is defined in R/layer_config.R

# -----------------------------
# Species colour palette
# -----------------------------
# Built dynamically — scales to any number of species.
# Uses curated colours for n <= 6, then colorspace::qualitative_hcl().
# See R/species_palette.R for details.
species_palette <- make_species_palette(species_choices)
)