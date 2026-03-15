# R/04_make_app_inputs.R
# Run with:
# source(here::here("R", "04_make_app_inputs.R"))

source(here::here("R", "00_setup_app.R"))

library(sf)
library(dplyr)
library(readr)
library(tibble)

# ---- Paths ----
GRID_GPKG   <- file.path("data", "processed", "app", "ca_grid_10km.gpkg")
POINTS_RDS  <- file.path("data", "processed", "app", "bat_points_app.rds")
COVS_CSV    <- file.path("data", "processed", "app", "grid_covariates_app.csv")

OUT_GRID_GPKG  <- file.path("data", "processed", "app", "ca_grid_10km.gpkg")
OUT_POINTS_RDS <- file.path("data", "processed", "app", "bat_points_app.rds")
OUT_COVS_CSV   <- file.path("data", "processed", "app", "grid_covariates_app.csv")
OUT_QA_CSV     <- file.path("data", "processed", "app", "app_inputs_qa.csv")

# ---- Checks ----
if (!file.exists(GRID_GPKG))  stop("Missing grid file: ", GRID_GPKG)
if (!file.exists(POINTS_RDS)) stop("Missing bat points file: ", POINTS_RDS)
if (!file.exists(COVS_CSV))   stop("Missing covariates file: ", COVS_CSV)

# ---- Load inputs ----
grid <- st_read(GRID_GPKG, layer = "ca_grid_10km", quiet = TRUE) %>%
  st_make_valid()

pts <- readRDS(POINTS_RDS)
covs <- readr::read_csv(COVS_CSV, show_col_types = FALSE)

# ---- Required columns ----
if (!"cell_id" %in% names(grid)) {
  stop("Grid file must contain a 'cell_id' column.")
}

required_point_cols <- c("gbif_id", "species", "species_key", "year", "lon", "lat")
missing_point_cols <- setdiff(required_point_cols, names(pts))
if (length(missing_point_cols) > 0) {
  stop("Points file is missing required columns: ", paste(missing_point_cols, collapse = ", "))
}

required_cov_cols <- c(
  "cell_id",
  "mean_radiance",
  "pct_developed",
  "pct_protected",
  "pop_density"
)

missing_cov_cols <- setdiff(required_cov_cols, names(covs))
if (length(missing_cov_cols) > 0) {
  stop("Covariates file is missing required columns: ", paste(missing_cov_cols, collapse = ", "))
}

# ---- Standardize types ----
grid <- grid %>%
  mutate(cell_id = as.character(cell_id)) %>%
  select(cell_id)

pts <- pts %>%
  mutate(
    gbif_id = as.character(gbif_id),
    species = as.character(species),
    species_key = as.integer(species_key),
    year = as.integer(year),
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    species_label = species
  ) %>%
  select(gbif_id, species, species_label, species_key, year, lon, lat, geometry)

covs <- covs %>%
  mutate(
    cell_id = as.character(cell_id),
    mean_radiance = as.numeric(mean_radiance),
    pct_developed = as.numeric(pct_developed),
    pct_protected = as.numeric(pct_protected),
    pop_density = as.numeric(pop_density)
  ) %>%
  select(cell_id, mean_radiance, pct_developed, pct_protected, pop_density)

# ---- QA checks ----
n_grid <- nrow(grid)
n_grid_unique <- dplyr::n_distinct(grid$cell_id)

n_covs <- nrow(covs)
n_covs_unique <- dplyr::n_distinct(covs$cell_id)

grid_not_in_covs <- sum(!grid$cell_id %in% covs$cell_id)
covs_not_in_grid <- sum(!covs$cell_id %in% grid$cell_id)

n_pts <- nrow(pts)
year_min <- min(pts$year, na.rm = TRUE)
year_max <- max(pts$year, na.rm = TRUE)
n_species <- pts %>% st_drop_geometry() %>% distinct(species) %>% nrow()

qa <- tibble(
  metric = c(
    "n_grid_rows",
    "n_unique_grid_cell_id",
    "n_covariate_rows",
    "n_unique_covariate_cell_id",
    "grid_cell_ids_missing_in_covariates",
    "covariate_cell_ids_missing_in_grid",
    "n_bat_points",
    "n_species",
    "year_min",
    "year_max"
  ),
  value = c(
    n_grid,
    n_grid_unique,
    n_covs,
    n_covs_unique,
    grid_not_in_covs,
    covs_not_in_grid,
    n_pts,
    n_species,
    year_min,
    year_max
  )
)

# ---- Fail if key joins are broken ----
if (n_grid != n_grid_unique) {
  stop("Grid cell_id values are not unique.")
}

if (n_covs != n_covs_unique) {
  stop("Covariate cell_id values are not unique.")
}

if (grid_not_in_covs > 0) {
  stop("Some grid cell_id values are missing in the covariates table.")
}

# ---- Save standardized app inputs ----
if (file.exists(OUT_GRID_GPKG)) file.remove(OUT_GRID_GPKG)

st_write(
  grid,
  OUT_GRID_GPKG,
  layer = "ca_grid_10km",
  quiet = TRUE
)

saveRDS(pts, OUT_POINTS_RDS)
readr::write_csv(covs, OUT_COVS_CSV)
readr::write_csv(qa, OUT_QA_CSV)

message("Saved standardized grid:       ", OUT_GRID_GPKG)
message("Saved standardized bat points: ", OUT_POINTS_RDS)
message("Saved standardized covariates: ", OUT_COVS_CSV)
message("Saved QA summary:              ", OUT_QA_CSV)
