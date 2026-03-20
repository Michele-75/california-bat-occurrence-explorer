# R/03_build_grid_covariates_app.R
# Run with:
# source(here::here("processing_scripts", "03_build_grid_covariates_app.R"))

source(here::here("processing_scripts", "00_setup_app.R"))

library(sf)
library(terra)
library(dplyr)
library(readr)
library(tibble)
library(purrr)
library(exactextractr)

# ---- Parameters ----
CA_EPSG <- 3310

# ---- Paths ----
GRID_GPKG <- file.path("data", "processed", "app", "ca_grid_10km.gpkg")
OUT_CSV   <- file.path("data", "processed", "app", "grid_covariates_app.csv")

VIIRS_DIR <- file.path("data", "raw", "covariates", "viirs")
NLCD_TIF  <- file.path("data", "raw", "covariates", "Annual_NLCD_LndCov_2019_CU_C1V1.tif")
PADUS_GDB <- file.path("data", "raw", "covariates", "PADUS4_1VectorAnalysis_PADUS_Only.gdb")
POP_TIF   <- file.path("data", "raw", "covariates", "population_density_2020.tif")

# ---- Checks ----
if (!file.exists(GRID_GPKG)) stop("Missing grid file: ", GRID_GPKG)
if (!dir.exists(VIIRS_DIR)) stop("Missing VIIRS directory: ", VIIRS_DIR)
if (!file.exists(NLCD_TIF)) stop("Missing NLCD raster: ", NLCD_TIF)
if (!dir.exists(PADUS_GDB)) stop("Missing PAD-US geodatabase folder: ", PADUS_GDB)
if (!file.exists(POP_TIF)) stop("Missing population density raster: ", POP_TIF)

VIIRS_FILES <- list.files(
  VIIRS_DIR,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(VIIRS_FILES) == 0) stop("No VIIRS .tif files found in: ", VIIRS_DIR)

# ---- Load grid ----
grid <- st_read(GRID_GPKG, layer = "ca_grid_10km", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(CA_EPSG)

if (!"cell_id" %in% names(grid)) stop("Grid must contain a cell_id column.")

# ============================================================
# 1) VIIRS mean radiance across all available years
# ============================================================

extract_viirs_mean_one <- function(tif_path) {
  r <- terra::rast(tif_path)
  
  # transform grid to raster CRS
  grid_r_crs <- st_transform(grid, terra::crs(r))
  
  # crop and mask first, while still in native raster CRS
  r_crop <- terra::crop(r, terra::vect(grid_r_crs)) |>
    terra::mask(terra::vect(grid_r_crs))
  
  # now reproject only the California subset
  r_ae <- terra::project(r_crop, paste0("EPSG:", CA_EPSG), method = "bilinear")
  
  vals <- exactextractr::exact_extract(r_ae, grid, "mean")
  
  tibble(
    cell_id = grid$cell_id,
    mean_radiance_year = vals
  )
}

viirs_long <- purrr::map_dfr(VIIRS_FILES, extract_viirs_mean_one)

viirs_cov <- viirs_long %>%
  group_by(cell_id) %>%
  summarise(
    mean_radiance = mean(mean_radiance_year, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 2) Percent developed from NLCD (faster binary approach)
# ============================================================

nlcd <- terra::rast(NLCD_TIF)

grid_nlcd <- terra::vect(st_transform(grid, terra::crs(nlcd)))
nlcd_crop <- terra::crop(nlcd, grid_nlcd) |> terra::mask(grid_nlcd)

# Recode NLCD to binary developed / not developed
# NLCD developed classes are 21, 22, 23, 24
nlcd_dev <- nlcd_crop %in% c(21, 22, 23, 24)

# Mean of 0/1 raster within each grid cell = proportion developed
dev_mean <- exactextractr::exact_extract(
  nlcd_dev,
  st_transform(grid, terra::crs(nlcd)),
  "mean"
)

dev_cov <- tibble(
  cell_id = grid$cell_id,
  pct_developed = 100 * dev_mean
)

# ============================================================
# 3) Percent protected from PAD-US (GAP 1-3)
# ============================================================

padus_layers <- sf::st_layers(PADUS_GDB)
PADUS_LAYER <- padus_layers$name[[1]]
GAP_FIELD <- "GAP_Sts"

padus <- st_read(PADUS_GDB, layer = PADUS_LAYER, quiet = TRUE)

if (!(GAP_FIELD %in% names(padus))) {
  stop("Expected PAD-US field not found: ", GAP_FIELD)
}

# Filter GAP first, before expensive spatial work
gap_vals <- suppressWarnings(as.integer(padus[[GAP_FIELD]]))
padus <- padus[!is.na(gap_vals) & gap_vals %in% c(1, 2, 3), ]

if (nrow(padus) == 0) {
  prot_cov <- tibble(cell_id = grid$cell_id, pct_protected = NA_real_)
} else {
  # Only fix invalid geometries if needed
  bad <- !st_is_valid(padus)
  if (any(bad)) {
    padus[bad, ] <- st_make_valid(padus[bad, ])
  }
  
  padus <- st_transform(padus, st_crs(grid))
  
  # Keep only polygons that intersect the grid, using sparse output
  hits <- st_intersects(padus, grid)
  padus <- padus[lengths(hits) > 0, ]
  
  inter <- st_intersection(
    grid %>% select(cell_id),
    padus
  )
  
  inter_area <- inter %>%
    mutate(area_m2 = as.numeric(st_area(.))) %>%
    st_drop_geometry() %>%
    group_by(cell_id) %>%
    summarise(protected_area_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop")
  
  grid_area <- grid %>%
    mutate(cell_area_m2 = as.numeric(st_area(.))) %>%
    st_drop_geometry() %>%
    select(cell_id, cell_area_m2)
  
  prot_cov <- grid_area %>%
    left_join(inter_area, by = "cell_id") %>%
    mutate(
      protected_area_m2 = coalesce(protected_area_m2, 0),
      pct_protected = 100 * protected_area_m2 / cell_area_m2
    ) %>%
    select(cell_id, pct_protected)
}

# ============================================================
# 4) Mean population density per grid cell
# ============================================================

pop <- terra::rast(POP_TIF)

grid_pop_crs <- st_transform(grid, terra::crs(pop))
pop_crop <- terra::crop(pop, terra::vect(grid_pop_crs)) |>
  terra::mask(terra::vect(grid_pop_crs))

pop_ae <- terra::project(pop_crop, paste0("EPSG:", CA_EPSG), method = "bilinear")


pop_cov <- tibble(
  cell_id = grid$cell_id,
  pop_density = exactextractr::exact_extract(pop_ae, grid, "mean")
)

# ============================================================
# Combine and save
# ============================================================

covs <- tibble(cell_id = grid$cell_id) %>%
  left_join(viirs_cov, by = "cell_id") %>%
  left_join(dev_cov, by = "cell_id") %>%
  left_join(prot_cov, by = "cell_id") %>%
  left_join(pop_cov, by = "cell_id") %>%
  arrange(cell_id)

readr::write_csv(covs, OUT_CSV)

message("Saved grid covariates to: ", OUT_CSV)
message("Rows: ", nrow(covs))
