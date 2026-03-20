# R/02_build_ca_grid_10km_app.R
# Run with:
# source(here::here("scripts", "02_build_ca_grid_10km_app.R"))

source(here::here("scripts", "00_setup_app.R"))

library(sf)
library(tigris)
library(dplyr)

# ---- Parameters ----
GRID_SIZE_M <- 10000
CA_EPSG <- 3310

options(tigris_use_cache = TRUE)

# ---- Download California boundary ----
ca <- tigris::states(cb = TRUE, year = 2022) %>%
  st_as_sf() %>%
  filter(STUSPS == "CA") %>%
  select(STUSPS, NAME) %>%
  st_make_valid() %>%
  st_transform(CA_EPSG)

# ---- Build 10 km grid over California extent ----
grid <- st_make_grid(
  ca,
  cellsize = GRID_SIZE_M,
  square = TRUE
) %>%
  st_as_sf() %>%
  mutate(cell_id = row_number())

# ---- Keep only grid cells that intersect California ----
ca_grid <- st_intersection(grid, ca) %>%
  select(cell_id)

# ---- Write output ----
if (file.exists(FILE_CA_GRID)) file.remove(FILE_CA_GRID)

st_write(
  ca_grid,
  FILE_CA_GRID,
  layer = "ca_grid_10km",
  quiet = TRUE
)

message("Saved California 10 km grid to: ", FILE_CA_GRID)
message("Number of grid cells: ", nrow(ca_grid))
