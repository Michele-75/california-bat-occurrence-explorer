# R/01_get_clean_bat_points_app.R
# Run with:
# source(here::here("scripts", "01_get_clean_bat_points_app.R"))

source(here::here("scripts", "00_setup_app.R"))

library(rgbif)
library(janitor)
library(dplyr)
library(readr)
library(sf)
library(purrr)
library(tibble)
library(usethis)

# ---- Parameters ----
BAT_SPECIES <- c(
  "Myotis yumanensis",
  "Myotis californicus",
  "Lasiurus cinereus"
)

MIN_YEAR <- 2012L

RAW_CSV <- file.path(DIR_GBIF_RAW, "bats_raw_app.csv")

# ---- GBIF credentials from .Renviron ----
gbif_user  <- Sys.getenv("GBIF_USER")
gbif_pwd   <- Sys.getenv("GBIF_PWD")
gbif_email <- Sys.getenv("GBIF_EMAIL")

if (gbif_user == "" || gbif_pwd == "" || gbif_email == "") {
  stop("Missing GBIF credentials. Add GBIF_USER, GBIF_PWD, and GBIF_EMAIL to .Renviron.")
}

# ---- Resolve species to GBIF keys ----
bat_taxa <- purrr::map_df(BAT_SPECIES, ~ {
  rgbif::name_backbone(name = .x) |>
    as_tibble() |>
    select(scientificName, speciesKey)
})

bat_keys <- bat_taxa$speciesKey

# ---- Download raw GBIF data ----
dl <- occ_download(
  pred_in("taxonKey", bat_keys),
  pred("hasCoordinate", TRUE),
  pred_gte("year", MIN_YEAR),
  format = "SIMPLE_CSV",
  user   = gbif_user,
  pwd    = gbif_pwd,
  email  = gbif_email
)

occ_download_wait(dl)

gbif_zip <- occ_download_get(dl, path = DIR_GBIF_RAW, overwrite = TRUE)

bats_raw <- occ_download_import(gbif_zip) |>
  as_tibble() |>
  janitor::clean_names()

readr::write_csv(bats_raw, RAW_CSV)

# ---- Keep only needed fields and clean ----
bat_points <- bats_raw |>
  transmute(
    gbif_id = as.character(gbif_id),
    species = as.character(species),
    species_key = suppressWarnings(as.integer(species_key)),
    year = suppressWarnings(as.integer(year)),
    lon = suppressWarnings(as.numeric(decimal_longitude)),
    lat = suppressWarnings(as.numeric(decimal_latitude))
  ) |>
  filter(
    !is.na(species),
    !is.na(year), year >= MIN_YEAR,
    !is.na(lon), !is.na(lat),
    lon != 0, lat != 0,
    lon >= -125, lon <= -113,
    lat >= 32, lat <= 42
  )

# ---- Convert to sf ----
bat_points_sf <- st_as_sf(
  bat_points,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

options(tigris_use_cache = TRUE)

ca <- tigris::states(cb = TRUE, year = 2022) |>
  st_as_sf() |>
  filter(STUSPS == "CA") |>
  st_make_valid() |>
  st_transform(4326)

inside_ca <- st_intersects(bat_points_sf, ca, sparse = FALSE)[, 1]

bat_points_sf <- bat_points_sf[inside_ca, ]

saveRDS(bat_points_sf, FILE_BAT_POINTS)

message("Saved app-ready bat points to: ", FILE_BAT_POINTS)
