# ============================================================
# scripts/05_add_species.R
#
# Add new bat species to the existing app dataset.
# Only downloads and processes the NEW species, then merges
# them with the existing bat_points_app.rds.
#
# After running this, re-run 04_make_app_inputs.R to
# re-standardise the app inputs, then restart the Shiny app.
#
# Run with:
#   source(here::here("scripts", "05_add_species.R"))
# ============================================================

source(here::here("scripts", "00_setup_app.R"))

library(rgbif)
library(janitor)
library(dplyr)
library(readr)
library(sf)
library(purrr)
library(tibble)

# ---- Species to ADD (only new ones) ----
# Pick California bat species with decent GBIF coverage.
# Some good candidates:
#   "Tadarida brasiliensis"   — Mexican free-tailed bat (very common)
#   "Eptesicus fuscus"        — Big brown bat
#   "Antrozous pallidus"      — Pallid bat
#   "Myotis lucifugus"        — Little brown bat
#   "Parastrellus hesperus"   — Canyon bat
#   "Corynorhinus townsendii" — Townsend's big-eared bat
#   "Myotis velifer"          — Cave myotis
#   "Nyctinomops macrotis"    — Big free-tailed bat

NEW_SPECIES <- c(
  "Tadarida brasiliensis",
  "Eptesicus fuscus",
  "Antrozous pallidus"
  #"Parastrellus hesperus"
)

MIN_YEAR <- 2012L

# ---- Check what already exists ----
if (!file.exists(FILE_BAT_POINTS)) {
  stop("Existing bat points file not found: ", FILE_BAT_POINTS,
       "\nRun 01_get_clean_bat_points_app.R first.")
}

existing_pts <- readRDS(FILE_BAT_POINTS)
existing_species <- unique(existing_pts$species)

message("Existing species: ", paste(existing_species, collapse = ", "))

# Remove any species that are already in the dataset
truly_new <- setdiff(NEW_SPECIES, existing_species)

if (length(truly_new) == 0) {
  message("All requested species are already in the dataset. Nothing to do.")
  # Still useful to know:
  message("Current species count: ", length(existing_species))
  stop("No new species to add.", call. = FALSE)
}

message("Adding ", length(truly_new), " new species: ",
        paste(truly_new, collapse = ", "))

# ---- GBIF credentials ----
gbif_user  <- Sys.getenv("GBIF_USER")
gbif_pwd   <- Sys.getenv("GBIF_PWD")
gbif_email <- Sys.getenv("GBIF_EMAIL")

if (gbif_user == "" || gbif_pwd == "" || gbif_email == "") {
  stop("Missing GBIF credentials. Add GBIF_USER, GBIF_PWD, and GBIF_EMAIL to .Renviron.")
}

# ---- Resolve to GBIF keys ----
new_taxa <- purrr::map_df(truly_new, ~ {
  rgbif::name_backbone(name = .x) |>
    as_tibble() |>
    select(scientificName, speciesKey)
})

new_keys <- new_taxa$speciesKey
message("Resolved GBIF keys: ", paste(new_keys, collapse = ", "))

# ---- Download from GBIF ----
dl <- occ_download(
  pred_in("taxonKey", new_keys),
  pred("hasCoordinate", TRUE),
  pred_gte("year", MIN_YEAR),
  format = "SIMPLE_CSV",
  user   = gbif_user,
  pwd    = gbif_pwd,
  email  = gbif_email
)

message("Waiting for GBIF download...")
occ_download_wait(dl)

gbif_zip <- occ_download_get(dl, path = DIR_GBIF_RAW, overwrite = TRUE)
new_raw <- occ_download_import(gbif_zip) |>
  as_tibble() |>
  janitor::clean_names()

message("Downloaded ", nrow(new_raw), " raw records for new species.")

# ---- Clean (same logic as script 01) ----
new_pts <- new_raw |>
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

# ---- Convert to sf and clip to California ----
new_pts_sf <- st_as_sf(
  new_pts,
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

inside_ca <- st_intersects(new_pts_sf, ca, sparse = FALSE)[, 1]
new_pts_sf <- new_pts_sf[inside_ca, ]

message("After cleaning and CA clip: ", nrow(new_pts_sf), " new records.")

# ---- Merge with existing data ----
combined <- bind_rows(existing_pts, new_pts_sf)

message("\nCombined dataset:")
message("  Total records: ", nrow(combined))
message("  Species: ", n_distinct(combined$species))

combined |>
  st_drop_geometry() |>
  count(species, name = "n") |>
  arrange(desc(n)) |>
  print()

# ---- Save ----
saveRDS(combined, FILE_BAT_POINTS)
message("\nSaved updated bat points to: ", FILE_BAT_POINTS)
message("Now re-run 04_make_app_inputs.R, then restart the Shiny app.")