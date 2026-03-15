
# ============================================================

# 00_setup_app.R

# ------------------------------------------------------------

# Setup script for the California Bat Interactive Map project.

#

# Responsibilities:

# - Load required packages

# - Set seed for reproducibility

# - Define directory paths

# - Create directories if they do not exist

#

# All preprocessing scripts for the app should begin with:

# source(here::here("processing_scripts", "00_setup_app.R"))

# ============================================================

# -----------------------------

# Load libraries

# -----------------------------

library(tidyverse)
library(sf)
library(terra)
library(here)

# -----------------------------

# Reproducibility

# -----------------------------

set.seed(123)

# -----------------------------

# Root directories

# -----------------------------

DIR_RAW        <- here("data", "raw")
DIR_PROCESSED  <- here("data", "processed")

DIR_APP        <- here("data", "processed", "app")

# -----------------------------

# Subdirectories

# -----------------------------


DIR_GBIF_RAW   <- here("data", "raw", "gbif")
DIR_COV_RAW    <- here("data", "raw", "covariates")
DIR_APP_PROC   <- here("data", "processed", "app")

# -----------------------------

# Create directories if missing

# -----------------------------

dir.create(DIR_RAW, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_PROCESSED, showWarnings = FALSE, recursive = TRUE)

dir.create(DIR_GBIF_RAW, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_COV_RAW, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_APP_PROC, showWarnings = FALSE, recursive = TRUE)

# -----------------------------

# App output file paths

# -----------------------------

FILE_CA_GRID       <- file.path(DIR_APP_PROC, "ca_grid_10km.gpkg")
FILE_BAT_POINTS    <- file.path(DIR_APP_PROC, "bat_points_app.rds")
FILE_GRID_COVS     <- file.path(DIR_APP_PROC, "grid_covariates_app.csv")

# -----------------------------

# Inform user setup completed

# -----------------------------

message("App setup loaded successfully.")

