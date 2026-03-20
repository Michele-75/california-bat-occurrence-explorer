# ============================================================
# R/popups.R
# Popup builders for grid cells and observation points
# ============================================================

# Grid cell popup — shows all covariates, active layer in bold.
# Returns a character vector (one string per row).
#
# Strategy: preformat all covariate strings as vectors, then
# paste row-wise.  Much faster than lapply row-by-row for
# thousands of grid cells.

make_grid_popup <- function(dat, active_var) {
  all_vars <- c("pop_density", "mean_radiance", "pct_developed", "pct_protected")
  
  # Build a matrix: one column per variable, one row per grid cell.
  line_matrix <- vapply(all_vars, function(v) {
    vals <- vapply(dat[[v]], format_layer_value, character(1), var = v)
    lab  <- pretty_layer_name(v)
    
    if (v == active_var) {
      sprintf("<strong>%s: %s</strong>", lab, vals)
    } else {
      sprintf("%s: %s", lab, vals)
    }
  }, character(nrow(dat)))
  
  # line_matrix is nrow(dat) x length(all_vars)
  # Paste each row's columns together with <br/>
  cov_lines <- apply(line_matrix, 1, paste, collapse = "<br/>")
  
  sprintf(
    "<strong>Cell ID:</strong> %s<br/>%s",
    dat$cell_id,
    cov_lines
  )
}

# Bat observation popup — one string per point (fully vectorised).
make_point_popup <- function(dat) {
  sprintf(
    paste0(
      "<strong>Species:</strong> %s<br/>",
      "<strong>Year:</strong> %s<br/>",
      "<strong>GBIF ID:</strong> %s"
    ),
    dat$species_label,
    dat$year,
    dat$gbif_id
  )
}