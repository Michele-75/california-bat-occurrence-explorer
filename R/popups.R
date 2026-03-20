# ============================================================
# R/popups.R
# Popup builders for grid cells and observation points
# ============================================================

# Grid cell popup — shows all covariates, active layer in bold.
# Returns a list of HTML strings, one per row.
make_grid_popup <- function(dat, active_var) {
  all_vars <- c("pop_density", "mean_radiance", "pct_developed", "pct_protected")
  
  lapply(seq_len(nrow(dat)), function(i) {
    lines <- vapply(all_vars, function(v) {
      val <- format_layer_value(dat[[v]][i], v)
      lab <- pretty_layer_name(v)
      if (v == active_var) {
        sprintf("<strong>%s: %s</strong>", lab, val)
      } else {
        sprintf("%s: %s", lab, val)
      }
    }, character(1))
    
    HTML(paste0(
      "<strong>Cell ID:</strong> ", dat$cell_id[i], "<br/>",
      paste(lines, collapse = "<br/>")
    ))
  })
}

# Bat observation popup — one per point.
make_point_popup <- function(dat) {
  lapply(seq_len(nrow(dat)), function(i) {
    HTML(sprintf(
      paste0(
        "<strong>Species:</strong> %s<br/>",
        "<strong>Year:</strong> %s<br/>",
        "<strong>GBIF ID:</strong> %s"
      ),
      dat$species_label[i],
      dat$year[i],
      dat$gbif_id[i]
    ))
  })
}