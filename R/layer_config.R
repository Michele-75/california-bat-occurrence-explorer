# ============================================================
# R/layer_config.R
# Layer metadata, palette construction, and value formatting
# ============================================================

# Background layer choices (order determines sidebar display order)
layer_choices <- c(
  "Population density" = "pop_density",
  "Light pollution"    = "mean_radiance",
  "Developed land (%)" = "pct_developed",
  "Protected area (%)" = "pct_protected"
)

# Metadata for each covariate layer.
#
# Log-transformed variables use colorBin with explicit breaks in
# original units.  This avoids the misleading legend that
# colorNumeric + back-transformed labels produces — where the
# smooth gradient implies even spacing but the labels are uneven.
#
# Percentage variables keep colorNumeric — no transform needed.

layer_meta <- function(var) {
  switch(
    var,
    
    # ----- LOG-TRANSFORMED: colorBin with explicit breaks -----
    
    "mean_radiance" = list(
      label        = "Light pollution",
      legend_title = "Light pollution (nW/cm\u00B2/sr)",
      digits       = 2,
      pal_type     = "bin",
      breaks       = c(0, 0.5, 1, 2, 5, 10, 25, 50, 100, Inf),
      transform    = log1p,
      bin_labels   = c(
        "0 \u2013 0.5", "0.5 \u2013 1", "1 \u2013 2", "2 \u2013 5",
        "5 \u2013 10", "10 \u2013 25", "25 \u2013 50", "50 \u2013 100", "> 100"
      )
    ),
    
    "pop_density" = list(
      label        = "Population density",
      legend_title = "Population density (people/km\u00B2)",
      digits       = 0,
      pal_type     = "bin",
      breaks       = c(0, 1, 10, 100, 1000, 5000, Inf),
      transform    = log1p,
      bin_labels   = c(
        "0 \u2013 1", "1 \u2013 10", "10 \u2013 100",
        "100 \u2013 1,000", "1,000 \u2013 5,000", "> 5,000"
      )
    ),
    
    # ----- UNTRANSFORMED: colorNumeric -----
    
    "pct_developed" = list(
      label        = "Developed land (%)",
      legend_title = "Developed land (%)",
      digits       = 1,
      pal_type     = "numeric",
      transform    = identity,
      domain       = c(0, 100.1),
      legend_domain = c(0, 100)
    ),
    
    "pct_protected" = list(
      label        = "Protected area (%)",
      legend_title = "Protected area (%)",
      digits       = 1,
      pal_type     = "numeric",
      transform    = identity,
      domain       = c(0, 100.1),
      legend_domain = c(0, 100)
    ),
    
    stop("Unknown layer: ", var)
  )
}

# Short display name for a layer variable
pretty_layer_name <- function(var) {
  layer_meta(var)$label
}

# Format a single value for popup display (always original units)
format_layer_value <- function(x, var) {
  if (is.na(x)) return("NA")
  
  switch(
    var,
    "pct_developed" = ,
    "pct_protected" = sprintf("%.1f%%", x),
    "mean_radiance" = paste0(sprintf("%.2f", x), " nW/cm\u00B2/sr"),
    "pop_density"   = paste0(scales::comma(round(x, 0)), " people/km\u00B2"),
    as.character(x)
  )
}

# Build a leaflet colour palette for a given variable.
# Returns a list:
#   pal    — colour function that accepts raw (untransformed) values
#   pal_fn — the underlying leaflet palette (for legend building)
#   type   — "bin" or "numeric"
make_palette <- function(x, var) {
  meta <- layer_meta(var)
  
  if (meta$pal_type == "bin") {
    transformed_breaks <- meta$transform(meta$breaks)
    
    pal_fn <- colorBin(
      palette  = "viridis",
      domain   = meta$transform(x),
      bins     = transformed_breaks,
      na.color = "transparent"
    )
    
    # Wrap so callers pass raw values
    wrapped <- function(raw_vals) pal_fn(meta$transform(raw_vals))
    list(pal = wrapped, pal_fn = pal_fn, type = "bin")
    
  } else {
    dom <- if (!is.null(meta$domain)) meta$domain else range(x, na.rm = TRUE)
    pal_fn <- colorNumeric(
      palette  = "viridis",
      domain   = dom,
      na.color = "transparent"
    )
    list(pal = pal_fn, pal_fn = pal_fn, type = "numeric")
  }
}