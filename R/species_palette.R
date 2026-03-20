# ============================================================
# R/species_palette.R
# Dynamic species colour palette
# ============================================================

#' Build a named colour palette for an arbitrary number of species.
#'
#' Uses colorspace::qualitative_hcl() to generate perceptually
#' uniform, distinguishable colours for any n.  Falls back to a
#' hand-picked palette for n <= 6 (these were chosen for high
#' contrast against viridis backgrounds).
#'
#' @param species_labels Character vector of unique species names
#'   (sorted — determines colour assignment order).
#' @return Named character vector: names = species labels, values = hex colours.

make_species_palette <- function(species_labels) {
  n <- length(species_labels)
  
  if (n == 0) {
    return(character(0))
  }
  
  # Hand-picked colours optimised for contrast on viridis backgrounds.
  # Used when the species count is small enough to benefit from curation.
  curated <- c(
    "#FF4E3A",
    "#00B0F6",
    "#FFD23F",
    "#E76BF3",
    "#39B600",
    "#F98400"
  )
  
  if (n <= length(curated)) {
    cols <- curated[seq_len(n)]
  } else {
    if (!requireNamespace("colorspace", quietly = TRUE)) {
      stop(
        "Package 'colorspace' is required when more than ",
        length(curated), " species are present. ",
        "Install it with install.packages('colorspace')."
      )
    }
    cols <- colorspace::qualitative_hcl(n, palette = "Dark 3")
    
    if (n > 12) {
      message(
        "Note: ", n, " species in palette. Beyond ~12 colours, ",
        "individual species become difficult to distinguish visually."
      )
    }
  }
  
  setNames(cols, species_labels)
}