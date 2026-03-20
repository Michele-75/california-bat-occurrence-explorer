# ============================================================
# R/species_key.R
# Custom HTML species legend for the map
# ============================================================

make_species_key_html <- function(species_counts, species_palette) {
  rows <- lapply(seq_len(nrow(species_counts)), function(i) {
    sp  <- species_counts$species_label[i]
    n   <- species_counts$n[i]
    col <- species_palette[[sp]]
    
    tags$div(
      style = "display:flex; align-items:center; margin-bottom:6px;",
      tags$div(
        style = paste0(
          "width:14px; height:14px; border-radius:50%; margin-right:8px; ",
          "background:", col, "; border:1px solid #333; flex-shrink:0;"
        )
      ),
      tags$span(paste0(sp, " (", scales::comma(n), ")"))
    )
  })
  
  as.character(
    tags$div(
      style = paste0(
        "background: rgba(255,255,255,0.95); ",
        "padding: 8px 10px; border-radius: 4px; ",
        "box-shadow: 0 1px 5px rgba(0,0,0,0.3); font-size: 13px;"
      ),
      tags$div(
        style = "font-weight: 600; margin-bottom: 6px;",
        "Species key"
      ),
      rows
    )
  )
}