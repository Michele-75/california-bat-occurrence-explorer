# ============================================================
# server.R
# Reactive logic, map rendering, and observers
# ============================================================

function(input, output, session) {
  
  # ---- Filtered observation points ----
  filtered_points <- reactive({
    req(input$species_filter, input$year_mode)
    
    if (input$year_mode == "animate") {
      req(input$year_animate)
      bat_points |>
        filter(
          species_label %in% input$species_filter,
          year <= input$year_animate
        )
    } else {
      req(input$year_range)
      bat_points |>
        filter(
          species_label %in% input$species_filter,
          year >= input$year_range[1],
          year <= input$year_range[2]
        )
    }
  })
  
  # ---- Base map (rendered once) ----
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addMapPane("gridPane", zIndex = 410) |>
      addMapPane("pointPane", zIndex = 420) |>
      setView(lng = -119.5, lat = 37.2, zoom = 6) |>
      # Zoom-to-area: hold Shift and drag to draw a rectangle.
      # Leaflet has this built in (L.Map.BoxZoom), but it's bound
      # to Shift+drag by default and there's no visual hint.
      # This onRender adds a small instruction label and ensures
      # the behaviour is active.
      htmlwidgets::onRender("
        function(el, x) {
          // Leaflet's built-in box zoom is enabled by default with Shift+drag.
          // Add a hint control so users know the feature exists.
          var hint = L.control({ position: 'topleft' });
          hint.onAdd = function(map) {
            var div = L.DomUtil.create('div', 'leaflet-control');
            div.innerHTML = '<div style=\"background:rgba(255,255,255,0.9);padding:4px 8px;border-radius:4px;font-size:11px;box-shadow:0 1px 4px rgba(0,0,0,0.3);color:#333;\">Shift + drag to zoom to area</div>';
            return div;
          };
          hint.addTo(this);
        }
      ")
  })
  
  # ---- Grid layer observer ----
  # Redraws polygons when covariate layer OR opacity changes.
  observe({
    req(input$covariate_layer, input$grid_opacity)
    
    var     <- input$covariate_layer
    meta    <- layer_meta(var)
    pal_obj <- make_palette(app_grid[[var]], var)
    
    fill_vals <- pal_obj$pal(app_grid[[var]])
    
    proxy <- leafletProxy("map", data = app_grid) |>
      clearGroup("grid") |>
      removeControl("bg_legend") |>
      addPolygons(
        layerId     = app_grid$cell_id,
        fillColor   = fill_vals,
        fillOpacity = input$grid_opacity,
        color       = "#666666",
        weight      = 0.3,
        popup       = make_grid_popup(app_grid, var),
        group       = "grid",
        options     = pathOptions(pane = "gridPane")
      )
    
    # Add legend — binned or continuous depending on layer type
    if (pal_obj$type == "bin") {
      proxy |>
        addLegend(
          position = "bottomright",
          colors   = vapply(
            seq_len(length(meta$breaks) - 1),
            function(i) {
              mid <- (meta$transform(meta$breaks[i]) +
                        meta$transform(meta$breaks[i + 1])) / 2
              pal_obj$pal_fn(mid)
            },
            character(1)
          ),
          labels  = meta$bin_labels,
          title   = meta$legend_title,
          opacity = 1,
          layerId = "bg_legend"
        )
    } else {
      proxy |>
        addLegend(
          position  = "bottomright",
          pal       = pal_obj$pal_fn,
          values    = app_grid[[var]],
          title     = meta$legend_title,
          opacity   = 1,
          labFormat = labelFormat(digits = meta$digits),
          layerId   = "bg_legend"
        )
    }
  })
  
  # ---- Observation points observer ----
  observe({
    pts <- filtered_points()
    
    leafletProxy("map") |>
      clearGroup("points") |>
      removeControl("species_key_control")
    
    if (isTRUE(input$show_points) && nrow(pts) > 0) {
      point_colors <- unname(species_palette[pts$species_label])
      
      species_counts <- pts |>
        st_drop_geometry() |>
        count(species_label, name = "n") |>
        arrange(match(species_label, input$species_filter))
      
      species_key_html <- make_species_key_html(species_counts, species_palette)
      
      leafletProxy("map", data = pts) |>
        addCircleMarkers(
          radius      = 4,
          stroke      = TRUE,
          color       = "#222222",
          weight      = 1,
          fillColor   = point_colors,
          fillOpacity = 0.95,
          popup       = make_point_popup(pts),
          group       = "points",
          options     = pathOptions(pane = "pointPane")
        ) |>
        addControl(
          html     = species_key_html,
          position = "topright",
          layerId  = "species_key_control"
        )
    }
  })
}