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
      htmlwidgets::onRender(sprintf("
        function(el, x) {
          var map = this;

          // Zoom hint
          var hint = L.control({ position: 'topleft' });
          hint.onAdd = function() {
            var div = L.DomUtil.create('div', 'leaflet-control');
            div.innerHTML = '<div style=\"background:rgba(255,255,255,0.9);padding:4px 8px;border-radius:4px;font-size:11px;box-shadow:0 1px 4px rgba(0,0,0,0.3);color:#333;\">Shift + drag to zoom to area</div>';
            return div;
          };
          hint.addTo(map);

          // Reset view button
          var resetBtn = L.control({ position: 'topleft' });
          resetBtn.onAdd = function() {
            var btn = L.DomUtil.create('div', 'leaflet-control');
            btn.innerHTML = '<button style=\"background:white;border:none;padding:6px 12px;border-radius:4px;font-size:12px;cursor:pointer;box-shadow:0 1px 4px rgba(0,0,0,0.3);color:#333;font-weight:500;\" onmouseover=\"this.style.background=\\'#f0f0f0\\'\" onmouseout=\"this.style.background=\\'white\\'\">%s Reset view</button>';
            L.DomEvent.disableClickPropagation(btn);
            btn.querySelector('button').addEventListener('click', function() {
              map.setView([%f, %f], %d);
            });
            return btn;
          };
          resetBtn.addTo(map);
        }
      ", "&#x21BA;", 37.2, -119.5, 6))
  })
  
  # ---- Reset filters: restore all sidebar inputs to defaults ----
  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "species_filter",
                         selected = species_choices)
    updateRadioButtons(session, "year_mode",
                       selected = "range")
    updateSliderInput(session, "year_range",
                      value = year_range)
    updateSliderInput(session, "year_animate",
                      value = year_range[1])
    updateSelectInput(session, "covariate_layer",
                      selected = "pop_density")
    updateCheckboxInput(session, "show_points",
                        value = TRUE)
    updateSliderInput(session, "grid_opacity",
                      value = 0.7)
    # Also reset the map view
    leafletProxy("map") |>
      setView(lng = -119.5, lat = 37.2, zoom = 6)
  })
  
  # ---- Grid layer observer ----
  # Redraws polygons only when covariate layer changes.
  # Opacity changes are handled separately via JavaScript (no redraw).
  observe({
    req(input$covariate_layer)
    
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
        fillOpacity = isolate(input$grid_opacity),
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
          values    = if (!is.null(meta$legend_domain)) meta$legend_domain
          else if (!is.null(meta$domain)) meta$domain
          else app_grid[[var]],
          title     = meta$legend_title,
          opacity   = 1,
          labFormat = labelFormat(digits = meta$digits),
          layerId   = "bg_legend"
        )
    }
  })
  
  # ---- Opacity observer ----
  # Sends opacity value to JavaScript for instant in-place update.
  # No polygon redraw needed.
  observeEvent(input$grid_opacity, {
    session$sendCustomMessage("updateGridOpacity", input$grid_opacity)
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