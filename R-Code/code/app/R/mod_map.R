# ============================================================
# mod_map.R
# Shiny-Modul: Leaflet-Karte mit Proxy-Updates
# ============================================================

map_ui <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map"), height = "88vh")
}

map_server <- function(id, sidebar, mun_data, mun_dorling_data,
                       layer_registry, relief_raster = NULL, lang = reactive("de")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Basis-Karte (einmalig gerendert) ---
    output$map <- renderLeaflet({
      m <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron)

      if (!is.null(relief_raster)) {
        m <- m %>%
          addRasterImage(relief_raster,
                         colors = grey.colors(256, start = 0.55, end = 1),
                         opacity = 0.4,
                         group = "relief",
                         project = FALSE)
      }

      m %>% setView(lng = 8.2, lat = 46.8, zoom = 8)
    })

    # --- Aktiver Layer ---
    active_config <- reactive({
      req(sidebar$active_layer_id())
      layer_registry[[sidebar$active_layer_id()]]
    })

    # --- Slider-Daten (nur für Pro-Mode Slider) ---
    slider_data <- reactive({
      req(sidebar$active_layer_id() == "slider")
      req(sidebar$slider_mode())

      dat <- mun_data

      if (sidebar$slider_mode() == "rent") {
        rent_val <- sidebar$rent_val()
        # Interpolation: Anteil bei gegebener Miete
        share_cols <- paste0("share_", seq(500, 6500, by = 500))
        rent_levels <- seq(500, 6500, by = 500)

        dat$slider_val <- sapply(seq_len(nrow(dat)), function(i) {
          shares <- as.numeric(st_drop_geometry(dat[i, share_cols]))
          valid <- !is.na(shares)
          if (sum(valid) < 2) return(NA_real_)
          approx(rent_levels[valid], shares[valid], xout = rent_val, rule = 2)$y
        })
        dat$slider_label <- "Anteil Haushalte"

      } else {
        share_val <- sidebar$share_val()
        rent_levels <- seq(500, 6500, by = 500)
        share_cols <- paste0("share_", rent_levels)

        dat$slider_val <- sapply(seq_len(nrow(dat)), function(i) {
          shares <- as.numeric(st_drop_geometry(dat[i, share_cols]))
          valid <- !is.na(shares)
          if (sum(valid) < 2) return(NA_real_)
          approx(shares[valid], rent_levels[valid], xout = share_val, rule = 1)$y
        })
        dat$slider_label <- "Miete (CHF)"
      }

      # Farbklassierung
      valid_vals <- dat$slider_val[!is.na(dat$slider_val)]
      if (length(valid_vals) < 2) return(dat)

      brks <- classIntervals(valid_vals, n = 7, style = "quantile")$brks
      pal  <- colorRampPalette(SLIDER_BLUES)(7)
      labeled <- cut(dat$slider_val, breaks = brks, include.lowest = TRUE, labels = pal)
      dat$slider_color <- as.character(labeled)
      dat
    })

    # --- Karte aktualisieren bei Layer-Wechsel ---
    observe({
      cfg <- active_config()
      req(cfg)

      use_dorling <- cfg$has_dorling && isTRUE(sidebar$dorling_on())

      # Welche Daten verwenden?
      if (cfg$id == "slider") {
        map_sf <- slider_data()
        color_col <- "slider_color"
      } else if (cfg$id == "median_slider") {
        map_sf <- mun_data
        size_key <- sidebar$median_size()
        if (is.null(size_key)) size_key <- "3.5zi"
        size_cfg <- MEDIAN_SIZES[[size_key]]
        color_col <- paste0(size_cfg$col, "_color")
      } else if (use_dorling) {
        map_sf <- mun_dorling_data
        color_col <- cfg$color_col
      } else {
        map_sf <- mun_data
        color_col <- cfg$color_col
      }

      # Legende
      l <- lang()
      legend_html <- cfg$legend_html
      if (cfg$id == "median_slider") {
        size_key <- sidebar$median_size()
        if (is.null(size_key)) size_key <- "3.5zi"
        size_cfg <- MEDIAN_SIZES[[size_key]]
        vals <- mun_data[[size_cfg$col]]
        valid <- vals[!is.na(vals)]
        if (length(valid) >= 2) {
          brks <- classIntervals(valid, n = 6, style = "equal")$brks
          pal  <- colorRampPalette(COLORS_MEDIAN)(6)
          legend_html <- make_standard_legend(brks, pal,
            paste0(tr("legend_median_prefix", l), " ", size_cfg$label), suffix = " CHF")
        }
      }
      if (cfg$id == "slider") {
        valid_vals <- map_sf$slider_val[!is.na(map_sf$slider_val)]
        if (length(valid_vals) >= 2) {
          brks <- classIntervals(valid_vals, n = 7, style = "quantile")$brks
          pal  <- colorRampPalette(SLIDER_BLUES)(7)
          lbl <- if (sidebar$slider_mode() == "rent") {
            paste0(tr("legend_share_at", l), " ", sidebar$rent_val(), " CHF")
          } else {
            paste0(tr("legend_rent_at", l), " ", round(sidebar$share_val() * 100), "%")
          }
          legend_html <- make_standard_legend(brks, pal, lbl)
        }
      }

      # Farben als Spalte ans sf-Objekt (Leaflet braucht Formel ~col)
      map_sf$fill_color_temp <- map_sf[[color_col]]
      map_sf$fill_color_temp[is.na(map_sf$fill_color_temp)] <- "#f5f5f5"

      proxy <- leafletProxy(ns("map")) %>%
        clearGroup("choropleth") %>%
        removeControl("legend")

      proxy %>%
        addPolygons(
          data            = map_sf,
          fillColor       = ~fill_color_temp,
          fillOpacity     = 0.7,
          color           = "#666666",
          weight          = 0.3,
          opacity         = 0.5,
          smoothFactor    = 0,
          group           = "choropleth",
          layerId         = ~bfs_nr,
          label           = ~municipality,
          highlightOptions = highlightOptions(
            weight = 2, color = "#333333",
            fillOpacity = 0.9, bringToFront = TRUE
          )
        )

      if (!is.null(legend_html)) {
        proxy %>%
          addControl(html = as.character(legend_html),
                     position = "bottomleft", layerId = "legend")
      }
    })

    # --- Klick-Handler: gibt reactive click_info zurück ---
    click_info <- reactiveVal(NULL)

    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      if (is.null(click$id)) return()

      bfs <- click$id
      cfg <- active_config()

      # Daten-Quelle je nach Layer
      if (cfg$id == "slider") {
        dat <- slider_data()
      } else if (cfg$has_dorling && isTRUE(sidebar$dorling_on())) {
        dat <- mun_dorling_data
      } else {
        dat <- mun_data
      }

      row <- dat[dat$bfs_nr == bfs, ]
      if (nrow(row) == 0) return()

      l <- lang()
      popup_cols <- cfg$popup_cols
      popup_labels <- sapply(cfg$popup_label_keys, function(k) tr(k, l))

      # Bivariate: Key "1-3" übersetzen in lesbare Labels
      if (cfg$id %in% c("bivariate_v1", "bivariate_v2")) {
        biv_key <- row[[cfg$value_col]][1]
        if (!is.na(biv_key) && grepl("-", biv_key)) {
          parts <- as.integer(strsplit(biv_key, "-")[[1]])
          leer_labels <- c(tr("biv_low", l), tr("biv_mid", l), tr("biv_high", l))
          leist_labels <- if (cfg$id == "bivariate_v1") {
            c(tr("biv_high", l), tr("biv_mid", l), tr("biv_low", l))
          } else {
            c(tr("biv_low", l), tr("biv_mid", l), tr("biv_high", l))
          }
          n_cls <- if (cfg$id == "bivariate_v1") 3 else 4
          if (n_cls == 4) {
            leer_labels <- leist_labels <- c(
              tr("biv_low", l), tr("biv_mid_low", l),
              tr("biv_mid_high", l), tr("biv_high", l))
          }
          row$leerstand_label <- leer_labels[parts[1]]
          row$leistbarkeit_label <- leist_labels[parts[2]]
        } else {
          row$leerstand_label <- NA
          row$leistbarkeit_label <- NA
        }
        popup_cols <- c("municipality", "leerstand_label", "leistbarkeit_label",
                        "Leerwohnungsziffer.2025", "Share_avg_rent_weighted_pct")
        popup_labels <- c(tr("popup_municipality", l), tr("popup_vacancy_label", l),
                          tr("popup_afford_label", l), tr("popup_vacancy_pct", l),
                          tr("popup_share_pct", l))
      }

      # Median-Slider: dynamische Popup-Spalten
      if (cfg$id == "median_slider") {
        size_key <- sidebar$median_size()
        if (is.null(size_key)) size_key <- "3.5zi"
        size_cfg <- MEDIAN_SIZES[[size_key]]
        popup_cols <- c("municipality", size_cfg$col, size_cfg$n_col)
        popup_labels <- c(tr("popup_municipality", l),
                          paste0(tr("popup_median_rent", l), " ", size_cfg$label, " (CHF)"),
                          tr("popup_n_inserate", l))
      }

      # Slider: zusätzliche Popup-Info
      if (cfg$id == "slider" && "slider_val" %in% names(row)) {
        row$slider_val_rounded <- round(row$slider_val, 2)
        popup_cols <- c(popup_cols, "slider_val_rounded")
        popup_labels <- c(popup_labels,
          if (sidebar$slider_mode() == "rent") tr("popup_share_households", l)
          else tr("popup_rent_chf", l))
      }

      info_df <- st_drop_geometry(row)[, popup_cols, drop = FALSE]

      click_info(list(
        municipality = row$municipality[1],
        labels = popup_labels,
        values = as.character(unlist(info_df[1, ]))
      ))
    })

    # Rückgabe: click_info reactive
    click_info
  })
}
