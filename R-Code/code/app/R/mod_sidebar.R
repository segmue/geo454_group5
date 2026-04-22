# ============================================================
# mod_sidebar.R
# Shiny-Modul: Sidebar mit Tabs, Layer-Auswahl und Controls
# ============================================================

# Mapping discrete slider positions to room size keys
MEDIAN_SIZE_ORDER <- c("1.5zi", "2.5zi", "3.5zi", "4.5zi", "5.5zi", "6pluszi")
MEDIAN_SIZE_LABELS <- c("1.5-Zi.", "2.5-Zi.", "3.5-Zi.", "4.5-Zi.", "5.5-Zi.", "6+-Zi.")

# --- Navigation UI (upper part of sidebar) ---
sidebar_nav_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Row: Tabs + Pro toggle
    div(style = "display:flex; align-items:flex-end; gap:6px; margin-bottom:0;",
      # Register tabs
      div(style = "flex:1;",
        uiOutput(ns("tab_buttons"))
      ),
      # Simple/Pro Slide-Toggle
      div(style = "white-space:nowrap; padding-bottom:6px;",
        tags$label(class = "toggle-switch",
          style = "cursor:pointer;display:inline-flex;align-items:center;",
          tags$input(type = "checkbox", id = ns("pro_mode"),
                     class = "toggle-input",
                     style = "display:none;"),
          tags$span(class = "toggle-slider"),
          tags$span("Pro", style = "margin-left:6px;font-size:11px;color:#999;")
        )
      )
    ),

    # Hidden input to track active tab (managed by JS)
    tags$input(type = "hidden", id = ns("main_tabs"), value = "availability",
               class = "shiny-bound-input"),

    # Tab content frame (bordered area below tabs)
    div(class = "tab-content-frame",
      # Layer radio buttons (choices updated dynamically; hidden if single layer)
      uiOutput(ns("layer_radio_ui")),

      # Dorling-Checkbox (nur bei Bivariate V1)
      conditionalPanel(
        condition = sprintf("input['%s'] == 'bivariate_v1'", ns("layer")),
        uiOutput(ns("dorling_ui"))
      ),

      # Median-Zimmergrösse Slider (nur bei median_slider)
      conditionalPanel(
        condition = sprintf("input['%s'] == 'median_slider'", ns("layer")),
        uiOutput(ns("median_slider_ui")),
        uiOutput(ns("median_label"))
      ),

      # Slider-Controls (nur bei Slider-Layer)
      conditionalPanel(
        condition = sprintf("input['%s'] == 'slider'", ns("layer")),
        uiOutput(ns("slider_controls_ui"))
      )
    ),

    hr(),

    # Layer title (descriptive, below separator)
    uiOutput(ns("layer_title")),

    # Click-Info panel (municipality data on map click)
    uiOutput(ns("info_panel"))
  )
}

# --- Info Box UI (bottom of sidebar) ---
sidebar_info_ui <- function(id) {
  ns <- NS(id)
  div(class = "sidebar-info-box",
    div(class = "info-box-header",
      onclick = "toggleInfoBox(this)",
      tags$span(style = "font-weight:bold; font-size:12px;",
        uiOutput(ns("info_box_header"), inline = TRUE)
      ),
      tags$span(class = "info-toggle-icon", style = "font-size:14px; font-weight:bold;", "+")
    ),
    div(class = "info-box-body", style = "display:none;",
      uiOutput(ns("info_box_content"))
    )
  )
}

sidebar_server <- function(id, layer_registry, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Register-style tab buttons (rendered dynamically for i18n) ---
    output$tab_buttons <- renderUI({
      l <- lang()
      tabs <- c("availability", "affordability", "result")
      labels <- c(tr("tab_availability", l), tr("tab_affordability", l), tr("tab_result", l))
      active <- isolate(input$main_tabs) %||% "availability"

      tags$div(class = "nav-tabs-register",
        lapply(seq_along(tabs), function(i) {
          cls <- if (tabs[i] == active) "tab-item active" else "tab-item"
          tags$div(class = cls,
            onclick = sprintf("
              document.querySelectorAll('.nav-tabs-register .tab-item').forEach(function(t){t.classList.remove('active')});
              this.classList.add('active');
              Shiny.setInputValue('%s', '%s', {priority:'event'});
            ", ns("main_tabs"), tabs[i]),
            labels[i]
          )
        })
      )
    })

    # --- Reset to Easy mode on tab switch ---
    observeEvent(input$main_tabs, {
      updateCheckboxInput(session, "pro_mode", value = FALSE)
    }, ignoreInit = TRUE)

    # --- Layer radio buttons (dynamic based on tab + mode) ---
    # Hidden when only 1 layer available
    output$layer_radio_ui <- renderUI({
      l <- lang()
      tab <- input$main_tabs %||% "availability"
      pro <- isTRUE(input$pro_mode)
      mode_filter <- if (pro) c("simple", "pro") else "simple"

      layers <- Filter(function(lr) lr$tab == tab && lr$mode %in% mode_filter, layer_registry)
      layers <- layers[order(sapply(layers, `[[`, "order"))]

      if (length(layers) == 0) return(NULL)

      choices <- setNames(
        sapply(layers, `[[`, "id"),
        sapply(layers, function(lr) tr(lr$label_key, l))
      )

      if (length(choices) == 1) {
        # Single layer: don't show radio buttons, just set the value
        tagList(
          tags$script(sprintf(
            "Shiny.setInputValue('%s', '%s');",
            ns("layer"), choices[1]
          ))
        )
      } else {
        radioButtons(ns("layer"), label = NULL,
                     choices = choices, selected = choices[1])
      }
    })

    # --- Dorling checkbox (new label) ---
    output$dorling_ui <- renderUI({
      checkboxInput(ns("dorling"), tr("label_dorling", lang()), FALSE)
    })

    # --- Median slider (discrete 1-6) ---
    output$median_slider_ui <- renderUI({
      sliderInput(ns("median_size_pos"), tr("label_room_size", lang()),
                  min = 1, max = 6, value = 3, step = 1, ticks = TRUE)
    })

    output$median_label <- renderUI({
      pos <- input$median_size_pos
      if (is.null(pos)) pos <- 3
      label <- MEDIAN_SIZE_LABELS[pos]
      tags$div(style = "text-align:center; font-size:12px; color:#666; margin-top:-8px;",
               label)
    })

    # Map slider position to room size key
    median_size <- reactive({
      pos <- input$median_size_pos
      if (is.null(pos)) return("3.5zi")
      MEDIAN_SIZE_ORDER[pos]
    })

    # --- Slider controls (Rent/Share) ---
    output$slider_controls_ui <- renderUI({
      l <- lang()
      tagList(
        radioButtons(ns("slider_mode"), tr("label_analysis", l),
          choices = c(
            setNames("rent", tr("label_rent_to_share", l)),
            setNames("share", tr("label_share_to_rent", l))
          ),
          inline = TRUE
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'rent'", ns("slider_mode")),
          sliderInput(ns("rent"), tr("label_rent", l),
                      min = 500, max = 6500, value = 2000, step = 500)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'share'", ns("slider_mode")),
          sliderInput(ns("share"), tr("label_share", l),
                      min = 0.05, max = 0.95, value = 0.5, step = 0.05)
        )
      )
    })

    # --- Layer title (descriptive, below hr) ---
    output$layer_title <- renderUI({
      req(input$layer)
      cfg <- layer_registry[[input$layer]]
      if (is.null(cfg)) return(NULL)
      div(class = "layer-title",
        tr(cfg$title_key, lang())
      )
    })

    # --- Info box header ---
    output$info_box_header <- renderUI({
      tr("label_information", lang())
    })

    # --- Info box content (layer description) ---
    output$info_box_content <- renderUI({
      req(input$layer)
      cfg <- layer_registry[[input$layer]]
      if (is.null(cfg)) return(NULL)
      p(tr(cfg$info_key, lang()), style = "margin:0; font-size:12px;")
    })

    # --- Return values for other modules ---
    list(
      active_layer_id = reactive(input$layer),
      active_tab      = reactive(input$main_tabs),
      dorling_on      = reactive(input$dorling),
      pro_mode        = reactive(isTRUE(input$pro_mode)),
      slider_mode     = reactive(input$slider_mode),
      rent_val        = reactive(input$rent),
      share_val       = reactive(input$share),
      median_size     = median_size,
      info_panel_id   = ns("info_panel")
    )
  })
}
