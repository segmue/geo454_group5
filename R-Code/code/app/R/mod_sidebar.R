# ============================================================
# mod_sidebar.R
# Shiny-Modul: Sidebar mit Sektionsliste und Controls
# ============================================================

# Mapping discrete slider positions to room size keys
MEDIAN_SIZE_ORDER <- c("1.5zi", "2.5zi", "3.5zi", "4.5zi", "5.5zi", "6pluszi")
MEDIAN_SIZE_LABELS <- c("1.5-Zi.", "2.5-Zi.", "3.5-Zi.", "4.5-Zi.", "5.5-Zi.", "6+-Zi.")
MEDIAN_SIZE_LABELS_EN <- c("1.5 rm.", "2.5 rm.", "3.5 rm.", "4.5 rm.", "5.5 rm.", "6+ rm.")
MEDIAN_SIZE_LABELS_ZH <- c("1.5\u5ba4", "2.5\u5ba4", "3.5\u5ba4", "4.5\u5ba4", "5.5\u5ba4", "6+\u5ba4")
MEDIAN_SIZE_LABELS_TH <- c("1.5\u0e2b\u0e49\u0e2d\u0e07", "2.5\u0e2b\u0e49\u0e2d\u0e07", "3.5\u0e2b\u0e49\u0e2d\u0e07", "4.5\u0e2b\u0e49\u0e2d\u0e07", "5.5\u0e2b\u0e49\u0e2d\u0e07", "6+\u0e2b\u0e49\u0e2d\u0e07")

get_size_labels <- function(lang) {
  switch(lang,
    en = MEDIAN_SIZE_LABELS_EN,
    zh = MEDIAN_SIZE_LABELS_ZH,
    th = MEDIAN_SIZE_LABELS_TH,
    MEDIAN_SIZE_LABELS
  )
}

# --- Navigation UI (upper part of sidebar) ---
sidebar_nav_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Pro toggle (top right)
    div(style = "display:flex; justify-content:flex-end; margin-bottom:6px;",
      tags$label(class = "toggle-switch",
        style = "cursor:pointer;display:inline-flex;align-items:center;",
        tags$input(type = "checkbox", id = ns("pro_mode"),
                   class = "toggle-input",
                   style = "display:none;"),
        tags$span(class = "toggle-slider"),
        tags$span("Pro", style = "margin-left:6px;font-size:11px;color:#999;")
      )
    ),

    # Layer list with section headers
    uiOutput(ns("layer_list_ui")),

    # Layer-specific controls
    div(class = "controls-frame",
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
      tags$span(class = "info-toggle-icon", style = "font-size:14px; font-weight:bold;", "\u2212")
    ),
    div(class = "info-box-body", style = "display:block;",
      uiOutput(ns("info_box_content"))
    )
  )
}

sidebar_server <- function(id, layer_registry, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Layer list with section headers ---
    output$layer_list_ui <- renderUI({
      l <- lang()
      pro <- isTRUE(input$pro_mode)
      mode_filter <- if (pro) c("simple", "pro") else "simple"

      tabs <- c("availability", "affordability", "result")
      tab_labels <- c(tr("tab_availability", l), tr("tab_affordability", l), tr("tab_result", l))

      # Currently selected layer (preserve selection if still visible)
      current <- isolate(input$layer)

      # Build UI elements and collect all visible layer IDs
      ui_elements <- list()
      all_layer_ids <- character(0)

      for (i in seq_along(tabs)) {
        layers <- Filter(function(lr) lr$tab == tabs[i] && lr$mode %in% mode_filter, layer_registry)
        layers <- layers[order(sapply(layers, `[[`, "order"))]
        if (length(layers) == 0) next

        # Section header
        ui_elements[[length(ui_elements) + 1]] <- tags$div(
          class = "section-header", tab_labels[i]
        )

        # Radio items for this section
        for (lr in layers) {
          lid <- lr$id
          lbl <- tr(lr$label_key, l)
          all_layer_ids <- c(all_layer_ids, lid)

          ui_elements[[length(ui_elements) + 1]] <- tags$label(
            class = "layer-radio-item",
            tags$input(
              type = "radio",
              name = ns("layer"),
              value = lid,
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})", ns("layer"), lid)
            ),
            lbl
          )
        }
      }

      # Determine which layer should be selected
      selected <- if (!is.null(current) && current %in% all_layer_ids) current else all_layer_ids[1]

      # Add JS to set initial selection and check the right radio
      ui_elements[[length(ui_elements) + 1]] <- tags$script(HTML(sprintf(
        "Shiny.setInputValue('%s', '%s');
         document.querySelectorAll('input[name=\"%s\"]').forEach(function(r) {
           r.checked = (r.value === '%s');
         });",
        ns("layer"), selected, ns("layer"), selected
      )))

      tagList(ui_elements)
    })

    # --- Dorling checkbox ---
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
      label <- get_size_labels(lang())[pos]
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
