# ============================================================
# app.R — Wohnungsknappheit in der Schweiz
# Modulare Shiny-App mit Tabs und Simple/Pro-Modus
# ============================================================

library(shinyjs)

# Alle Module und Daten laden (global.R lädt shiny, leaflet, sf, dplyr, classInt)
source("global.R")
source("R/mod_sidebar.R")
source("R/mod_map.R")

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("
    body { margin: 0; padding: 0; }
    .container-fluid { padding: 0; }
    .row { margin-left: 0; margin-right: 0; }
    .col-sm-3 { padding-left: 0; padding-right: 0; }
    .col-sm-9 { padding-left: 0; padding-right: 0; }
    h5 { margin-top: 8px; margin-bottom: 4px; }
    .table-condensed td { padding: 2px 6px; }

    /* Slide Toggle Switch */
    .toggle-slider {
      position: relative;
      display: inline-block;
      width: 34px; height: 18px;
      background: #ccc;
      border-radius: 18px;
      transition: background 0.3s;
      vertical-align: middle;
    }
    .toggle-slider::after {
      content: '';
      position: absolute;
      width: 14px; height: 14px;
      left: 2px; top: 2px;
      background: white;
      border-radius: 50%;
      transition: transform 0.3s;
    }
    .toggle-input:checked ~ .toggle-slider {
      background: #4a90d9;
    }
    .toggle-input:checked ~ .toggle-slider::after {
      transform: translateX(16px);
    }

    /* Full-height sidebar */
    .col-sm-3 .well {
      display: flex;
      flex-direction: column;
      height: 100vh;
      margin-bottom: 0;
      border-radius: 0;
      padding: 12px;
      overflow: hidden;
    }
    .sidebar-nav {
      flex: 1;
      overflow-y: auto;
      min-height: 0;
    }

    /* Section headers in layer list */
    .section-header {
      font-weight: bold;
      font-size: 12px;
      color: #333;
      margin-top: 10px;
      margin-bottom: 4px;
      padding-bottom: 2px;
      border-bottom: 1px solid #ddd;
    }
    .section-header:first-child { margin-top: 0; }

    /* Layer radio items */
    .layer-radio-item {
      display: flex;
      align-items: center;
      padding: 3px 0;
      cursor: pointer;
      font-size: 12px;
      font-weight: normal;
      color: #555;
    }
    .layer-radio-item:hover { color: #333; }
    .layer-radio-item input[type='radio'] { margin: 0 6px 0 4px; }
    .layer-radio-item.active { color: #333; }

    /* Controls frame below layer list */
    .controls-frame { padding: 8px 0 0 0; }

    /* Info Box (bottom of sidebar) */
    .sidebar-info-box {
      border: 1px solid #ccc;
      border-radius: 4px;
      background: #fafafa;
      margin-top: auto;
      flex-shrink: 0;
    }
    .info-box-header {
      cursor: pointer;
      padding: 8px 10px;
      display: flex;
      justify-content: space-between;
      align-items: center;
    }
    .info-box-header:hover { background: #f0f0f0; }
    .info-box-body {
      padding: 4px 10px 10px;
      font-size: 12px;
      color: #555;
      border-top: 1px solid #ddd;
    }

    /* Layer title styling */
    .layer-title {
      font-size: 15px;
      font-weight: bold;
      color: #333;
      margin-top: 4px;
      margin-bottom: 16px;
    }
  ")),
  # JS for info box toggle
  tags$script(HTML("
    function toggleInfoBox(el) {
      var content = el.nextElementSibling;
      var icon = el.querySelector('.info-toggle-icon');
      if (content.style.display === 'none') {
        content.style.display = 'block';
        icon.textContent = '\\u2212';
      } else {
        content.style.display = 'none';
        icon.textContent = '+';
      }
    }
  ")),
  # JS bridge: receive language from parent iframe via postMessage
  tags$script(HTML("
    window.addEventListener('message', function(e) {
      if (e.data && e.data.lang) {
        var lang = e.data.lang;
        if (['de','en','zh','th'].indexOf(lang) === -1) lang = 'de';
        Shiny.setInputValue('lang', lang);
      }
    });
  "))
  ),

  sidebarLayout(
    sidebarPanel(width = 3,
      # Navigation area (scrollable)
      div(class = "sidebar-nav",
        sidebar_nav_ui("sidebar")
      ),
      # Info box (pinned to bottom)
      sidebar_info_ui("sidebar")
    ),
    mainPanel(width = 9,
      map_ui("map")
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  # Language reactive
  lang <- reactive(input$lang %||% "de")

  # Sidebar-Modul starten
  sidebar <- sidebar_server("sidebar", layer_registry, lang)

  # Map-Modul starten — gibt click_info reactive zurück
  click_info <- map_server("map", sidebar, mun, mun_dorling,
                           layer_registry, relief_raster, ch_border, lang)

  # Info-Panel in der Sidebar rendern bei Klick
  output[["sidebar-info_panel"]] <- renderUI({
    info <- click_info()
    if (is.null(info)) {
      return(p(style = "color:#999; font-size:12px; font-style:italic;",
               tr("click_placeholder", lang())))
    }

    vals <- info$values
    vals[is.na(vals)] <- "\u2013"

    tagList(
      h5(info$municipality),
      tags$table(class = "table table-sm table-condensed",
        style = "font-size:12px;",
        mapply(function(label, val) {
          tags$tr(tags$td(tags$b(label)), tags$td(val))
        }, info$labels, vals, SIMPLIFY = FALSE)
      )
    )
  })
}

shinyApp(ui, server)
