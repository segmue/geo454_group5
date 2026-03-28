library(shiny)
library(tmap)
library(sf)
library(raster)
library(dplyr)
library(DT)

tmap_mode("view")

# 数据读取
municipalities <- st_read("wohnkompass_gemeinden.gpkg", quiet = TRUE) |>
  rename(
    municipality = gemeinde_name,
    rent = miete_vorgabe,
    share = anteil_haushalte
  ) |>
  st_transform(4326)

relief <- raster("02-relief-georef-clipped-resampled.tif")
geom <- municipalities[!duplicated(municipalities$bfs_nr), ] |> dplyr::select(bfs_nr)

safe_approx <- function(x, y, xout) {
  valid <- !is.na(x) & !is.na(y)
  if (sum(valid) < 2) return(NA_real_)
  approx(x[valid], y[valid], xout = xout, rule = 1)$y
}

i18n <- list(
  en = list(
    title = "Housing Affordability in Switzerland",
    desc = "Share of households that can afford a given rent, based on a 30% income affordability threshold.",
    analysis = "Analysis",
    mode_rent = "Rent → Share",
    mode_share = "Share → Rent",
    rent_lbl = "Rent (CHF/month)",
    share_lbl = "Share of households",
    btn_show = "Show Data",
    btn_hide = "Hide Data",
    tab_title = "Source data",
    col_muni = "Municipality",
    col_rent = "Rent (CHF)",
    col_share = "Share",
    col_interp = "Interpolated rent"
  ),
  zh = list(
    title = "瑞士住房可负担性",
    desc = "基于收入30%的可负担性阈值，可负担对应租金的家庭占比。",
    analysis = "分析",
    mode_rent = "租金 → 占比",
    mode_share = "占比 → 租金",
    rent_lbl = "租金（CHF/月）",
    share_lbl = "家庭占比",
    btn_show = "显示数据",
    btn_hide = "隐藏数据",
    tab_title = "地图源数据",
    col_muni = "市镇",
    col_rent = "租金（CHF）",
    col_share = "占比",
    col_interp = "插值租金"
  ),
  de = list(
    title = "Wohnungserschwinglichkeit in der Schweiz",
    desc = "Anteil der Haushalte, die sich eine bestimmte Miete leisten können (30 % Einkommensgrenze).",
    analysis = "Analyse",
    mode_rent = "Miete → Anteil",
    mode_share = "Anteil → Miete",
    rent_lbl = "Miete (CHF/Monat)",
    share_lbl = "Haushaltsanteil",
    btn_show = "Daten anzeigen",
    btn_hide = "Daten ausblenden",
    tab_title = "Quelldaten",
    col_muni = "Gemeinde",
    col_rent = "Miete (CHF)",
    col_share = "Anteil",
    col_interp = "Interpolierte Miete"
  ),
  th = list(
    title = "ความสามารถในการจ่ายค่าที่อยู่อาศัยในสวิตเซอร์แลนด์",
    desc = "สัดส่วนครัวเรือนที่จ่ายค่าเช่าได้ ภายใต้เกณฑ์รายได้ 30%",
    analysis = "วิเคราะห์",
    mode_rent = "ค่าเช่า → สัดส่วน",
    mode_share = "สัดส่วน → ค่าเช่า",
    rent_lbl = "ค่าเช่า (CHF/เดือน)",
    share_lbl = "สัดส่วนครัวเรือน",
    btn_show = "แสดงข้อมูล",
    btn_hide = "ซ่อนข้อมูล",
    tab_title = "ข้อมูลต้นทาง",
    col_muni = "เทศบาล",
    col_rent = "ค่าเช่า (CHF)",
    col_share = "สัดส่วน",
    col_interp = "ค่าเช่าประมาณค่า"
  )
)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .map-container { position: relative; }
    .toggle-btn { position: absolute; top: 10px; right: 10px; z-index: 1000; }
    #map { height: 88vh !important; }
  "))),
  titlePanel(NULL),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "lang", "Language / 语言 / Sprache / ภาษา",
        choices = c("English" = "en", "简体中文" = "zh", "Deutsch" = "de", "ไทย" = "th"),
        selected = "en"
      ),
      h4(textOutput("txt_title")),
      p(textOutput("txt_desc")),
      h5(textOutput("txt_analysis")),
      radioButtons("mode", NULL, choices = c("rent" = "rent", "share" = "share"), inline = TRUE),
      conditionalPanel(
        "input.mode == 'rent'",
        sliderInput("rent", textOutput("txt_rent_lbl"), min = 500, max = 6500, value = 2000, step = 500)
      ),
      conditionalPanel(
        "input.mode == 'share'",
        sliderInput("share", textOutput("txt_share_lbl"), min = 0.05, max = 0.95, value = 0.5, step = 0.05)
      )
    ),
    mainPanel(
      width = 9,
      uiOutput("map_table_layout")
    )
  )
)

server <- function(input, output, session) {
  L <- reactive(i18n[[input$lang]])
  
  output$txt_title    <- renderText(L()$title)
  output$txt_desc     <- renderText(L()$desc)
  output$txt_analysis <- renderText(L()$analysis)
  observe({
    updateRadioButtons(
      session, "mode",
      choices = setNames(c("rent", "share"), c(L()$mode_rent, L()$mode_share))
    )
    updateSliderInput(session, "rent", label = L()$rent_lbl)
    updateSliderInput(session, "share", label = L()$share_lbl)
  })
  
  show_table <- reactiveVal(FALSE)
  observeEvent(input$toggle_table, { show_table(!show_table()) })
  
  map_data <- reactive({
    if (input$mode == "rent") {
      dat <- municipalities[municipalities$rent == input$rent, ]
      dat$fill_val <- dat$share
      list(data = dat, legend = L()$share_lbl, n = 7, is_rent = TRUE)
    } else {
      interp <- municipalities |>
        st_drop_geometry() |>
        arrange(bfs_nr, share) |>
        group_by(bfs_nr, municipality) |>
        summarise(interp_rent = safe_approx(share, rent, input$share), .groups = "drop") |>
        filter(!is.na(interp_rent))
      dat <- merge(geom, interp, by = "bfs_nr")
      dat$fill_val <- dat$interp_rent
      list(data = dat, legend = L()$rent_lbl, n = 7, is_rent = FALSE)
    }
  })
  
  output$map <- renderTmap({
    md <- map_data()
    dat <- md$data
    popup_vars <- if (md$is_rent) {
      setNames(
        c("municipality", "rent", "share"),
        c(L()$col_muni, L()$col_rent, L()$col_share)
      )
    } else {
      setNames(
        c("municipality", "interp_rent"),
        c(L()$col_muni, L()$col_interp)
      )
    }
    blues <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")
    fill_scale <- tm_scale_intervals(n = md$n, values = blues, style = "quantile")
    
    tm_shape(relief) +
      tm_raster(
        col.legend = tm_legend_hide(),
        col.scale = tm_scale_intervals(n = 100, values = grey.colors(100, start = 0.7, end = 1))
      ) +
      tm_shape(dat) +
      tm_polygons(
        fill = "fill_val",
        fill.scale = fill_scale,
        fill.legend = tm_legend(title = md$legend),
        col = "grey40", lwd = 0.2,
        popup.vars = popup_vars
      ) +
      tm_basemap("CartoDB.Positron")
  })
  
  output$data_table <- DT::renderDT({
    md <- map_data()
    df <- st_drop_geometry(md$data)
    if (md$is_rent) {
      out <- df |> select(municipality, rent, share)
      names(out) <- c(L()$col_muni, L()$col_rent, L()$col_share)
    } else {
      out <- df |> select(municipality, interp_rent)
      names(out) <- c(L()$col_muni, L()$col_interp)
    }
    datatable(out, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$map_table_layout <- renderUI({
    action_label <- if (show_table()) L()$btn_hide else L()$btn_show
    fluidRow(
      div(
        class = "map-container",
        actionButton("toggle_table", action_label, class = "btn btn-primary toggle-btn"),
        column(
          width = if (show_table()) 7 else 12,
          tmapOutput("map", height = "88vh")
        ),
        if (show_table()) {
          column(
            width = 5,
            h4(L()$tab_title),
            DTOutput("data_table")
          )
        }
      )
    )
  })
}

shinyApp(ui, server)