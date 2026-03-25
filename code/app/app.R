library(shiny)
library(tmap)
library(sf)
library(raster)
library(dplyr)

tmap_mode("view")

municipalities <- st_read("../../data/wohnkompass_gemeinden.gpkg", quiet = TRUE) |>
  rename(municipality = gemeinde_name, rent = miete_vorgabe, share = anteil_haushalte) |>
  st_transform(4326)
relief <- raster("../../data/02-relief-georef-clipped-resampled.tif")

# Precompute one geometry per municipality
geom <- municipalities[!duplicated(municipalities$bfs_nr), ] |> dplyr::select(bfs_nr)

# Safe interpolation: returns NA if fewer than 2 non-NA values
safe_approx <- function(x, y, xout) {
  valid <- !is.na(x) & !is.na(y)
  if (sum(valid) < 2) return(NA_real_)
  approx(x[valid], y[valid], xout = xout, rule = 1)$y
}

ui <- fluidPage(
  titlePanel("Housing Affordability in Switzerland"),
  sidebarLayout(
    sidebarPanel(width = 3,
      p("Share of households that can afford a given rent, based on a 30% income affordability threshold."),
      radioButtons("mode", "Analysis",
        choices = c("Rent → Share" = "rent", "Share → Rent" = "share"),
        inline = TRUE
      ),
      conditionalPanel("input.mode == 'rent'",
        sliderInput("rent", "Rent (CHF/month)", min = 500, max = 6500, value = 2000, step = 500)
      ),
      conditionalPanel("input.mode == 'share'",
        sliderInput("share", "Share of households", min = 0.05, max = 0.95, value = 0.5, step = 0.05)
      )
    ),
    mainPanel(width = 9,
      tmapOutput("map", height = "85vh")
    )
  )
)

server <- function(input, output, session) {
  map_data <- reactive({
    if (input$mode == "rent") {
      dat <- municipalities[municipalities$rent == input$rent, ]
      dat$fill_val <- dat$share
      list(data = dat, legend = "Share of households", n = 7, is_rent = TRUE)
    } else {
      interp <- municipalities |>
        st_drop_geometry() |>
        arrange(bfs_nr, share) |>
        group_by(bfs_nr, municipality) |>
        summarise(
          interp_rent = safe_approx(share, rent, input$share),
          .groups = "drop"
        ) |>
        filter(!is.na(interp_rent))
      dat <- merge(geom, interp, by = "bfs_nr")
      dat$fill_val <- dat$interp_rent
      list(data = dat, legend = "Rent (CHF)", n = 7, is_rent = FALSE)
    }
  })

  output$map <- renderTmap({
    md <- map_data()
    dat <- md$data

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
        col = "grey40",
        lwd = 0.2,
        popup.vars = if (md$is_rent) c("Municipality" = "municipality", "Rent" = "rent", "Share" = "share")
                     else c("Municipality" = "municipality", "Interpolated rent" = "interp_rent")
      ) +
    tm_basemap("CartoDB.Positron")
  })
}

shinyApp(ui, server)
