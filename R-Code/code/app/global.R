# ============================================================
# global.R
# Daten laden, Farben vorberechnen, Dorling
# Wird einmal beim App-Start ausgeführt.
# ============================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(classInt)
library(raster)
library(cartogram)
library(cols4all)

# --- Hilfsfunktionen laden ---
source("R/i18n.R")
source("R/color_utils.R")
source("R/layer_config.R")
source("R/legend_utils.R")

# ============================================================
# 1. DATEN LADEN
# ============================================================
mun_raw <- readRDS("data/municipalities_analysis.rds")
# Relief-Raster laden (bereits reprojiziert via preprocess_relief.R)
relief_raster <- raster("data/02-relief-georef-clipped-resampled.tif")

# ============================================================
# 2. DATEN VORBEREITEN
# ============================================================
mun <- mun_raw

# ============================================================
# 3. ABGELEITETE SPALTEN
# ============================================================
mun$Share_avg_rent_weighted_pct <- round(mun$Share_avg_rent_weighted * 100, 1)
mun$Share_avg_rent_weighted_inverted <- 1 - mun$Share_avg_rent_weighted

# valid_weights auch in Prozent (falls noch 0-1)
# (ist bereits 0-100 laut str-Output)

# ============================================================
# 4. ALLE FARBEN VORBERECHNEN
# ============================================================

# --- Share_avg_rent_weighted_pct (Rot) ---
mun <- add_color_col(mun, "Share_avg_rent_weighted_pct", 6, COLORS_RED, "equal")

# --- Leerwohnungsziffer.2025 (fixe Breaks, Rot+Blau) ---
labeled_leer <- cut(mun$Leerwohnungsziffer.2025,
                    breaks = BREAKS_LEER,
                    include.lowest = TRUE,
                    labels = COLORS_LEER_ALL)
mun$Leerwohnungsziffer.2025_color <- as.character(labeled_leer)

# --- avg_rent_weighted (Blau) ---
mun <- add_color_col(mun, "avg_rent_weighted", 6, COLORS_BLUE, "equal")

# --- quote_gesamt (Violett) ---
mun <- add_color_col(mun, "quote_gesamt", 6, COLORS_PURPLE, "equal")

# --- median_*zi (Orange/Braun, 6 Grössen) ---
median_cols <- c("median_1.5zi", "median_2.5zi", "median_3.5zi",
                 "median_4.5zi", "median_5.5zi", "median_6pluszi")
# Spaltenname-Fix: "median_6zi_plus" → "median_6pluszi" (falls nötig)
if ("median_6zi_plus" %in% names(mun) && !"median_6pluszi" %in% names(mun)) {
  mun$median_6pluszi <- mun$median_6zi_plus
}
if ("n_total_6zi_plus" %in% names(mun) && !"n_total_6pluszi" %in% names(mun)) {
  mun$n_total_6pluszi <- mun$n_total_6zi_plus
}

for (col in median_cols) {
  if (col %in% names(mun)) {
    mun <- add_color_col(mun, col, 6, COLORS_MEDIAN, "equal")
  }
}

# --- Bivariate V1: 3×3 Blau-Violett-Rot ---
mun <- add_bivariate_color_col(mun,
  "Leerwohnungsziffer.2025", BIVARIATE_BREAKS_V1,
  "Share_avg_rent_weighted_inverted", BIVARIATE_N_V1, "quantile",
  BIVARIATE_COLORS_V1, suffix = "_v1")

# --- Bivariate V2: 4×4 ---
# Farbmatrix: X-Achse invertieren damit 1=tief(alarm)=dunkel, 4=hoch(ideal)=hell
mat_v2 <- cols4all::c4a("bivario.verdant_orchard", n = 4)
bivariate_colors_v2 <- setNames(
  as.vector(mat_v2[4:1, ]),
  paste0(rep(1:4, times = 4), "-", rep(1:4, each = 4))
)
mun <- add_bivariate_color_col(mun,
  "Leerwohnungsziffer.2025", BIVARIATE_BREAKS_V2,
  "Share_avg_rent_weighted_inverted", BIVARIATE_N_V2, "quantile",
  bivariate_colors_v2, suffix = "_v2")

# ============================================================
# 5. DORLING CARTOGRAM VORBERECHNEN
# ============================================================
message("  Dorling cartogram...")
t_dorling <- proc.time()
mun_proj <- st_transform(mun, 2056)
mun_proj <- mun_proj[!is.na(mun_proj$sum_gwr) & mun_proj$sum_gwr > 0, ]

mun_dorling <- cartogram_dorling(mun_proj,
                                  weight  = "sum_gwr",
                                  k       = 0.8,
                                  itermax = 30)
mun_dorling <- st_transform(mun_dorling, 4326)
message("  Dorling fertig (", round((proc.time() - t_dorling)[3], 1), "s)")

# ============================================================
# 6. LAYER-REGISTRY AUFBAUEN
# ============================================================
layer_registry <- get_layer_registry(mun)

# Bivariate V2 Farben einfügen
layer_registry$bivariate_v2$color_matrix <- bivariate_colors_v2

# ============================================================
# 7. LEGENDEN VORBERECHNEN
# ============================================================
for (lid in names(layer_registry)) {
  cfg <- layer_registry[[lid]]

  if (cfg$legend_type == "standard" && lid != "slider") {
    vals <- mun[[cfg$value_col]]
    valid <- vals[!is.na(vals)]
    brks <- classIntervals(valid, n = cfg$n_breaks, style = cfg$style)$brks
    pal  <- colorRampPalette(cfg$colors)(cfg$n_breaks)
    layer_registry[[lid]]$legend_html <- make_standard_legend(
      brks, pal, tr(cfg$label_key, "de"), suffix = cfg$suffix)

  } else if (cfg$legend_type == "fixed") {
    layer_registry[[lid]]$legend_html <- make_fixed_legend(
      cfg$breaks, cfg$colors, tr(cfg$label_key, "de"), suffix = cfg$suffix)

  } else if (cfg$legend_type == "bivariate") {
    layer_registry[[lid]]$legend_html <- make_bivariate_legend(
      cfg$color_matrix, cfg$n_biv, cfg$x_label, cfg$y_label)

  } else if (cfg$legend_type == "bivariate_pro") {
    # Breaks Y aus der vorberechneten Klassierung (als attr gespeichert)
    breaks_y <- attr(mun[[paste0("bivariate_color", "_v2")]], "breaks_y")
    layer_registry[[lid]]$legend_html <- make_bivariate_legend_pro(
      cfg$color_matrix, cfg$n_biv, cfg$x_label, cfg$y_label,
      cfg$breaks_x, breaks_y)
  }
}

message("Global.R: Daten geladen, ", nrow(mun), " Gemeinden, ",
        length(layer_registry), " Layer konfiguriert.")
