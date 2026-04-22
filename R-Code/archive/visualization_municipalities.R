# ============================================================
# visualization_municipalities.R
# Choropleth-Visualisierung des municipalities_analysis Datensatzes
# ============================================================

library(sf)
library(mapview)
library(classInt)
library(dplyr)

mun <- readRDS("data/new_data/municipalities_analysis.rds")


# ============================================================
# HELPER FUNCTION
# Klassiert eine numerische Spalte und schreibt Hex-Farben
# in eine neue Spalte <col>_color.
#
# Args:
#   data     : sf-Objekt
#   col      : Spaltenname (character)
#   n_breaks : Anzahl Klassen (integer)
#   colors   : Farbvektor mit n_breaks Einträgen (character)
#   style    : classInt-Stil, z.B. "equal", "quantile", "jenks"
# ============================================================

add_color_col <- function(data, col, n_breaks, colors, style = "equal") {
  vals  <- data[[col]]
  valid <- vals[!is.na(vals)]
  brks  <- classIntervals(valid, n = n_breaks, style = style)$brks
  pal   <- colorRampPalette(colors)(n_breaks)
  labeled <- cut(vals, breaks = brks, include.lowest = TRUE, labels = pal)
  data[[paste0(col, "_color")]] <- as.character(labeled)
  data
}

# Divergierende Klassierung mit festem Mittelpunkt (center).
# colors_low: von extremem Tiefstwert bis Mitte (z.B. dunkelrot → weisslich)
# colors_high: von Mitte bis extremem Höchstwert (z.B. weisslich → dunkelblau)
add_diverging_color_col <- function(data, col, center,
                                    n_low, n_high,
                                    colors_low, colors_high) {
  vals    <- data[[col]]
  min_val <- min(vals, na.rm = TRUE)
  max_val <- max(vals, na.rm = TRUE)

  brks_low  <- seq(min_val, center, length.out = n_low  + 1)
  brks_high <- seq(center,  max_val, length.out = n_high + 1)
  all_brks  <- c(brks_low, brks_high[-1])   # doppelten center-Wert entfernen

  pal_low  <- colorRampPalette(colors_low)(n_low)
  pal_high <- colorRampPalette(colors_high)(n_high)
  all_pal  <- c(pal_low, pal_high)

  labeled <- cut(vals, breaks = all_brks, include.lowest = TRUE, labels = all_pal)
  data[[paste0(col, "_color")]] <- as.character(labeled)
  data
}

# Bivariater Choropleth: col_x mit fixen Breaks, col_y mit classInt.
# Schreibt 'bivariate_class' ("x-y") und 'bivariate_color' (Hex) ins sf-Objekt.
add_bivariate_color_col <- function(data,
                                    col_x, breaks_x,
                                    col_y, n_y, style_y,
                                    color_matrix, suffix = "") {
  x <- data[[col_x]]
  y <- data[[col_y]]

  cls_x <- as.integer(cut(x, breaks = breaks_x, include.lowest = TRUE))

  valid_y <- y[!is.na(y)]
  brks_y  <- classIntervals(valid_y, n = n_y, style = style_y)$brks
  cls_y   <- as.integer(cut(y, breaks = brks_y, include.lowest = TRUE))

  key <- paste0(cls_x, "-", cls_y)
  data[[paste0("bivariate_class", suffix)]] <- key
  data[[paste0("bivariate_color", suffix)]] <- color_matrix[key]
  data
}


# ============================================================
# VISUALIZATION
# ============================================================
# Für jede Spalte / Gruppe:
#   1. n_breaks, Farbvektor und Klassierungsstil festlegen
#   2. add_color_col() aufrufen → neue <col>_color Spalte
#
# Änderungen nur hier oben nötig – die Karten unten passen
# sich automatisch an.
# ============================================================


# ------------------------------------------------------------
# avg_rent_weighted  [CHF, ca. 968–5652]
# ------------------------------------------------------------
n_breaks_avg_rent    <- 6
colors_avg_rent      <- c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b", "#041e42")
classify_avg_rent    <- "equal"   # Alternativen: "quantile", "jenks", "pretty"

mun <- add_color_col(mun, "avg_rent_weighted",
                     n_breaks_avg_rent, colors_avg_rent, classify_avg_rent)


# ------------------------------------------------------------
# Leerwohnungsziffer.2025
# Fixe Bins: 6 Rottöne (angespannt, < 1.5 %) + 3 Blautöne (entspannt, > 1.5 %)
#   Rot:  0–0.2, 0.2–0.4, 0.4–0.6, 0.6–0.9, 0.9–1.2, 1.2–1.5  (dunkelrot → hellrot)
#   Blau: 1.5–2, 2–3, >3                                          (hellblau → dunkelblau)
# ------------------------------------------------------------
breaks_leer_fixed <- c(0, 0.2, 0.4, 0.6, 0.9, 1.2, 1.5, 2, 3, Inf)
colors_leer_red   <- c("#4b0f19", "#7e1f2a", "#a83a46", "#cc7480", "#f2c9cc", "#fff5f2")
colors_leer_blue  <- c("#f0f8ff", "#6baed6", "#08306b")
colors_leer_all   <- c(colors_leer_red, colors_leer_blue)

labeled_leer <- cut(mun$Leerwohnungsziffer.2025,
                    breaks         = breaks_leer_fixed,
                    include.lowest = TRUE,
                    labels         = colors_leer_all)
mun$Leerwohnungsziffer.2025_color <- as.character(labeled_leer)


# ------------------------------------------------------------
# Share_avg_rent_weighted  [0–1]
# ------------------------------------------------------------
n_breaks_share_avg   <- 6
colors_share_avg <- rev(c("#fff5f6", "#f6d6db", "#e7a9b3", "#c86b81", "#8f2f3c", "#4b0f19"))
# No, create color scale red with 5 shades from light (0) to dark (1)

classify_share_avg   <- "equal"

mun <- add_color_col(mun, "Share_avg_rent_weighted",
                     n_breaks_share_avg, colors_share_avg, classify_share_avg)


# ------------------------------------------------------------
# quote_gesamt  [ca. 0.002–0.123]
# ------------------------------------------------------------
n_breaks_quote       <- 6
colors_quote         <- c("#fcfbfd", "#dadaeb", "#9e9ac8", "#6a51a3", "#54278f", "#3f007d")
classify_quote       <- "equal"

mun <- add_color_col(mun, "quote_gesamt",
                     n_breaks_quote, colors_quote, classify_quote)


# ------------------------------------------------------------
# valid_weights  [0–1]
# ------------------------------------------------------------
n_breaks_valid       <- 6
colors_valid         <- c("#fff7bc", "#fee391", "#fec44f", "#fe9929", "#d95f0e", "#993404")
classify_valid       <- "equal"

mun <- add_color_col(mun, "valid_weights",
                     n_breaks_valid, colors_valid, classify_valid)


# ------------------------------------------------------------
# sum_gwr  [Ganzzahl]
# ------------------------------------------------------------
n_breaks_sum_gwr     <- 6
colors_sum_gwr       <- c("#f7f7f7", "#d9d9d9", "#969696", "#636363", "#252525", "#000000")
classify_sum_gwr     <- "equal"

mun <- add_color_col(mun, "sum_gwr",
                     n_breaks_sum_gwr, colors_sum_gwr, classify_sum_gwr)


# ------------------------------------------------------------
# share_500 … share_6500  (Gruppe, 13 Spalten)  [0–1]
# ------------------------------------------------------------
# n_breaks_share_group <- 6
# colors_share_group   <- c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b", "#041e42")
# classify_share_group <- "equal"

# share_cols <- paste0("share_", c(500, 1000, 1500, 2000, 2500, 3000,
#                                   3500, 4000, 4500, 5000, 5500, 6000, 6500))
# for (col in share_cols) {
#   mun <- add_color_col(mun, col,
#                        n_breaks_share_group, colors_share_group, classify_share_group)
# }


# ------------------------------------------------------------
# median_*zi  (Gruppe, 6 Spalten)  [CHF]
# ------------------------------------------------------------
n_breaks_median      <- 6
colors_median        <- c("#fff5eb", "#fdd0a2", "#fd8d3c", "#d94801", "#8c2d04", "#4a1500")
classify_median      <- "equal"

median_cols <- c("median_1.5zi", "median_2.5zi", "median_3.5zi",
                 "median_4.5zi", "median_5.5zi", "median_6pluszi")
for (col in median_cols) {
  mun <- add_color_col(mun, col,
                       n_breaks_median, colors_median, classify_median)
}


# ------------------------------------------------------------
# BIVARIATE: Leerwohnungsziffer.2025 × Share_avg_rent_weighted
# Zwei Versionen:
#   V1 — 3×3, Blau-Violett-Rot  (Breaks: 0 / 0.5 / 2 / Inf)
#   V2 — 4×4, Blau-Orange       (Breaks: 0 / 0.5 / 1 / 1.5 / Inf)
# ------------------------------------------------------------
mun$Share_avg_rent_weighted_inverted <- 1 - mun$Share_avg_rent_weighted

# --- V1: 3×3 Blau-Violett-Rot ---
# Hue   → Leer-Achse  (blau+stark = tight, blass = loose)
# Sätt. → Share-Achse (hell = erschwinglich, dunkel = unerschwinglich)
# Alarm-Ecke "1-3" (tight + unerschwinglich) = dunkelviolett #3F2949
bivariate_breaks_v1 <- c(0, 0.5, 2, Inf)
bivariate_n_v1      <- 3
bivariate_colors_v1 <- c(
  "1-1" = "#4885C1",  "1-2" = "#435786",  "1-3" = "#3F2949",
  "2-1" = "#89A1C8",  "2-2" = "#806A8A",  "2-3" = "#77324C",
  "3-1" = "#CABED0",  "3-2" = "#BC7C8F",  "3-3" = "#AE3A4E"
)
mun <- add_bivariate_color_col(mun,
  "Leerwohnungsziffer.2025", bivariate_breaks_v1,
  "Share_avg_rent_weighted_inverted", bivariate_n_v1, "quantile",
  bivariate_colors_v1, suffix = "_v1")

# --- V2: 4×4, drei Farbpaletten-Varianten (eine auskommentieren) ---
# cols4all-Paletten via c4a(): Matrix [Zeile=Leer-Klasse, Spalte=Share-Klasse]
# → in named vector "row-col" = "#hex" umwandeln für add_bivariate_color_col()
library(cols4all)

c4a_to_biv <- function(pal_name, n = 4, transpose = FALSE) {
  mat <- c4a(pal_name, n = n)  # n×n Farbmatrix
  if (transpose) mat <- t(mat) # falls cols4all rows/cols anders orientiert
  # as.vector() läuft spaltenweise: mat[1,1], mat[2,1], ..., mat[1,2], ...
  # → Key "r-c" = mat[r, c] (erste Zahl = cls_x = Leer, zweite = cls_y = Share)
  setNames(
    as.vector(mat),
    paste0(rep(1:n, times = n), "-", rep(1:n, each = n))
  )
}



bivariate_breaks_v2 <- c(0, 0.5, 1, 1.5, Inf)
bivariate_n_v2      <- 4

# Variante A: bivario.verdant_orchard  (Grün-Orange-Töne)
bivariate_colors_v2 <- c4a_to_biv("bivario.verdant_orchard", n = 4, transpose = F)

# Variante B: bivario.electric_neon  (kräftige Neon-Töne)
# bivariate_colors_v2 <- c4a_to_biv("bivario.electric_neon")

# Variante C: manuell Blau-Orange (bilinear interpoliert)
# bivariate_colors_v2 <- c(
#   "1-1" = "#0571B0",  "1-2" = "#336699",  "1-3" = "#665C82",  "1-4" = "#7F3B08",
#   "2-1" = "#4D8DBE",  "2-2" = "#6D7E96",  "2-3" = "#8D6F6E",  "2-4" = "#9B5631",
#   "3-1" = "#96BACF",  "3-2" = "#ABBAB1",  "3-3" = "#C0BB93",  "3-4" = "#CA9B5D",
#   "4-1" = "#D1E5F0",  "4-2" = "#DEE1CC",  "4-3" = "#EBDDA8",  "4-4" = "#FEE0B6"
# )

mun <- add_bivariate_color_col(mun,
  "Leerwohnungsziffer.2025", bivariate_breaks_v2,
  "Share_avg_rent_weighted", bivariate_n_v2, "quantile",
  bivariate_colors_v2, suffix = "_v2")


# ============================================================
# MAPVIEW VISUALISIERUNGEN
# ============================================================
# Für jede Hauptspalte eine interaktive Choroplethenkarte.
# Die Breaks und Farben werden aus den Parametern oben
# abgeleitet – einfach oben ändern, Karte neu ausführen.
# ============================================================

make_mapview <- function(data, col, n_breaks, colors, style) {
  valid_vals <- data[[col]][!is.na(data[[col]])]
  brks <- classIntervals(valid_vals, n = n_breaks, style = style)$brks
  mapview(data,
          zcol        = col,
          col.regions = colorRampPalette(colors)(100),
          at          = brks,
          layer.name  = col,
          map.types   = "CartoDB.Positron")
}


# --- avg_rent_weighted ---
make_mapview(mun, "avg_rent_weighted",
             n_breaks_avg_rent, colors_avg_rent, classify_avg_rent)

# --- Leerwohnungsziffer.2025 (fixe Bins) ---
at_leer <- c(0, 0.2, 0.4, 0.6, 0.9, 1.2, 1.5, 2, 3,
             max(mun$Leerwohnungsziffer.2025, na.rm = TRUE))
mapview(mun,
        zcol        = "Leerwohnungsziffer.2025",
        col.regions = colors_leer_all,
        at          = at_leer,
        layer.name  = "Leerwohnungsziffer.2025",
        map.types   = "CartoDB.Positron")

# --- Share_avg_rent_weighted ---
make_mapview(mun, "Share_avg_rent_weighted",
             n_breaks_share_avg, colors_share_avg, classify_share_avg)

# --- quote_gesamt ---
make_mapview(mun, "quote_gesamt",
             n_breaks_quote, colors_quote, classify_quote)

# --- valid_weights ---
make_mapview(mun, "valid_weights",
             n_breaks_valid, colors_valid, classify_valid)

# --- Bivariate V1: 3×3 Blau-Violett-Rot ---
mun$bivariate_class_v1_f <- factor(mun$bivariate_class_v1, levels = names(bivariate_colors_v1))
mapview(mun |> select(municipality, Leerwohnungsziffer.2025, Share_avg_rent_weighted_inverted, Share_avg_rent_weighted, bivariate_class_v1_f),
        zcol        = "bivariate_class_v1_f",
        col.regions = unname(bivariate_colors_v1),
        layer.name  = "V1 3\u00d73: Leer \u00d7 Share (Blau-Rot)",
        na.color    = "grey94",
        map.types   = "CartoDB.Positron")

# --- Bivariate V2: 4×4 Blau-Orange ---
mun$bivariate_class_v2_f <- factor(mun$bivariate_class_v2, levels = names(bivariate_colors_v2))
mapview(mun |> select(municipality, Leerwohnungsziffer.2025, Share_avg_rent_weighted_inverted, Share_avg_rent_weighted, bivariate_class_v2_f),
        zcol        = "bivariate_class_v2_f",
        col.regions = unname(bivariate_colors_v2),
        layer.name  = "V2 4\u00d74: Leer \u00d7 Share (Blau-Orange)",
        na.color    = "grey94",
        map.types   = "CartoDB.Positron")


# ============================================================
# DORLING CARTOGRAM (Prototyp)
# Kreisgrösse = sum_gwr (Anzahl GWR-Haushalte pro Gemeinde)
# ============================================================
library(cartogram)

# Projektion auf LV95 (cartogram_dorling braucht projiziertes KBS)
mun_proj <- st_transform(mun, 2056)

# Dorling berechnen  (k steuert Kompression/Überlappungsvermeidung)
k_dorling       <- 0.8   # anpassen bei zu starker Überlappung/Spreizung
itermax_dorling <- 100

mun_proj <- mun_proj %>%
  filter(!is.na(sum_gwr) & sum_gwr > 0)  # Dorling nur mit gültigen Größenwerten

mun_dorling <- cartogram_dorling(mun_proj,
                                 weight   = "sum_gwr",
                                 k        = k_dorling,
                                 itermax  = itermax_dorling)

# Zurück nach WGS84 für mapview
mun_dorling <- st_transform(mun_dorling, 4326)

# Visualisierung — Farbe = bivariate V1 Klassierung, Grösse = GWR-Haushalte
mun_dorling$bivariate_class_v1_f <- factor(mun_dorling$bivariate_class_v1,
                                           levels = names(bivariate_colors_v1))

mapview(mun_dorling,
        zcol        = "bivariate_class_v1_f",
        col.regions = unname(bivariate_colors_v1),
        na.color    = "grey94",
        layer.name  = "Dorling: Grösse = GWR-Haushalte",
        map.types   = "CartoDB.Positron")
