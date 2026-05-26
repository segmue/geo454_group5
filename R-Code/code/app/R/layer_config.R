# ============================================================
# layer_config.R
# Zentrale Layer-Registry: Farbschemas, Breaks, Popup-Felder
# ============================================================



# NA-Farbe (grey85)
NA_COLOR <- "#D9D9D9"

# Farbpaletten
COLORS_BLUE   <- c("#c6dbef", "#6baed6", "#2171b5", "#08306b")
COLORS_RED    <- rev(c("#fff5f6", "#f6d6db", "#e7a9b3", "#c86b81", "#8f2f3c", "#4b0f19"))
COLORS_PURPLE <- c("#c4c4df", "#9e9ac8", "#6a51a3", "#4a1382", "#1a0040")
COLORS_MEDIAN <- c("#fff5eb", "#fdd0a2", "#fd8d3c", "#d94801", "#8c2d04", "#4a1500")

# Leerstandsziffer: Rot (angespannt) + Blau (entspannt)
BREAKS_LEER   <- c(0, 0.2, 0.5, 1, 1.5, 2, 3, Inf)
COLORS_LEER_RED  <- colorRampPalette(c("#AE3A4E", "#BC7C8F", "#CABED0", "#f5f0f0"))(4)
COLORS_LEER_RED <- colorRampPalette(c("#8a2c3c", "#F6D6DB"), space = "Lab")(4)
COLORS_LEER_BLUE <- colorRampPalette(c("#e8f0f8", "#89A1C8", "#4885C1"))(3)
COLORS_LEER_ALL  <- c(COLORS_LEER_RED, COLORS_LEER_BLUE)

# Bivariate V1: 3×3 Blau-Violett-Rot
BIVARIATE_BREAKS_V1 <- c(0, 0.5, 2, Inf)
BIVARIATE_N_V1      <- 3
BIVARIATE_COLORS_V1 <- c(
  "1-1" = "#4885C1", "1-2" = "#435786", "1-3" = "#3F2949",
  "2-1" = "#89A1C8", "2-2" = "#806A8A", "2-3" = "#77324C",
  "3-1" = "#CABED0", "3-2" = "#BC7C8F", "3-3" = "#AE3A4E"
)

# Bivariate V2: 4×4 (wird in global.R mit c4a_to_biv generiert)
BIVARIATE_BREAKS_V2 <- c(0, 0.5, 1, 1.5, Inf)
BIVARIATE_N_V2      <- 4

# Slider-Konfiguration (alte App)
SLIDER_BLUES <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")

#' Erstellt die Layer-Registry
#' @param mun sf-Objekt mit allen vorberechneten Spalten
get_layer_registry <- function(mun) {
  list(
    # ---- TAB: AVAILABILITY ----
    vacancy_rate = list(
      id         = "vacancy_rate",
      tab        = "availability",
      order      = 1,
      label_key  = "layer_vacancy_rate",
      title_key  = "title_vacancy_rate",
      info_key   = "info_vacancy_rate",
      mode       = "simple",
      color_col  = "Leerwohnungsziffer.2025_color",
      value_col  = "Leerwohnungsziffer.2025",
      legend_type = "fixed",
      breaks     = BREAKS_LEER,
      colors     = COLORS_LEER_ALL,
      suffix     = "%",
      popup_cols   = c("municipality", "Leerwohnungsziffer.2025",
                        "leerstehende_wohnungen"),
      popup_label_keys = c("popup_municipality", "popup_vacancy_pct",
                            "popup_vacant_count"),
      has_dorling = FALSE
    ),

    avg_rent = list(
      id         = "avg_rent",
      tab        = "availability",
      order      = 2,
      label_key  = "layer_avg_rent",
      title_key  = "title_avg_rent",
      info_key   = "info_avg_rent",
      mode       = "simple",
      color_col  = "avg_rent_weighted_color",
      value_col  = "avg_rent_weighted",
      legend_type = "standard",
      n_breaks   = 4,
      colors     = COLORS_BLUE,
      style      = "equal",
      suffix     = " CHF",
      popup_cols   = c("municipality", "avg_rent_weighted", "sum_angebote"),
      popup_label_keys = c("popup_municipality", "popup_avg_rent",
                            "popup_inserate"),
      has_dorling = FALSE
    ),

    median_slider = list(
      id         = "median_slider",
      tab        = "availability",
      order      = 3,
      label_key  = "layer_median_slider",
      title_key  = "title_median_slider",
      info_key   = "info_median_slider",
      mode       = "pro",
      color_col  = "median_3.5zi_color",
      value_col  = "median_3.5zi",
      legend_type = "median_dynamic",
      n_breaks   = 6,
      colors     = COLORS_MEDIAN,
      style      = "equal",
      suffix     = " CHF",
      popup_cols   = c("municipality"),
      popup_label_keys = c("popup_municipality"),
      has_dorling = FALSE
    ),

    quote_gesamt = list(
      id         = "quote_gesamt",
      tab        = "availability",
      order      = 4,
      label_key  = "layer_quote_gesamt",
      title_key  = "title_quote_gesamt",
      info_key   = "info_quote_gesamt",
      mode       = "pro",
      color_col  = "quote_gesamt_pct_color",
      value_col  = "quote_gesamt_pct",
      legend_type = "standard",
      n_breaks   = 5,
      colors     = COLORS_PURPLE,
      style      = "equal",
      suffix     = "%",
      popup_cols   = c("municipality", "quote_gesamt_pct", "sum_angebote", "sum_gwr"),
      popup_label_keys = c("popup_municipality", "popup_inseratequote",
                            "popup_inserate", "popup_gwr_total"),
      has_dorling = FALSE
    ),

    # ---- TAB: AFFORDABILITY ----
    share_avg_rent = list(
      id         = "share_avg_rent",
      tab        = "affordability",
      order      = 1,
      label_key  = "layer_share_avg_rent",
      title_key  = "title_share_avg_rent",
      info_key   = "info_share_avg_rent",
      mode       = "simple",
      color_col  = "Share_avg_rent_weighted_pct_color",
      value_col  = "Share_avg_rent_weighted_pct",
      legend_type = "standard",
      n_breaks   = 6,
      colors     = COLORS_RED,
      style      = "equal",
      suffix     = "%",
      popup_cols   = c("municipality", "Share_avg_rent_weighted_pct",
                        "avg_rent_weighted", "Leerwohnungsziffer.2025"),
      popup_label_keys = c("popup_municipality", "popup_share_pct",
                            "popup_avg_rent", "popup_vacancy_pct"),
      has_dorling = FALSE
    ),

    slider = list(
      id         = "slider",
      tab        = "affordability",
      order      = 2,
      label_key  = "layer_slider",
      title_key  = "title_slider",
      info_key   = "info_slider",
      mode       = "pro",
      color_col  = "slider_color",
      value_col  = "slider_val",
      legend_type = "slider",
      n_breaks   = 7,
      colors     = SLIDER_BLUES,
      style      = "quantile",
      suffix     = "",
      popup_cols   = c("municipality"),
      popup_label_keys = c("popup_municipality"),
      has_dorling = FALSE
    ),

    # ---- TAB: RESULT ----
    bivariate_v1 = list(
      id         = "bivariate_v1",
      tab        = "result",
      order      = 1,
      label_key  = "layer_bivariate_v1",
      title_key  = "title_bivariate_v1",
      info_key   = "info_bivariate_v1",
      mode       = "simple",
      color_col  = "bivariate_color_v1",
      value_col  = "bivariate_class_v1",
      legend_type = "bivariate",
      color_matrix = BIVARIATE_COLORS_V1,
      n_biv      = 3,
      x_label    = "Leerstand",
      y_label    = "Unleistbarkeit",
      popup_cols   = c("municipality", "bivariate_class_v1",
                        "Leerwohnungsziffer.2025", "Share_avg_rent_weighted_pct"),
      popup_label_keys = c("popup_municipality", "popup_class",
                            "popup_vacancy_pct", "popup_share_pct"),
      has_dorling = TRUE
    ),

    bivariate_v2 = list(
      id         = "bivariate_v2",
      tab        = "result",
      order      = 2,
      label_key  = "layer_bivariate_v2",
      title_key  = "title_bivariate_v2",
      info_key   = "info_bivariate_v2",
      mode       = "pro",
      color_col  = "bivariate_color_v2",
      value_col  = "bivariate_class_v2",
      legend_type = "bivariate_pro",
      color_matrix = NULL,
      n_biv      = 4,
      x_label    = "Leerstand",
      y_label    = "Leistbarkeit",
      breaks_x   = BIVARIATE_BREAKS_V2,
      popup_cols   = c("municipality", "bivariate_class_v2",
                        "Leerwohnungsziffer.2025", "Share_avg_rent_weighted_pct"),
      popup_label_keys = c("popup_municipality", "popup_class",
                            "popup_vacancy_pct", "popup_share_pct"),
      has_dorling = FALSE
    )
  )
}

# Zimmergrössen-Konfiguration für Median-Slider
MEDIAN_SIZES <- list(
  "1.5zi"   = list(label = "1.5-Zi.", col = "median_1.5zi", n_col = "n_total_1.5zi"),
  "2.5zi"   = list(label = "2.5-Zi.", col = "median_2.5zi", n_col = "n_total_2.5zi"),
  "3.5zi"   = list(label = "3.5-Zi.", col = "median_3.5zi", n_col = "n_total_3.5zi"),
  "4.5zi"   = list(label = "4.5-Zi.", col = "median_4.5zi", n_col = "n_total_4.5zi"),
  "5.5zi"   = list(label = "5.5-Zi.", col = "median_5.5zi", n_col = "n_total_5.5zi"),
  "6pluszi"  = list(label = "6+-Zi.",  col = "median_6pluszi", n_col = "n_total_6zi_plus")
)
