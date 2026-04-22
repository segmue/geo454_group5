# ============================================================
# color_utils.R
# Klassierungs-Hilfsfunktionen für Choropleth-Karten
# ============================================================

#' Standard-Klassierung: teilt numerische Werte in n Klassen
#' und schreibt Hex-Farben in <col>_color
add_color_col <- function(data, col, n_breaks, colors, style = "equal") {
  vals  <- data[[col]]
  valid <- vals[!is.na(vals)]
  brks  <- classIntervals(valid, n = n_breaks, style = style)$brks
  pal   <- colorRampPalette(colors)(n_breaks)
  labeled <- cut(vals, breaks = brks, include.lowest = TRUE, labels = pal)
  data[[paste0(col, "_color")]] <- as.character(labeled)
  attr(data[[paste0(col, "_color")]], "breaks") <- brks
  attr(data[[paste0(col, "_color")]], "palette") <- pal
  data
}

#' Divergierende Klassierung mit festem Mittelpunkt
add_diverging_color_col <- function(data, col, center,
                                    n_low, n_high,
                                    colors_low, colors_high) {
  vals    <- data[[col]]
  min_val <- min(vals, na.rm = TRUE)
  max_val <- max(vals, na.rm = TRUE)

  brks_low  <- seq(min_val, center, length.out = n_low  + 1)
  brks_high <- seq(center,  max_val, length.out = n_high + 1)
  all_brks  <- c(brks_low, brks_high[-1])

  pal_low  <- colorRampPalette(colors_low)(n_low)
  pal_high <- colorRampPalette(colors_high)(n_high)
  all_pal  <- c(pal_low, pal_high)

  labeled <- cut(vals, breaks = all_brks, include.lowest = TRUE, labels = all_pal)
  data[[paste0(col, "_color")]] <- as.character(labeled)
  attr(data[[paste0(col, "_color")]], "breaks") <- all_brks
  attr(data[[paste0(col, "_color")]], "palette") <- all_pal
  data
}

#' Bivariater Choropleth: col_x mit fixen Breaks, col_y mit classInt
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
  data[[paste0("bivariate_color", suffix)]] <- unname(color_matrix[key])

  attr(data[[paste0("bivariate_color", suffix)]], "breaks_x") <- breaks_x
  attr(data[[paste0("bivariate_color", suffix)]], "breaks_y") <- brks_y
  data
}

#' cols4all-Palette in Named Vector für bivariate Klassierung
c4a_to_biv <- function(pal_name, n = 4, transpose = FALSE) {
  mat <- cols4all::c4a(pal_name, n = n)
  if (transpose) mat <- t(mat)
  setNames(
    as.vector(mat),
    paste0(rep(1:n, times = n), "-", rep(1:n, each = n))
  )
}
