# ============================================================
# legend_utils.R
# HTML-Legenden für Leaflet-Karten
# ============================================================

#' NA-Eintrag als HTML (für alle Legenden)
make_na_entry <- function(lang = "de") {
  paste0(
    '<div style="display:flex;align-items:center;margin-top:6px;">',
    '<span style="background:', NA_COLOR, ';width:18px;height:14px;display:inline-block;',
    'margin-right:6px;border:1px solid #ccc;"></span>',
    '<span style="font-size:10px;color:#888;">', tr("legend_no_data", lang), '</span></div>'
  )
}

#' Standard-Legende mit Farbboxen und Labels
make_standard_legend <- function(breaks, colors, title, suffix = "", reverse = FALSE, lang = "de", digits = 0) {
  n <- length(colors)
  labels <- character(n)
  for (i in seq_len(n)) {
    labels[i] <- paste0(
      format(round(breaks[i], digits), big.mark = "'"),
      " – ",
      format(round(breaks[i + 1], digits), big.mark = "'"),
      suffix
    )
  }

  if (reverse) {
    colors <- rev(colors)
    labels <- rev(labels)
  }

  items <- paste0(
    '<div style="display:flex;align-items:center;margin:2px 0;">',
    '<span style="background:', colors,
    ';width:18px;height:14px;display:inline-block;margin-right:6px;',
    'border:1px solid #ccc;"></span>',
    '<span style="font-size:11px;">', labels, '</span></div>'
  )

  HTML(paste0(
    '<div style="background:rgba(255,255,255,0.9);padding:8px 10px;',
    'border-radius:4px;border:1px solid #ccc;max-width:220px;">',
    '<div style="font-weight:bold;font-size:12px;margin-bottom:4px;">', title, '</div>',
    paste(items, collapse = ""),
    make_na_entry(lang),
    '</div>'
  ))
}

#' Legende mit fixen Breaks (z.B. Leerstandsziffer)
make_fixed_legend <- function(breaks, colors, title, suffix = "%", lang = "de") {
  n <- length(colors)
  labels <- character(n)
  for (i in seq_len(n)) {
    hi <- if (is.infinite(breaks[i + 1])) paste0(">", breaks[i]) else paste0(breaks[i], "–", breaks[i + 1])
    labels[i] <- paste0(hi, suffix)
  }

  items <- paste0(
    '<div style="display:flex;align-items:center;margin:2px 0;">',
    '<span style="background:', colors,
    ';width:18px;height:14px;display:inline-block;margin-right:6px;',
    'border:1px solid #ccc;"></span>',
    '<span style="font-size:11px;">', labels, '</span></div>'
  )

  HTML(paste0(
    '<div style="background:rgba(255,255,255,0.9);padding:8px 10px;',
    'border-radius:4px;border:1px solid #ccc;max-width:200px;">',
    '<div style="font-weight:bold;font-size:12px;margin-bottom:4px;">', title, '</div>',
    paste(items, collapse = ""),
    make_na_entry(lang),
    '</div>'
  ))
}

#' Bivariate Legenden-Matrix (n×n Grid) — Simple Mode
#' Layout (gespiegelt): x=1 links, x=n rechts; y=1 oben, y=n unten
#'   → Dunkelviolett unten-links, Grau oben-rechts
make_bivariate_legend <- function(color_matrix, n, x_label, y_label, lang = "de") {
  sz <- 24
  cells <- ""
  for (y_class in 1:n) {
    css_row <- y_class
    for (display_col in 1:n) {
      x_class <- display_col
      key <- paste0(x_class, "-", y_class)
      clr <- color_matrix[key]
      if (is.na(clr)) clr <- "#ccc"
      cells <- paste0(cells,
        '<div style="grid-column:', display_col, ';grid-row:', css_row,
        ';background:', clr, ';width:', sz, 'px;height:', sz, 'px;"></div>')
    }
  }

  grid_w <- n * (sz + 1)
  HTML(paste0(
    '<div style="background:rgba(255,255,255,0.9);padding:10px 10px 10px 30px;',
    'border-radius:4px;border:1px solid #ccc;">',
    '<div style="font-weight:bold;font-size:12px;margin-bottom:6px;">',
    tr("legend_vacancy", lang), ' \u00d7 ', tr("legend_unaffordability", lang), '</div>',
    '<div style="position:relative;display:inline-block;">',
    # y-axis label (left) — Leistbarkeit → (pointing down = increasing unaffordability)
    '<div style="position:absolute;left:-25px;top:50%;transform:rotate(-90deg) translateX(-50%);',
    'font-size:9px;white-space:nowrap;transform-origin:0 0;">',
    tr("legend_affordability", lang), ' \u2192</div>',
    # grid
    '<div style="display:grid;grid-template-columns:repeat(', n, ',', sz, 'px);',
    'grid-template-rows:repeat(', n, ',', sz, 'px);gap:1px;">',
    cells, '</div>',
    # x-axis label (bottom)
    '<div style="font-size:9px;margin-top:3px;">',
    tr("legend_vacancy", lang), ' \u2192</div>',
    '</div>',
    # NA entry
    make_na_entry(lang),
    '</div>'
  ))
}

#' Bivariate Legenden-Matrix — Pro Mode mit Break-Labels
make_bivariate_legend_pro <- function(color_matrix, n, x_label, y_label,
                                       breaks_x, breaks_y, lang = "de") {
  sz <- 32

  rows_html <- ""
  for (y_class in n:1) {
    lo_y <- round(breaks_y[y_class], 2)
    hi_y <- round(breaks_y[y_class + 1], 2)
    y_lbl <- paste0(lo_y, "\u2013", hi_y)

    row_cells <- paste0(
      '<td style="font-size:10px;color:#666;text-align:right;padding-right:4px;',
      'white-space:nowrap;">', y_lbl, '</td>')

    for (display_col in 1:n) {
      x_class <- n - display_col + 1
      key <- paste0(x_class, "-", y_class)
      clr <- color_matrix[key]
      if (is.na(clr)) clr <- "#ccc"
      row_cells <- paste0(row_cells,
        '<td style="background:', clr, ';width:', sz, 'px;height:', sz, 'px;',
        'border:1px solid rgba(255,255,255,0.3);"></td>')
    }
    rows_html <- paste0(rows_html, '<tr>', row_cells, '</tr>')
  }

  # X-Tick-Zeile
  x_tick_cells <- '<td></td>'
  for (i in 1:n) {
    x_cls <- n - i + 1
    lo_x <- breaks_x[x_cls]
    hi_x <- breaks_x[x_cls + 1]
    x_lbl <- if (is.infinite(hi_x)) paste0(">", lo_x, "%") else paste0(lo_x, "\u2013", hi_x, "%")
    x_tick_cells <- paste0(x_tick_cells,
      '<td style="font-size:10px;color:#666;text-align:center;padding-top:2px;">',
      x_lbl, '</td>')
  }
  rows_html <- paste0(rows_html, '<tr>', x_tick_cells, '</tr>')

  HTML(paste0(
    '<div style="background:rgba(255,255,255,0.9);padding:10px 12px;',
    'border-radius:4px;border:1px solid #ccc;">',
    '<div style="font-weight:bold;font-size:12px;margin-bottom:6px;">',
    tr("legend_vacancy", lang), ' \u00d7 ', tr("legend_affordability", lang), '</div>',
    '<table style="border-collapse:collapse;border-spacing:0;">',
    rows_html, '</table>',
    # NA entry
    make_na_entry(lang),
    '</div>'
  ))
}
