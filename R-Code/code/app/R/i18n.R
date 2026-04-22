# ============================================================
# i18n.R
# Translation dictionary and helper for multilingual UI
# ============================================================

TRANSLATIONS <- list(
  de = list(
    # App
    app_title        = "Wohnungsknappheit in der Schweiz",

    # Tabs
    tab_availability = "Verf\u00fcgbarkeit",
    tab_affordability = "Leistbarkeit",
    tab_result       = "Ergebnis",

    # Mode toggle
    toggle_pro       = "Pro",
    toggle_easy      = "Easy",

    # Layer labels
    layer_vacancy_rate   = "Leerstandsziffer 2025",
    layer_avg_rent       = "Durchschn. Marktmiete",
    layer_median_slider  = "Medianpreis pro Zimmerkategorie",
    layer_quote_gesamt   = "Inseratequote (Unsicherheit)",
    layer_share_avg_rent = "Leistbarkeit (% Haushalte)",
    layer_slider         = "Miete \u2194 Anteil Haushalte",
    layer_bivariate_v1   = "Leerstand \u00d7 Leistbarkeit",
    layer_bivariate_v2   = "Leerstand \u00d7 Leistbarkeit (4\u00d74)",

    # Layer info texts (collapsible info box)
    info_vacancy_rate   = "Die Leerstandsziffer zeigt den Anteil leerstehender Wohnungen am Gesamtbestand einer Gemeinde (Stand 2025). Tiefe Werte deuten auf einen angespannten Wohnungsmarkt hin.",
    info_avg_rent       = "Die gewichtete Durchschnittsmiete fasst die Medianpreise aller Zimmerkategorien pro Gemeinde zusammen, gewichtet nach dem jeweiligen Wohnungsbestand (GWR).",
    info_median_slider  = "Zeigt den Medianpreis der inserierten Mietwohnungen pro Zimmerkategorie und Gemeinde. Verschieben Sie den Regler, um zwischen den Kategorien (1.5- bis 6+-Zi.) zu wechseln.",
    info_quote_gesamt   = "Die Inseratequote gibt an, welcher Anteil des Wohnungsbestandes in die Analyse einfliesst. Tiefe Werte bedeuten weniger Datenbasis und h\u00f6here Unsicherheit.",
    info_share_avg_rent = "Zeigt, wie viel Prozent der Haushalte sich die durchschnittliche Marktmiete in ihrer Gemeinde leisten k\u00f6nnen. Basiert auf der gewichteten Durchschnittsmiete und Einkommensdaten.",
    info_slider         = "Interaktiver Modus: W\u00e4hlen Sie eine Miete (CHF) und sehen Sie, welcher Anteil der Haushalte sich diese leisten kann \u2013 oder umgekehrt.",
    info_bivariate_v1   = "Kombiniert Leerstand und Leistbarkeit in einer bivariaten Karte (3\u00d73). Gemeinden oben rechts (tiefer Leerstand + geringe Leistbarkeit) sind besonders unter Druck.",
    info_bivariate_v2   = "Erweiterte bivariate Darstellung (4\u00d74) f\u00fcr eine detailliertere Klassierung von Leerstand und Leistbarkeit.",

    # Layer titles (longer descriptive, shown below separator)
    title_vacancy_rate   = "Anteil leerstehender Wohnungen am Gesamtbestand (2025)",
    title_avg_rent       = "Gewichtete durchschnittliche Marktmiete pro Gemeinde",
    title_median_slider  = "Medianpreis der inserierten Mietwohnungen pro Zimmerkategorie",
    title_quote_gesamt   = "Anteil des erfassten Wohnungsbestandes (Unsicherheitsmass)",
    title_share_avg_rent = "Leistbarkeit (% Haushalte) der durchschnittlichen Marktmiete",
    title_slider         = "Interaktiver Vergleich: Miete und Anteil leistbarer Haushalte",
    title_bivariate_v1   = "Bivariate Darstellung: Leerstand \u00d7 Leistbarkeit (3\u00d73)",
    title_bivariate_v2   = "Erweiterte bivariate Darstellung (4\u00d74)",

    # Control labels
    label_map_view    = "Kartenansicht:",
    label_room_size   = "Zimmerkategorie:",
    label_analysis    = "Analyse:",
    label_rent        = "Miete (CHF/Monat)",
    label_share       = "Anteil Haushalte",
    label_dorling     = "Fl\u00e4che proportional zur Bev\u00f6lkerung anzeigen",
    label_information = "Information",
    label_rent_to_share = "Miete \u2192 Anteil",
    label_share_to_rent = "Anteil \u2192 Miete",

    # Placeholder
    click_placeholder = "Klicke auf eine Gemeinde um mehr zu erfahren",

    # Popup labels
    popup_municipality    = "Gemeinde",
    popup_share_pct       = "Anteil leistbar (%)",
    popup_avg_rent        = "Durchschn. Miete (CHF)",
    popup_vacancy_pct     = "Leerstandsziffer (%)",
    popup_gwr_total       = "GWR-Wohnungen",
    popup_inserate        = "Inserate",
    popup_inseratequote   = "Inseratequote",
    popup_class           = "Klasse",
    popup_vacancy_label   = "Leerstand",
    popup_afford_label    = "Leistbarkeit",
    popup_median_rent     = "Medianmiete",
    popup_n_inserate      = "Anzahl Inserate",
    popup_share_households = "Anteil Haushalte",
    popup_rent_chf        = "Miete (CHF)",
    popup_data_coverage   = "Datenabdeckung (%)",

    # Legend
    legend_no_data    = "Keine Daten",
    legend_median_prefix = "Medianmiete",
    legend_share_at   = "Anteil bei",
    legend_rent_at    = "Miete bei",

    # Bivariate labels
    biv_low           = "tief",
    biv_mid           = "mittel",
    biv_high          = "hoch",
    biv_mid_low       = "mittel-tief",
    biv_mid_high      = "mittel-hoch"
  ),

  en = list(
    app_title        = "Housing Shortage in Switzerland",

    tab_availability = "Availability",
    tab_affordability = "Affordability",
    tab_result       = "Result",

    toggle_pro       = "Pro",
    toggle_easy      = "Easy",

    layer_vacancy_rate   = "Vacancy Rate 2025",
    layer_avg_rent       = "Average Market Rent",
    layer_median_slider  = "Median Rent by Room Category",
    layer_quote_gesamt   = "Ad Coverage (Uncertainty)",
    layer_share_avg_rent = "Affordability (% of Households)",
    layer_slider         = "Rent \u2194 Household Share",
    layer_bivariate_v1   = "Vacancy \u00d7 Affordability",
    layer_bivariate_v2   = "Vacancy \u00d7 Affordability (4\u00d74)",

    info_vacancy_rate   = "The vacancy rate shows the share of unoccupied dwellings relative to total housing stock per municipality (2025). Low values indicate a tight housing market.",
    info_avg_rent       = "The weighted average rent aggregates median prices across all room categories per municipality, weighted by the housing stock from the Federal Register of Buildings and Dwellings (GWR).",
    info_median_slider  = "Shows the median asking rent per room category and municipality. Move the slider to switch between categories (1.5 to 6+ rooms).",
    info_quote_gesamt   = "The ad coverage rate indicates what share of the housing stock is captured in the analysis. Low values mean less data and higher uncertainty.",
    info_share_avg_rent = "Shows what percentage of households can afford the average market rent in their municipality. Based on the weighted average rent and income data.",
    info_slider         = "Interactive mode: Choose a rent (CHF) and see what share of households can afford it \u2013 or vice versa.",
    info_bivariate_v1   = "Combines vacancy and affordability in a bivariate map (3\u00d73). Municipalities in the top-right (low vacancy + low affordability) are under the most pressure.",
    info_bivariate_v2   = "Extended bivariate view (4\u00d74) for a more detailed classification of vacancy and affordability.",

    title_vacancy_rate   = "Share of vacant dwellings relative to total housing stock (2025)",
    title_avg_rent       = "Weighted average market rent per municipality",
    title_median_slider  = "Median asking rent per room category",
    title_quote_gesamt   = "Share of housing stock captured in the analysis (uncertainty measure)",
    title_share_avg_rent = "Affordability (% of households) of average market rent",
    title_slider         = "Interactive comparison: rent and share of affordable households",
    title_bivariate_v1   = "Bivariate map: Vacancy \u00d7 Affordability (3\u00d73)",
    title_bivariate_v2   = "Extended bivariate map (4\u00d74)",

    label_map_view    = "Map View:",
    label_room_size   = "Room Category:",
    label_analysis    = "Analysis:",
    label_rent        = "Rent (CHF/month)",
    label_share       = "Household Share",
    label_dorling     = "Show area proportional to population",
    label_information = "Information",
    label_rent_to_share = "Rent \u2192 Share",
    label_share_to_rent = "Share \u2192 Rent",

    click_placeholder = "Click on a municipality to learn more",

    popup_municipality    = "Municipality",
    popup_share_pct       = "Affordable Share (%)",
    popup_avg_rent        = "Avg. Rent (CHF)",
    popup_vacancy_pct     = "Vacancy Rate (%)",
    popup_gwr_total       = "GWR Dwellings",
    popup_inserate        = "Listings",
    popup_inseratequote   = "Ad Coverage",
    popup_class           = "Class",
    popup_vacancy_label   = "Vacancy",
    popup_afford_label    = "Affordability",
    popup_median_rent     = "Median Rent",
    popup_n_inserate      = "Number of Listings",
    popup_share_households = "Household Share",
    popup_rent_chf        = "Rent (CHF)",
    popup_data_coverage   = "Data Coverage (%)",

    legend_no_data    = "No data",
    legend_median_prefix = "Median Rent",
    legend_share_at   = "Share at",
    legend_rent_at    = "Rent at",

    biv_low           = "low",
    biv_mid           = "medium",
    biv_high          = "high",
    biv_mid_low       = "medium-low",
    biv_mid_high      = "medium-high"
  ),

  # Mandarin Chinese - stubs, fallback to DE
  zh = list(
    app_title         = "\u745e\u58eb\u4f4f\u623f\u77ed\u7f3a",
    tab_availability  = "\u53ef\u7528\u6027",
    tab_affordability = "\u8d1f\u62c5\u80fd\u529b",
    tab_result        = "\u7ed3\u679c"
  ),

  # Thai - stubs, fallback to DE
  th = list(
    app_title         = "\u0e01\u0e32\u0e23\u0e02\u0e32\u0e14\u0e41\u0e04\u0e25\u0e19\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22\u0e43\u0e19\u0e2a\u0e27\u0e34\u0e15\u0e40\u0e0b\u0e2d\u0e23\u0e4c\u0e41\u0e25\u0e19\u0e14\u0e4c",
    tab_availability  = "\u0e04\u0e27\u0e32\u0e21\u0e1e\u0e23\u0e49\u0e2d\u0e21\u0e43\u0e0a\u0e49\u0e07\u0e32\u0e19",
    tab_affordability = "\u0e04\u0e27\u0e32\u0e21\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e43\u0e19\u0e01\u0e32\u0e23\u0e08\u0e48\u0e32\u0e22",
    tab_result        = "\u0e1c\u0e25\u0e25\u0e31\u0e1e\u0e18\u0e4c"
  )
)

#' Translate a key to the given language
#' Falls back to German, then to the raw key
#' @param key Character string - the translation key
#' @param lang Character string - language code ("de", "en", "zh", "th")
#' @return Character string - translated text
tr <- function(key, lang = "de") {
  val <- TRANSLATIONS[[lang]][[key]]
  if (!is.null(val)) return(val)
  # Fallback to German
  val_de <- TRANSLATIONS[["de"]][[key]]
  if (!is.null(val_de)) return(val_de)
  # Last resort: return key itself
  key
}
