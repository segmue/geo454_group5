# ============================================================
# i18n.R
# Translation dictionary and helper for multilingual UI
# ============================================================

TRANSLATIONS <- list(
  de = list(
    # App
    app_title        = "Wohnungsknappheit in der Schweiz (2025)",

    # Tabs
    tab_availability = "Verf\u00fcgbarkeit",
    tab_affordability = "Leistbarkeit",
    tab_result       = "Ergebnis",

    # Mode toggle
    toggle_pro       = "Pro",
    toggle_easy      = "Easy",

    # Layer labels (Kurznamen – keine Abkürzungen, keine Jahreszahlen)
    layer_vacancy_rate   = "Leerstandsziffer",
    layer_avg_rent       = "Durchschnittliche Angebotsmiete",
    layer_median_slider  = "Medianpreis pro Zimmerkategorie",
    layer_quote_gesamt   = "Inseratequote (Unsicherheit)",
    layer_share_avg_rent = "Leistbarkeit (% Haushalte)",
    layer_slider         = "Mietpreis-Analyse",
    layer_bivariate_v1   = "Leerstand \u00d7 Leistbarkeit",
    layer_bivariate_v2   = "Leerstand \u00d7 Leistbarkeit (4\u00d74)",

    # Layer info texts (collapsible info box)
    info_vacancy_rate   = "Die Leerstandsziffer zeigt den Anteil leerstehender Wohnungen am Gesamtbestand einer Gemeinde. Gem\u00e4ss dem BWO gilt eine Leerstandsziffer unter 1 % als Wohnungsnot und zwischen 1 und 1,5 % als Wohnungsmangel (vgl. SVIT, 2023; NZZ, 2023). Die divergierende Farbskala bildet diese Schwellenwerte ab. Datenquelle: BFS STAT-TAB, 2025.",
    info_avg_rent       = "Diese Karte zeigt die durchschnittliche Angebotsmiete pro Gemeinde. Die Berechnung erfolgt auf Basis der Medianpreise aller Zimmerkategorien (Layer \u00abMedianpreis pro Zimmerkategorie\u00bb), gewichtet nach dem Wohnungsbestand gem\u00e4ss dem Geb\u00e4ude- und Wohnungsregister (GWR). Datenquellen: Lookmove / Watson, 2025.",
    info_median_slider  = "Zeigt den Medianpreis der inserierten Mietwohnungen pro Zimmerkategorie und Gemeinde. Die Ausgangsdaten auf Postleitzahlebene wurden proportional auf Gemeindeebene gewichtet und modelliert. Diese Daten bilden die Grundlage f\u00fcr die Berechnung der durchschnittlichen Angebotsmiete. Datenquelle: Lookmove, 2025.",
    info_quote_gesamt   = "F\u00fcr die Berechnung der Angebotsmieten wurden Inserate von Lookmove verwendet. Diese stellen nur eine Stichprobe des tats\u00e4chlichen Wohnungsmarktes dar und unterliegen daher Ungenauigkeiten. Diese Karte visualisiert die Datenabdeckung: Sie zeigt, wie viel Prozent des Gesamtwohnungsbestandes einer Gemeinde als Inserat erfasst sind. Tiefe Werte bedeuten eine geringere Datenbasis und h\u00f6here Unsicherheit. Datenquellen: Lookmove / GWR, 2025.",
    info_share_avg_rent = "Dieser Layer verbindet die durchschnittliche Angebotsmiete (Lookmove, 2025) mit Daten zur Leistbarkeit von Mietpreisen (Nachfragemonitor, 2023). Er zeigt, wie viel Prozent der Haushalte sich eine durchschnittliche Wohnung auf dem Markt ihrer Gemeinde leisten k\u00f6nnten. Ein Mietpreis gilt als leistbar, wenn er nicht mehr als 30 % des verf\u00fcgbaren Nettoeinkommens betr\u00e4gt. Datenquellen: Lookmove / Nachfragemonitor Mietwohnungen, 2023/2025.",
    info_slider         = "Interaktive Analyse: Einen Mietpreis (CHF) eingeben und sehen, welcher Anteil der Haushalte sich diesen leisten kann \u2013 oder umgekehrt einen Haushaltsanteil w\u00e4hlen und die entsprechende Miete ermitteln. Angezeigte Werte sind Bruttomieten (inkl. Nebenkosten). Datenquelle: Nachfragemonitor, 2023.",
    info_bivariate_v1   = "Diese Karte kombiniert die Ergebnisse der vorangehenden Layer: die Leerstandsziffer (Verf\u00fcgbarkeit) und die Leistbarkeit der durchschnittlichen Angebotsmiete. Die Leerstandsziffer wird anhand fester Schwellenwerte in drei Klassen eingeteilt (0\u20130,5 %, 0,5\u20132 %, \u00fcber 2 %). Die Leistbarkeit wird mittels Quantilen klassiert, sodass jede der drei Klassen gleich viele Gemeinden enth\u00e4lt. Die Kombination ergibt eine 3\u00d73-Matrix: Dunkle Farbt\u00f6ne kennzeichnen Gemeinden mit tiefem Leerstand und geringer Leistbarkeit \u2013 dort ist der Wohnungsmarkt besonders angespannt. Mit der Option \u00abFl\u00e4che proportional zur Bev\u00f6lkerung\u00bb werden die Gemeinden als Dorling-Kartogramm dargestellt, um die demografische Relevanz sichtbar zu machen.",
    info_bivariate_v2   = "Erweiterte bivariate Darstellung mit einer 4\u00d74-Klassierung f\u00fcr eine differenziertere Betrachtung von Leerstand und Leistbarkeit. Die Leerstandsziffer wird in vier Klassen eingeteilt (0\u20130,5 %, 0,5\u20131 %, 1\u20131,5 %, \u00fcber 1,5 %), die Leistbarkeit ebenfalls mittels Quantilen in vier Klassen. Die feinere Abstufung erm\u00f6glicht es, detailierter zwischen Gemeinden mit Wohnungsnot, bzw. mangel und entspannten Leerstandsziffern zu unterscheiden.",

    # Layer titles (longer descriptive, shown below separator)
    title_vacancy_rate   = "Anteil leerstehender Wohnungen am Gesamtbestand",
    title_avg_rent       = "Gewichtete durchschnittliche Angebotsmiete pro Gemeinde",
    title_median_slider  = "Medianpreis der inserierten Mietwohnungen pro Zimmerkategorie",
    title_quote_gesamt   = "Anteil des erfassten Wohnungsbestandes (Unsicherheitsmass)",
    title_share_avg_rent = "Anteil der Haushalte, die sich die durchschnittliche Angebotsmiete leisten k\u00f6nnen",
    title_slider         = "Wieviele Haushalte k\u00f6nnen sich wieviel Miete leisten?",
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
    label_rent_to_share = "Von Miete zu Haushalten",
    label_share_to_rent = "Von Anteil Haushalten zu Miete",

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
    biv_mid_high      = "mittel-hoch",

    # Legend axis labels
    legend_vacancy       = "Leerstand",
    legend_affordability = "Leistbarkeit",
    legend_unaffordability = "Unleistbarkeit"
  ),

  en = list(
    # App
    app_title        = "Housing Shortage in Switzerland (2025)",

    # Tabs
    tab_availability = "Availability",
    tab_affordability = "Affordability",
    tab_result       = "Result",

    # Mode toggle
    toggle_pro       = "Pro",
    toggle_easy      = "Easy",

    # Layer labels
    layer_vacancy_rate   = "Vacancy Rate",
    layer_avg_rent       = "Average Asking Rent",
    layer_median_slider  = "Median Rent by Room Category",
    layer_quote_gesamt   = "Ad Coverage (Uncertainty)",
    layer_share_avg_rent = "Affordability (% of Households)",
    layer_slider         = "Rent Analysis",
    layer_bivariate_v1   = "Vacancy \u00d7 Affordability",
    layer_bivariate_v2   = "Vacancy \u00d7 Affordability (4\u00d74)",

    # Layer info texts
    info_vacancy_rate   = "The vacancy rate shows the share of unoccupied dwellings relative to total housing stock per municipality. According to the Federal Office for Housing (BWO), a vacancy rate below 1% indicates a housing crisis, while rates between 1% and 1.5% indicate a housing shortage (cf. SVIT, 2023; NZZ, 2023). The diverging colour scale reflects these thresholds. Data source: FSO STAT-TAB, 2025.",
    info_avg_rent       = "This map shows the average asking rent per municipality. It is calculated from the median prices of all room categories (layer \u00abMedian Rent by Room Category\u00bb), weighted by the housing stock according to the Federal Register of Buildings and Dwellings (GWR). Data sources: Lookmove / Watson, 2025.",
    info_median_slider  = "Shows the median asking rent per room category and municipality. The underlying data at postal code level was proportionally weighted and modelled at municipality level. These data form the basis for calculating the average asking rent. Data source: Lookmove, 2025.",
    info_quote_gesamt   = "Listings from Lookmove were used to calculate asking rents. These represent only a sample of the actual housing market and are therefore subject to inaccuracies. This map visualises data coverage: it shows what percentage of a municipality\u2019s total housing stock is captured by listings. Low values indicate a smaller data basis and higher uncertainty. Data sources: Lookmove / GWR, 2025.",
    info_share_avg_rent = "This layer combines the average asking rent (Lookmove, 2025) with data on rent affordability (Demand Monitor, 2023). It shows what percentage of households could afford an average dwelling on the market in their municipality. Rent is considered affordable if it does not exceed 30% of available net income. Data sources: Lookmove / Demand Monitor for Rental Housing, 2023/2025.",
    info_slider         = "Interactive analysis: enter a rent amount (CHF) and see what share of households can afford it \u2013 or select a household share and find the corresponding rent. Displayed values are gross rents (including utilities). Data source: Demand Monitor, 2023.",
    info_bivariate_v1   = "This map combines the results of the preceding layers: the vacancy rate (availability) and the affordability of the average asking rent. The vacancy rate is classified into three classes using fixed thresholds (0\u20130.5%, 0.5\u20132%, above 2%). Affordability is classified using quantiles, so that each of the three classes contains an equal number of municipalities. The combination produces a 3\u00d73 matrix: dark tones indicate municipalities with low vacancy and low affordability \u2013 where the housing market is most strained. The option \u00abShow area proportional to population\u00bb displays municipalities as a Dorling cartogram to highlight demographic relevance.",
    info_bivariate_v2   = "Extended bivariate display with a 4\u00d74 classification for a more detailed view of vacancy and affordability. The vacancy rate is divided into four classes (0\u20130.5%, 0.5\u20131%, 1\u20131.5%, above 1.5%), and affordability is also classified into four quantile-based classes. The finer gradation makes it possible to distinguish more precisely between municipalities facing a housing crisis, a housing shortage, and those with relaxed vacancy rates.",

    # Layer titles
    title_vacancy_rate   = "Share of vacant dwellings relative to total housing stock",
    title_avg_rent       = "Weighted average asking rent per municipality",
    title_median_slider  = "Median asking rent per room category",
    title_quote_gesamt   = "Share of housing stock captured in the analysis (uncertainty measure)",
    title_share_avg_rent = "Share of households that can afford the average asking rent",
    title_slider         = "How many households can afford how much rent?",
    title_bivariate_v1   = "Bivariate map: Vacancy \u00d7 Affordability (3\u00d73)",
    title_bivariate_v2   = "Extended bivariate map (4\u00d74)",

    # Control labels
    label_map_view    = "Map View:",
    label_room_size   = "Room Category:",
    label_analysis    = "Analysis:",
    label_rent        = "Rent (CHF/month)",
    label_share       = "Household Share",
    label_dorling     = "Show area proportional to population",
    label_information = "Information",
    label_rent_to_share = "From Rent to Households",
    label_share_to_rent = "From Households to Rent",

    # Placeholder
    click_placeholder = "Click on a municipality to learn more",

    # Popup labels
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

    # Legend
    legend_no_data    = "No data",
    legend_median_prefix = "Median Rent",
    legend_share_at   = "Share at",
    legend_rent_at    = "Rent at",

    # Bivariate labels
    biv_low           = "low",
    biv_mid           = "medium",
    biv_high          = "high",
    biv_mid_low       = "medium-low",
    biv_mid_high      = "medium-high",

    # Legend axis labels
    legend_vacancy       = "Vacancy",
    legend_affordability = "Affordability",
    legend_unaffordability = "Unaffordability"
  ),

  zh = list(
    # App
    app_title        = "\u745e\u58eb\u4f4f\u623f\u77ed\u7f3a (2025)",

    # Tabs
    tab_availability  = "\u53ef\u7528\u6027",
    tab_affordability = "\u8d1f\u62c5\u80fd\u529b",
    tab_result        = "\u7ed3\u679c",

    # Mode toggle
    toggle_pro       = "Pro",
    toggle_easy      = "Easy",

    # Layer labels
    layer_vacancy_rate   = "\u7a7a\u7f6e\u7387",
    layer_avg_rent       = "\u5e73\u5747\u62db\u79df\u79df\u91d1",
    layer_median_slider  = "\u6309\u623f\u578b\u4e2d\u4f4d\u79df\u91d1",
    layer_quote_gesamt   = "\u5e7f\u544a\u8986\u76d6\u7387\uff08\u4e0d\u786e\u5b9a\u6027\uff09",
    layer_share_avg_rent = "\u8d1f\u62c5\u80fd\u529b\uff08\u5bb6\u5ead\u6bd4\u4f8b\uff09",
    layer_slider         = "\u79df\u91d1\u5206\u6790",
    layer_bivariate_v1   = "\u7a7a\u7f6e\u7387 \u00d7 \u8d1f\u62c5\u80fd\u529b",
    layer_bivariate_v2   = "\u7a7a\u7f6e\u7387 \u00d7 \u8d1f\u62c5\u80fd\u529b (4\u00d74)",

    # Layer info texts
    info_vacancy_rate   = "\u7a7a\u7f6e\u7387\u663e\u793a\u5404\u5e02\u9547\u7a7a\u7f6e\u4f4f\u623f\u5360\u603b\u4f4f\u623f\u5b58\u91cf\u7684\u6bd4\u4f8b\u3002\u6839\u636e\u8054\u90a6\u4f4f\u623f\u5c40\uff08BWO\uff09\u7684\u6807\u51c6\uff0c\u7a7a\u7f6e\u7387\u4f4e\u4e8e1%\u8868\u793a\u4f4f\u623f\u5371\u673a\uff0c1%\u81f31.5%\u8868\u793a\u4f4f\u623f\u77ed\u7f3a\uff08\u53c2\u89c1SVIT, 2023; NZZ, 2023\uff09\u3002\u53d1\u6563\u8272\u6807\u53cd\u6620\u4e86\u8fd9\u4e9b\u9608\u503c\u3002\u6570\u636e\u6765\u6e90\uff1aBFS STAT-TAB, 2025\u3002",
    info_avg_rent       = "\u8be5\u5730\u56fe\u663e\u793a\u5404\u5e02\u9547\u7684\u5e73\u5747\u62db\u79df\u79df\u91d1\u3002\u8ba1\u7b97\u57fa\u4e8e\u6240\u6709\u623f\u578b\u7684\u4e2d\u4f4d\u4ef7\u683c\uff08\u56fe\u5c42\u300a\u6309\u623f\u578b\u4e2d\u4f4d\u79df\u91d1\u300b\uff09\uff0c\u6309\u7167\u5efa\u7b51\u548c\u4f4f\u623f\u767b\u8bb0\u518c\uff08GWR\uff09\u7684\u4f4f\u623f\u5b58\u91cf\u8fdb\u884c\u52a0\u6743\u3002\u6570\u636e\u6765\u6e90\uff1aLookmove / Watson, 2025\u3002",
    info_median_slider  = "\u663e\u793a\u6309\u623f\u578b\u548c\u5e02\u9547\u5212\u5206\u7684\u62db\u79df\u4f4f\u623f\u4e2d\u4f4d\u79df\u91d1\u3002\u90ae\u653f\u7f16\u7801\u5c42\u9762\u7684\u539f\u59cb\u6570\u636e\u5df2\u6309\u6bd4\u4f8b\u52a0\u6743\u5e76\u5efa\u6a21\u5230\u5e02\u9547\u5c42\u9762\u3002\u8fd9\u4e9b\u6570\u636e\u662f\u8ba1\u7b97\u5e73\u5747\u62db\u79df\u79df\u91d1\u7684\u57fa\u7840\u3002\u6570\u636e\u6765\u6e90\uff1aLookmove, 2025\u3002",
    info_quote_gesamt   = "\u62db\u79df\u79df\u91d1\u7684\u8ba1\u7b97\u4f7f\u7528\u4e86Lookmove\u7684\u5e7f\u544a\u6570\u636e\u3002\u8fd9\u4e9b\u4ec5\u4ee3\u8868\u5b9e\u9645\u4f4f\u623f\u5e02\u573a\u7684\u4e00\u4e2a\u6837\u672c\uff0c\u56e0\u6b64\u5b58\u5728\u4e0d\u51c6\u786e\u6027\u3002\u8be5\u5730\u56fe\u53ef\u89c6\u5316\u6570\u636e\u8986\u76d6\u8303\u56f4\uff1a\u663e\u793a\u5e02\u9547\u603b\u4f4f\u623f\u5b58\u91cf\u4e2d\u6709\u591a\u5c11\u767e\u5206\u6bd4\u88ab\u5e7f\u544a\u8986\u76d6\u3002\u4f4e\u503c\u8868\u793a\u6570\u636e\u57fa\u7840\u8f83\u5c0f\uff0c\u4e0d\u786e\u5b9a\u6027\u8f83\u9ad8\u3002\u6570\u636e\u6765\u6e90\uff1aLookmove / GWR, 2025\u3002",
    info_share_avg_rent = "\u8be5\u56fe\u5c42\u5c06\u5e73\u5747\u62db\u79df\u79df\u91d1\uff08Lookmove, 2025\uff09\u4e0e\u79df\u91d1\u8d1f\u62c5\u80fd\u529b\u6570\u636e\uff08\u9700\u6c42\u76d1\u6d4b, 2023\uff09\u76f8\u7ed3\u5408\u3002\u5b83\u663e\u793a\u6709\u591a\u5c11\u767e\u5206\u6bd4\u7684\u5bb6\u5ead\u80fd\u591f\u8d1f\u62c5\u5f97\u8d77\u5176\u5e02\u9547\u5e02\u573a\u4e0a\u7684\u5e73\u5747\u4f4f\u623f\u3002\u5982\u679c\u79df\u91d1\u4e0d\u8d85\u8fc7\u53ef\u652f\u914d\u51c0\u6536\u5165\u768430%\uff0c\u5219\u88ab\u89c6\u4e3a\u53ef\u8d1f\u62c5\u3002\u6570\u636e\u6765\u6e90\uff1aLookmove / \u79df\u623f\u9700\u6c42\u76d1\u6d4b, 2023/2025\u3002",
    info_slider         = "\u4ea4\u4e92\u5f0f\u5206\u6790\uff1a\u8f93\u5165\u79df\u91d1\u91d1\u989d\uff08CHF\uff09\u67e5\u770b\u6709\u591a\u5c11\u5bb6\u5ead\u80fd\u591f\u8d1f\u62c5\u2014\u2014\u6216\u53cd\u8fc7\u6765\uff0c\u9009\u62e9\u5bb6\u5ead\u6bd4\u4f8b\u67e5\u770b\u5bf9\u5e94\u79df\u91d1\u3002\u663e\u793a\u7684\u503c\u4e3a\u6bdb\u79df\u91d1\uff08\u542b\u6742\u8d39\uff09\u3002\u6570\u636e\u6765\u6e90\uff1a\u9700\u6c42\u76d1\u6d4b, 2023\u3002",
    info_bivariate_v1   = "\u8be5\u5730\u56fe\u7ed3\u5408\u4e86\u524d\u8ff0\u56fe\u5c42\u7684\u7ed3\u679c\uff1a\u7a7a\u7f6e\u7387\uff08\u53ef\u7528\u6027\uff09\u548c\u5e73\u5747\u62db\u79df\u79df\u91d1\u7684\u8d1f\u62c5\u80fd\u529b\u3002\u7a7a\u7f6e\u7387\u6309\u56fa\u5b9a\u9608\u503c\u5206\u4e3a\u4e09\u7c7b\uff080\u20130.5%\u30010.5\u20132%\u30012%\u4ee5\u4e0a\uff09\u3002\u8d1f\u62c5\u80fd\u529b\u6309\u5206\u4f4d\u6570\u5206\u7c7b\uff0c\u4f7f\u6bcf\u4e2a\u7c7b\u522b\u5305\u542b\u76f8\u540c\u6570\u91cf\u7684\u5e02\u9547\u3002\u7ec4\u5408\u4ea7\u751f3\u00d73\u77e9\u9635\uff1a\u6df1\u8272\u8868\u793a\u7a7a\u7f6e\u7387\u4f4e\u4e14\u8d1f\u62c5\u80fd\u529b\u4f4e\u7684\u5e02\u9547\u2014\u2014\u4f4f\u623f\u5e02\u573a\u6700\u4e3a\u7d27\u5f20\u3002\u901a\u8fc7\u300a\u9762\u79ef\u4e0e\u4eba\u53e3\u6210\u6bd4\u4f8b\u300b\u9009\u9879\uff0c\u5e02\u9547\u4ee5Dorling\u5236\u56fe\u6cd5\u663e\u793a\uff0c\u4ee5\u7a81\u51fa\u4eba\u53e3\u7edf\u8ba1\u610f\u4e49\u3002",
    info_bivariate_v2   = "\u6269\u5c55\u7684\u53cc\u53d8\u91cf\u663e\u793a\uff0c\u91c7\u75284\u00d74\u5206\u7c7b\uff0c\u5bf9\u7a7a\u7f6e\u7387\u548c\u8d1f\u62c5\u80fd\u529b\u8fdb\u884c\u66f4\u7ec6\u5316\u7684\u5206\u6790\u3002\u7a7a\u7f6e\u7387\u5206\u4e3a\u56db\u7c7b\uff080\u20130.5%\u30010.5\u20131%\u30011\u20131.5%\u30011.5%\u4ee5\u4e0a\uff09\uff0c\u8d1f\u62c5\u80fd\u529b\u540c\u6837\u6309\u5206\u4f4d\u6570\u5206\u4e3a\u56db\u7c7b\u3002\u66f4\u7cbe\u7ec6\u7684\u5206\u7ea7\u4f7f\u5f97\u80fd\u591f\u66f4\u51c6\u786e\u5730\u533a\u5206\u4f4f\u623f\u5371\u673a\u3001\u4f4f\u623f\u77ed\u7f3a\u548c\u7a7a\u7f6e\u7387\u5bbd\u677e\u7684\u5e02\u9547\u3002",

    # Layer titles
    title_vacancy_rate   = "\u7a7a\u7f6e\u4f4f\u623f\u5360\u603b\u4f4f\u623f\u5b58\u91cf\u7684\u6bd4\u4f8b",
    title_avg_rent       = "\u5404\u5e02\u9547\u52a0\u6743\u5e73\u5747\u62db\u79df\u79df\u91d1",
    title_median_slider  = "\u6309\u623f\u578b\u5212\u5206\u7684\u62db\u79df\u4f4f\u623f\u4e2d\u4f4d\u79df\u91d1",
    title_quote_gesamt   = "\u5206\u6790\u4e2d\u6db5\u76d6\u7684\u4f4f\u623f\u5b58\u91cf\u6bd4\u4f8b\uff08\u4e0d\u786e\u5b9a\u6027\u6307\u6807\uff09",
    title_share_avg_rent = "\u80fd\u591f\u8d1f\u62c5\u5e73\u5747\u62db\u79df\u79df\u91d1\u7684\u5bb6\u5ead\u6bd4\u4f8b",
    title_slider         = "\u591a\u5c11\u5bb6\u5ead\u80fd\u591f\u8d1f\u62c5\u591a\u5c11\u79df\u91d1\uff1f",
    title_bivariate_v1   = "\u53cc\u53d8\u91cf\u5730\u56fe\uff1a\u7a7a\u7f6e\u7387 \u00d7 \u8d1f\u62c5\u80fd\u529b (3\u00d73)",
    title_bivariate_v2   = "\u6269\u5c55\u53cc\u53d8\u91cf\u5730\u56fe (4\u00d74)",

    # Control labels
    label_map_view    = "\u5730\u56fe\u89c6\u56fe\uff1a",
    label_room_size   = "\u623f\u578b\uff1a",
    label_analysis    = "\u5206\u6790\uff1a",
    label_rent        = "\u79df\u91d1\uff08CHF/\u6708\uff09",
    label_share       = "\u5bb6\u5ead\u6bd4\u4f8b",
    label_dorling     = "\u6309\u4eba\u53e3\u6bd4\u4f8b\u663e\u793a\u9762\u79ef",
    label_information = "\u4fe1\u606f",
    label_rent_to_share = "\u4ece\u79df\u91d1\u5230\u5bb6\u5ead",
    label_share_to_rent = "\u4ece\u5bb6\u5ead\u5230\u79df\u91d1",

    # Placeholder
    click_placeholder = "\u70b9\u51fb\u5e02\u9547\u4ee5\u4e86\u89e3\u66f4\u591a\u4fe1\u606f",

    # Popup labels
    popup_municipality    = "\u5e02\u9547",
    popup_share_pct       = "\u53ef\u8d1f\u62c5\u6bd4\u4f8b (%)",
    popup_avg_rent        = "\u5e73\u5747\u79df\u91d1 (CHF)",
    popup_vacancy_pct     = "\u7a7a\u7f6e\u7387 (%)",
    popup_gwr_total       = "GWR\u4f4f\u623f",
    popup_inserate        = "\u5e7f\u544a",
    popup_inseratequote   = "\u5e7f\u544a\u8986\u76d6\u7387",
    popup_class           = "\u7c7b\u522b",
    popup_vacancy_label   = "\u7a7a\u7f6e\u7387",
    popup_afford_label    = "\u8d1f\u62c5\u80fd\u529b",
    popup_median_rent     = "\u4e2d\u4f4d\u79df\u91d1",
    popup_n_inserate      = "\u5e7f\u544a\u6570\u91cf",
    popup_share_households = "\u5bb6\u5ead\u6bd4\u4f8b",
    popup_rent_chf        = "\u79df\u91d1 (CHF)",
    popup_data_coverage   = "\u6570\u636e\u8986\u76d6\u7387 (%)",

    # Legend
    legend_no_data    = "\u65e0\u6570\u636e",
    legend_median_prefix = "\u4e2d\u4f4d\u79df\u91d1",
    legend_share_at   = "\u6bd4\u4f8b\u4e3a",
    legend_rent_at    = "\u79df\u91d1\u4e3a",

    # Bivariate labels
    biv_low           = "\u4f4e",
    biv_mid           = "\u4e2d",
    biv_high          = "\u9ad8",
    biv_mid_low       = "\u4e2d\u4f4e",
    biv_mid_high      = "\u4e2d\u9ad8",

    # Legend axis labels
    legend_vacancy       = "\u7a7a\u7f6e\u7387",
    legend_affordability = "\u8d1f\u62c5\u80fd\u529b",
    legend_unaffordability = "\u4e0d\u53ef\u8d1f\u62c5\u6027"
  ),

  th = list(
    # App
    app_title        = "\u0e01\u0e32\u0e23\u0e02\u0e32\u0e14\u0e41\u0e04\u0e25\u0e19\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22\u0e43\u0e19\u0e2a\u0e27\u0e34\u0e15\u0e40\u0e0b\u0e2d\u0e23\u0e4c\u0e41\u0e25\u0e19\u0e14\u0e4c (2025)",

    # Tabs
    tab_availability  = "\u0e04\u0e27\u0e32\u0e21\u0e1e\u0e23\u0e49\u0e2d\u0e21\u0e43\u0e0a\u0e49\u0e07\u0e32\u0e19",
    tab_affordability = "\u0e04\u0e27\u0e32\u0e21\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e43\u0e19\u0e01\u0e32\u0e23\u0e08\u0e48\u0e32\u0e22",
    tab_result        = "\u0e1c\u0e25\u0e25\u0e31\u0e1e\u0e18\u0e4c",

    # Mode toggle
    toggle_pro       = "Pro",
    toggle_easy      = "Easy",

    # Layer labels
    layer_vacancy_rate   = "\u0e2d\u0e31\u0e15\u0e23\u0e32\u0e2b\u0e49\u0e2d\u0e07\u0e27\u0e48\u0e32\u0e07",
    layer_avg_rent       = "\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e40\u0e2a\u0e19\u0e2d\u0e40\u0e09\u0e25\u0e35\u0e48\u0e22",
    layer_median_slider  = "\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e21\u0e31\u0e18\u0e22\u0e10\u0e32\u0e19\u0e15\u0e32\u0e21\u0e1b\u0e23\u0e30\u0e40\u0e20\u0e17\u0e2b\u0e49\u0e2d\u0e07",
    layer_quote_gesamt   = "\u0e04\u0e27\u0e32\u0e21\u0e04\u0e23\u0e2d\u0e1a\u0e04\u0e25\u0e38\u0e21\u0e02\u0e2d\u0e07\u0e1b\u0e23\u0e30\u0e01\u0e32\u0e28 (\u0e04\u0e27\u0e32\u0e21\u0e44\u0e21\u0e48\u0e41\u0e19\u0e48\u0e19\u0e2d\u0e19)",
    layer_share_avg_rent = "\u0e04\u0e27\u0e32\u0e21\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e43\u0e19\u0e01\u0e32\u0e23\u0e08\u0e48\u0e32\u0e22 (\u0e04\u0e23\u0e31\u0e27\u0e40\u0e23\u0e37\u0e2d\u0e19 %)",
    layer_slider         = "\u0e01\u0e32\u0e23\u0e27\u0e34\u0e40\u0e04\u0e23\u0e32\u0e30\u0e2b\u0e4c\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32",
    layer_bivariate_v1   = "\u0e2b\u0e49\u0e2d\u0e07\u0e27\u0e48\u0e32\u0e07 \u00d7 \u0e04\u0e27\u0e32\u0e21\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e43\u0e19\u0e01\u0e32\u0e23\u0e08\u0e48\u0e32\u0e22",
    layer_bivariate_v2   = "\u0e2b\u0e49\u0e2d\u0e07\u0e27\u0e48\u0e32\u0e07 \u00d7 \u0e04\u0e27\u0e32\u0e21\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e43\u0e19\u0e01\u0e32\u0e23\u0e08\u0e48\u0e32\u0e22 (4\u00d74)",

    # Layer info texts
    info_vacancy_rate   = "\u0e2d\u0e31\u0e15\u0e23\u0e32\u0e2b\u0e49\u0e2d\u0e07\u0e27\u0e48\u0e32\u0e07\u0e41\u0e2a\u0e14\u0e07\u0e2a\u0e31\u0e14\u0e2a\u0e48\u0e27\u0e19\u0e02\u0e2d\u0e07\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22\u0e17\u0e35\u0e48\u0e27\u0e48\u0e32\u0e07\u0e40\u0e21\u0e37\u0e48\u0e2d\u0e40\u0e17\u0e35\u0e22\u0e1a\u0e01\u0e31\u0e1a\u0e2a\u0e15\u0e47\u0e2d\u0e01\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22\u0e17\u0e31\u0e49\u0e07\u0e2b\u0e21\u0e14\u0e43\u0e19\u0e41\u0e15\u0e48\u0e25\u0e30\u0e40\u0e17\u0e28\u0e1a\u0e32\u0e25 \u0e15\u0e32\u0e21\u0e2a\u0e33\u0e19\u0e31\u0e01\u0e07\u0e32\u0e19\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22\u0e41\u0e2b\u0e48\u0e07\u0e2a\u0e2b\u0e1e\u0e31\u0e19\u0e18\u0e4c (BWO) \u0e2d\u0e31\u0e15\u0e23\u0e32\u0e2b\u0e49\u0e2d\u0e07\u0e27\u0e48\u0e32\u0e07\u0e15\u0e48\u0e33\u0e01\u0e27\u0e48\u0e32 1% \u0e1a\u0e48\u0e07\u0e0a\u0e35\u0e49\u0e27\u0e48\u0e32\u0e40\u0e01\u0e34\u0e14\u0e27\u0e34\u0e01\u0e24\u0e15\u0e14\u0e49\u0e32\u0e19\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22 \u0e41\u0e25\u0e30\u0e23\u0e30\u0e2b\u0e27\u0e48\u0e32\u0e07 1% \u0e16\u0e36\u0e07 1.5% \u0e1a\u0e48\u0e07\u0e0a\u0e35\u0e49\u0e27\u0e48\u0e32\u0e02\u0e32\u0e14\u0e41\u0e04\u0e25\u0e19\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22 (\u0e14\u0e39 SVIT, 2023; NZZ, 2023) \u0e2a\u0e40\u0e01\u0e25\u0e2a\u0e35\u0e41\u0e1a\u0e1a\u0e41\u0e22\u0e01\u0e2a\u0e35\u0e2a\u0e30\u0e17\u0e49\u0e2d\u0e19\u0e04\u0e48\u0e32\u0e40\u0e01\u0e13\u0e11\u0e4c\u0e40\u0e2b\u0e25\u0e48\u0e32\u0e19\u0e35\u0e49 \u0e41\u0e2b\u0e25\u0e48\u0e07\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25: BFS STAT-TAB, 2025",
    info_avg_rent       = "\u0e41\u0e1c\u0e19\u0e17\u0e35\u0e48\u0e19\u0e35\u0e49\u0e41\u0e2a\u0e14\u0e07\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e40\u0e2a\u0e19\u0e2d\u0e40\u0e09\u0e25\u0e35\u0e48\u0e22\u0e15\u0e48\u0e2d\u0e40\u0e17\u0e28\u0e1a\u0e32\u0e25 \u0e04\u0e33\u0e19\u0e27\u0e13\u0e08\u0e32\u0e01\u0e04\u0e48\u0e32\u0e21\u0e31\u0e18\u0e22\u0e10\u0e32\u0e19\u0e02\u0e2d\u0e07\u0e17\u0e38\u0e01\u0e1b\u0e23\u0e30\u0e40\u0e20\u0e17\u0e2b\u0e49\u0e2d\u0e07 (\u0e0a\u0e31\u0e49\u0e19\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25 \u00ab\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e21\u0e31\u0e18\u0e22\u0e10\u0e32\u0e19\u0e15\u0e32\u0e21\u0e1b\u0e23\u0e30\u0e40\u0e20\u0e17\u0e2b\u0e49\u0e2d\u0e07\u00bb) \u0e16\u0e48\u0e27\u0e07\u0e19\u0e49\u0e33\u0e2b\u0e19\u0e31\u0e01\u0e15\u0e32\u0e21\u0e2a\u0e15\u0e47\u0e2d\u0e01\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22\u0e08\u0e32\u0e01\u0e17\u0e30\u0e40\u0e1a\u0e35\u0e22\u0e19\u0e2d\u0e32\u0e04\u0e32\u0e23\u0e41\u0e25\u0e30\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22 (GWR) \u0e41\u0e2b\u0e25\u0e48\u0e07\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25: Lookmove / Watson, 2025",
    info_median_slider  = "\u0e41\u0e2a\u0e14\u0e07\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e21\u0e31\u0e18\u0e22\u0e10\u0e32\u0e19\u0e02\u0e2d\u0e07\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22\u0e43\u0e2b\u0e49\u0e40\u0e0a\u0e48\u0e32\u0e15\u0e32\u0e21\u0e1b\u0e23\u0e30\u0e40\u0e20\u0e17\u0e2b\u0e49\u0e2d\u0e07\u0e41\u0e25\u0e30\u0e40\u0e17\u0e28\u0e1a\u0e32\u0e25 \u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25\u0e15\u0e49\u0e19\u0e09\u0e1a\u0e31\u0e1a\u0e23\u0e30\u0e14\u0e31\u0e1a\u0e23\u0e2b\u0e31\u0e2a\u0e44\u0e1b\u0e23\u0e29\u0e13\u0e35\u0e22\u0e4c\u0e16\u0e39\u0e01\u0e16\u0e48\u0e27\u0e07\u0e19\u0e49\u0e33\u0e2b\u0e19\u0e31\u0e01\u0e41\u0e25\u0e30\u0e08\u0e33\u0e25\u0e2d\u0e07\u0e2a\u0e39\u0e48\u0e23\u0e30\u0e14\u0e31\u0e1a\u0e40\u0e17\u0e28\u0e1a\u0e32\u0e25 \u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25\u0e40\u0e2b\u0e25\u0e48\u0e32\u0e19\u0e35\u0e49\u0e40\u0e1b\u0e47\u0e19\u0e1e\u0e37\u0e49\u0e19\u0e10\u0e32\u0e19\u0e2a\u0e33\u0e2b\u0e23\u0e31\u0e1a\u0e01\u0e32\u0e23\u0e04\u0e33\u0e19\u0e27\u0e13\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e40\u0e2a\u0e19\u0e2d\u0e40\u0e09\u0e25\u0e35\u0e48\u0e22 \u0e41\u0e2b\u0e25\u0e48\u0e07\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25: Lookmove, 2025",
    info_quote_gesamt   = "\u0e01\u0e32\u0e23\u0e04\u0e33\u0e19\u0e27\u0e13\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e40\u0e2a\u0e19\u0e2d\u0e43\u0e0a\u0e49\u0e1b\u0e23\u0e30\u0e01\u0e32\u0e28\u0e08\u0e32\u0e01 Lookmove \u0e0b\u0e36\u0e48\u0e07\u0e40\u0e1b\u0e47\u0e19\u0e40\u0e1e\u0e35\u0e22\u0e07\u0e15\u0e31\u0e27\u0e2d\u0e22\u0e48\u0e32\u0e07\u0e02\u0e2d\u0e07\u0e15\u0e25\u0e32\u0e14\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22\u0e08\u0e23\u0e34\u0e07\u0e40\u0e17\u0e48\u0e32\u0e19\u0e31\u0e49\u0e19 \u0e08\u0e36\u0e07\u0e2d\u0e32\u0e08\u0e21\u0e35\u0e04\u0e27\u0e32\u0e21\u0e44\u0e21\u0e48\u0e41\u0e21\u0e48\u0e19\u0e22\u0e33 \u0e41\u0e1c\u0e19\u0e17\u0e35\u0e48\u0e19\u0e35\u0e49\u0e41\u0e2a\u0e14\u0e07\u0e04\u0e27\u0e32\u0e21\u0e04\u0e23\u0e2d\u0e1a\u0e04\u0e25\u0e38\u0e21\u0e02\u0e2d\u0e07\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25: \u0e41\u0e2a\u0e14\u0e07\u0e27\u0e48\u0e32\u0e2a\u0e15\u0e47\u0e2d\u0e01\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22\u0e17\u0e31\u0e49\u0e07\u0e2b\u0e21\u0e14\u0e02\u0e2d\u0e07\u0e40\u0e17\u0e28\u0e1a\u0e32\u0e25\u0e21\u0e35\u0e01\u0e35\u0e48\u0e40\u0e1b\u0e2d\u0e23\u0e4c\u0e40\u0e0b\u0e47\u0e19\u0e15\u0e4c\u0e17\u0e35\u0e48\u0e1b\u0e23\u0e32\u0e01\u0e0f\u0e43\u0e19\u0e1b\u0e23\u0e30\u0e01\u0e32\u0e28 \u0e04\u0e48\u0e32\u0e15\u0e48\u0e33\u0e2b\u0e21\u0e32\u0e22\u0e16\u0e36\u0e07\u0e10\u0e32\u0e19\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25\u0e17\u0e35\u0e48\u0e19\u0e49\u0e2d\u0e22\u0e01\u0e27\u0e48\u0e32\u0e41\u0e25\u0e30\u0e04\u0e27\u0e32\u0e21\u0e44\u0e21\u0e48\u0e41\u0e19\u0e48\u0e19\u0e2d\u0e19\u0e17\u0e35\u0e48\u0e2a\u0e39\u0e07\u0e01\u0e27\u0e48\u0e32 \u0e41\u0e2b\u0e25\u0e48\u0e07\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25: Lookmove / GWR, 2025",
    info_share_avg_rent = "\u0e0a\u0e31\u0e49\u0e19\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25\u0e19\u0e35\u0e49\u0e23\u0e27\u0e21\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e40\u0e2a\u0e19\u0e2d\u0e40\u0e09\u0e25\u0e35\u0e48\u0e22 (Lookmove, 2025) \u0e01\u0e31\u0e1a\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25\u0e04\u0e27\u0e32\u0e21\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e43\u0e19\u0e01\u0e32\u0e23\u0e08\u0e48\u0e32\u0e22\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32 (\u0e15\u0e31\u0e27\u0e15\u0e34\u0e14\u0e15\u0e32\u0e21\u0e2d\u0e38\u0e1b\u0e2a\u0e07\u0e04\u0e4c, 2023) \u0e41\u0e2a\u0e14\u0e07\u0e27\u0e48\u0e32\u0e04\u0e23\u0e31\u0e27\u0e40\u0e23\u0e37\u0e2d\u0e19\u0e01\u0e35\u0e48\u0e40\u0e1b\u0e2d\u0e23\u0e4c\u0e40\u0e0b\u0e47\u0e19\u0e15\u0e4c\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e08\u0e48\u0e32\u0e22\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22\u0e40\u0e09\u0e25\u0e35\u0e48\u0e22\u0e43\u0e19\u0e15\u0e25\u0e32\u0e14\u0e02\u0e2d\u0e07\u0e40\u0e17\u0e28\u0e1a\u0e32\u0e25\u0e44\u0e14\u0e49 \u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e16\u0e37\u0e2d\u0e27\u0e48\u0e32\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e08\u0e48\u0e32\u0e22\u0e44\u0e14\u0e49\u0e2b\u0e32\u0e01\u0e44\u0e21\u0e48\u0e40\u0e01\u0e34\u0e19 30% \u0e02\u0e2d\u0e07\u0e23\u0e32\u0e22\u0e44\u0e14\u0e49\u0e2a\u0e38\u0e17\u0e18\u0e34\u0e17\u0e35\u0e48\u0e43\u0e0a\u0e49\u0e08\u0e48\u0e32\u0e22\u0e44\u0e14\u0e49 \u0e41\u0e2b\u0e25\u0e48\u0e07\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25: Lookmove / \u0e15\u0e31\u0e27\u0e15\u0e34\u0e14\u0e15\u0e32\u0e21\u0e2d\u0e38\u0e1b\u0e2a\u0e07\u0e04\u0e4c\u0e01\u0e32\u0e23\u0e40\u0e0a\u0e48\u0e32, 2023/2025",
    info_slider         = "\u0e01\u0e32\u0e23\u0e27\u0e34\u0e40\u0e04\u0e23\u0e32\u0e30\u0e2b\u0e4c\u0e41\u0e1a\u0e1a\u0e42\u0e15\u0e49\u0e15\u0e2d\u0e1a: \u0e1b\u0e49\u0e2d\u0e19\u0e08\u0e33\u0e19\u0e27\u0e19\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32 (CHF) \u0e40\u0e1e\u0e37\u0e48\u0e2d\u0e14\u0e39\u0e27\u0e48\u0e32\u0e04\u0e23\u0e31\u0e27\u0e40\u0e23\u0e37\u0e2d\u0e19\u0e01\u0e35\u0e48\u0e2a\u0e31\u0e14\u0e2a\u0e48\u0e27\u0e19\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e08\u0e48\u0e32\u0e22\u0e44\u0e14\u0e49 \u2013 \u0e2b\u0e23\u0e37\u0e2d\u0e40\u0e25\u0e37\u0e2d\u0e01\u0e2a\u0e31\u0e14\u0e2a\u0e48\u0e27\u0e19\u0e04\u0e23\u0e31\u0e27\u0e40\u0e23\u0e37\u0e2d\u0e19\u0e40\u0e1e\u0e37\u0e48\u0e2d\u0e14\u0e39\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e17\u0e35\u0e48\u0e2a\u0e2d\u0e14\u0e04\u0e25\u0e49\u0e2d\u0e07 \u0e04\u0e48\u0e32\u0e17\u0e35\u0e48\u0e41\u0e2a\u0e14\u0e07\u0e40\u0e1b\u0e47\u0e19\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e23\u0e27\u0e21 (\u0e23\u0e27\u0e21\u0e04\u0e48\u0e32\u0e2a\u0e32\u0e18\u0e32\u0e23\u0e13\u0e39\u0e1b\u0e42\u0e20\u0e04) \u0e41\u0e2b\u0e25\u0e48\u0e07\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25: \u0e15\u0e31\u0e27\u0e15\u0e34\u0e14\u0e15\u0e32\u0e21\u0e2d\u0e38\u0e1b\u0e2a\u0e07\u0e04\u0e4c, 2023",
    info_bivariate_v1   = "\u0e41\u0e1c\u0e19\u0e17\u0e35\u0e48\u0e19\u0e35\u0e49\u0e23\u0e27\u0e21\u0e1c\u0e25\u0e08\u0e32\u0e01\u0e0a\u0e31\u0e49\u0e19\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25\u0e01\u0e48\u0e2d\u0e19\u0e2b\u0e19\u0e49\u0e32: \u0e2d\u0e31\u0e15\u0e23\u0e32\u0e2b\u0e49\u0e2d\u0e07\u0e27\u0e48\u0e32\u0e07 (\u0e04\u0e27\u0e32\u0e21\u0e1e\u0e23\u0e49\u0e2d\u0e21\u0e43\u0e0a\u0e49\u0e07\u0e32\u0e19) \u0e41\u0e25\u0e30\u0e04\u0e27\u0e32\u0e21\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e43\u0e19\u0e01\u0e32\u0e23\u0e08\u0e48\u0e32\u0e22\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e40\u0e2a\u0e19\u0e2d\u0e40\u0e09\u0e25\u0e35\u0e48\u0e22 \u0e2d\u0e31\u0e15\u0e23\u0e32\u0e2b\u0e49\u0e2d\u0e07\u0e27\u0e48\u0e32\u0e07\u0e41\u0e1a\u0e48\u0e07\u0e40\u0e1b\u0e47\u0e19 3 \u0e23\u0e30\u0e14\u0e31\u0e1a\u0e15\u0e32\u0e21\u0e40\u0e01\u0e13\u0e11\u0e4c\u0e04\u0e07\u0e17\u0e35\u0e48 (0\u20130.5%, 0.5\u20132%, \u0e21\u0e32\u0e01\u0e01\u0e27\u0e48\u0e32 2%) \u0e04\u0e27\u0e32\u0e21\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e43\u0e19\u0e01\u0e32\u0e23\u0e08\u0e48\u0e32\u0e22\u0e41\u0e1a\u0e48\u0e07\u0e15\u0e32\u0e21\u0e04\u0e27\u0e2d\u0e44\u0e17\u0e25\u0e4c\u0e42\u0e14\u0e22\u0e41\u0e15\u0e48\u0e25\u0e30\u0e23\u0e30\u0e14\u0e31\u0e1a\u0e21\u0e35\u0e08\u0e33\u0e19\u0e27\u0e19\u0e40\u0e17\u0e28\u0e1a\u0e32\u0e25\u0e40\u0e17\u0e48\u0e32\u0e01\u0e31\u0e19 \u0e1c\u0e25\u0e25\u0e31\u0e1e\u0e18\u0e4c\u0e40\u0e1b\u0e47\u0e19\u0e40\u0e21\u0e17\u0e23\u0e34\u0e01\u0e0b\u0e4c 3\u00d73: \u0e42\u0e17\u0e19\u0e2a\u0e35\u0e40\u0e02\u0e49\u0e21\u0e1a\u0e48\u0e07\u0e0a\u0e35\u0e49\u0e40\u0e17\u0e28\u0e1a\u0e32\u0e25\u0e17\u0e35\u0e48\u0e21\u0e35\u0e2d\u0e31\u0e15\u0e23\u0e32\u0e2b\u0e49\u0e2d\u0e07\u0e27\u0e48\u0e32\u0e07\u0e15\u0e48\u0e33\u0e41\u0e25\u0e30\u0e04\u0e27\u0e32\u0e21\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e43\u0e19\u0e01\u0e32\u0e23\u0e08\u0e48\u0e32\u0e22\u0e15\u0e48\u0e33 \u0e15\u0e31\u0e27\u0e40\u0e25\u0e37\u0e2d\u0e01 \u00ab\u0e41\u0e2a\u0e14\u0e07\u0e1e\u0e37\u0e49\u0e19\u0e17\u0e35\u0e48\u0e15\u0e32\u0e21\u0e2a\u0e31\u0e14\u0e2a\u0e48\u0e27\u0e19\u0e1b\u0e23\u0e30\u0e0a\u0e32\u0e01\u0e23\u00bb \u0e41\u0e2a\u0e14\u0e07\u0e40\u0e17\u0e28\u0e1a\u0e32\u0e25\u0e40\u0e1b\u0e47\u0e19\u0e41\u0e1c\u0e19\u0e17\u0e35\u0e48\u0e04\u0e32\u0e23\u0e4c\u0e42\u0e15\u0e41\u0e01\u0e23\u0e21 Dorling \u0e40\u0e1e\u0e37\u0e48\u0e2d\u0e40\u0e19\u0e49\u0e19\u0e04\u0e27\u0e32\u0e21\u0e2a\u0e33\u0e04\u0e31\u0e0d\u0e17\u0e32\u0e07\u0e1b\u0e23\u0e30\u0e0a\u0e32\u0e01\u0e23\u0e28\u0e32\u0e2a\u0e15\u0e23\u0e4c",
    info_bivariate_v2   = "\u0e01\u0e32\u0e23\u0e41\u0e2a\u0e14\u0e07\u0e1c\u0e25\u0e41\u0e1a\u0e1a\u0e17\u0e27\u0e34\u0e20\u0e32\u0e04\u0e35\u0e02\u0e22\u0e32\u0e22\u0e14\u0e49\u0e27\u0e22\u0e01\u0e32\u0e23\u0e41\u0e1a\u0e48\u0e07\u0e0a\u0e31\u0e49\u0e19 4\u00d74 \u0e40\u0e1e\u0e37\u0e48\u0e2d\u0e27\u0e34\u0e40\u0e04\u0e23\u0e32\u0e30\u0e2b\u0e4c\u0e2d\u0e31\u0e15\u0e23\u0e32\u0e2b\u0e49\u0e2d\u0e07\u0e27\u0e48\u0e32\u0e07\u0e41\u0e25\u0e30\u0e04\u0e27\u0e32\u0e21\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e43\u0e19\u0e01\u0e32\u0e23\u0e08\u0e48\u0e32\u0e22\u0e44\u0e14\u0e49\u0e25\u0e30\u0e40\u0e2d\u0e35\u0e22\u0e14\u0e22\u0e34\u0e48\u0e07\u0e02\u0e36\u0e49\u0e19 \u0e2d\u0e31\u0e15\u0e23\u0e32\u0e2b\u0e49\u0e2d\u0e07\u0e27\u0e48\u0e32\u0e07\u0e41\u0e1a\u0e48\u0e07\u0e40\u0e1b\u0e47\u0e19 4 \u0e23\u0e30\u0e14\u0e31\u0e1a (0\u20130.5%, 0.5\u20131%, 1\u20131.5%, \u0e21\u0e32\u0e01\u0e01\u0e27\u0e48\u0e32 1.5%) \u0e41\u0e25\u0e30\u0e04\u0e27\u0e32\u0e21\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e43\u0e19\u0e01\u0e32\u0e23\u0e08\u0e48\u0e32\u0e22\u0e41\u0e1a\u0e48\u0e07\u0e15\u0e32\u0e21\u0e04\u0e27\u0e2d\u0e44\u0e17\u0e25\u0e4c\u0e40\u0e1b\u0e47\u0e19 4 \u0e23\u0e30\u0e14\u0e31\u0e1a \u0e01\u0e32\u0e23\u0e41\u0e1a\u0e48\u0e07\u0e0a\u0e31\u0e49\u0e19\u0e17\u0e35\u0e48\u0e25\u0e30\u0e40\u0e2d\u0e35\u0e22\u0e14\u0e22\u0e34\u0e48\u0e07\u0e02\u0e36\u0e49\u0e19\u0e0a\u0e48\u0e27\u0e22\u0e41\u0e22\u0e01\u0e41\u0e22\u0e30\u0e40\u0e17\u0e28\u0e1a\u0e32\u0e25\u0e17\u0e35\u0e48\u0e1b\u0e23\u0e30\u0e2a\u0e1a\u0e27\u0e34\u0e01\u0e24\u0e15\u0e14\u0e49\u0e32\u0e19\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22 \u0e02\u0e32\u0e14\u0e41\u0e04\u0e25\u0e19\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22 \u0e41\u0e25\u0e30\u0e2d\u0e31\u0e15\u0e23\u0e32\u0e2b\u0e49\u0e2d\u0e07\u0e27\u0e48\u0e32\u0e07\u0e17\u0e35\u0e48\u0e1c\u0e48\u0e2d\u0e19\u0e04\u0e25\u0e32\u0e22\u0e44\u0e14\u0e49\u0e0a\u0e31\u0e14\u0e40\u0e08\u0e19\u0e22\u0e34\u0e48\u0e07\u0e02\u0e36\u0e49\u0e19",

    # Layer titles
    title_vacancy_rate   = "\u0e2a\u0e31\u0e14\u0e2a\u0e48\u0e27\u0e19\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22\u0e27\u0e48\u0e32\u0e07\u0e15\u0e48\u0e2d\u0e2a\u0e15\u0e47\u0e2d\u0e01\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22\u0e17\u0e31\u0e49\u0e07\u0e2b\u0e21\u0e14",
    title_avg_rent       = "\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e40\u0e2a\u0e19\u0e2d\u0e40\u0e09\u0e25\u0e35\u0e48\u0e22\u0e16\u0e31\u0e27\u0e07\u0e19\u0e49\u0e33\u0e2b\u0e19\u0e31\u0e01\u0e15\u0e48\u0e2d\u0e40\u0e17\u0e28\u0e1a\u0e32\u0e25",
    title_median_slider  = "\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e21\u0e31\u0e18\u0e22\u0e10\u0e32\u0e19\u0e02\u0e2d\u0e07\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22\u0e43\u0e2b\u0e49\u0e40\u0e0a\u0e48\u0e32\u0e15\u0e32\u0e21\u0e1b\u0e23\u0e30\u0e40\u0e20\u0e17\u0e2b\u0e49\u0e2d\u0e07",
    title_quote_gesamt   = "\u0e2a\u0e31\u0e14\u0e2a\u0e48\u0e27\u0e19\u0e2a\u0e15\u0e47\u0e2d\u0e01\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22\u0e17\u0e35\u0e48\u0e04\u0e23\u0e2d\u0e1a\u0e04\u0e25\u0e38\u0e21\u0e43\u0e19\u0e01\u0e32\u0e23\u0e27\u0e34\u0e40\u0e04\u0e23\u0e32\u0e30\u0e2b\u0e4c (\u0e15\u0e31\u0e27\u0e0a\u0e35\u0e49\u0e27\u0e31\u0e14\u0e04\u0e27\u0e32\u0e21\u0e44\u0e21\u0e48\u0e41\u0e19\u0e48\u0e19\u0e2d\u0e19)",
    title_share_avg_rent = "\u0e2a\u0e31\u0e14\u0e2a\u0e48\u0e27\u0e19\u0e04\u0e23\u0e31\u0e27\u0e40\u0e23\u0e37\u0e2d\u0e19\u0e17\u0e35\u0e48\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e08\u0e48\u0e32\u0e22\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e40\u0e2a\u0e19\u0e2d\u0e40\u0e09\u0e25\u0e35\u0e48\u0e22\u0e44\u0e14\u0e49",
    title_slider         = "\u0e04\u0e23\u0e31\u0e27\u0e40\u0e23\u0e37\u0e2d\u0e19\u0e01\u0e35\u0e48\u0e2b\u0e25\u0e31\u0e07\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e08\u0e48\u0e32\u0e22\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e44\u0e14\u0e49\u0e40\u0e17\u0e48\u0e32\u0e44\u0e2b\u0e23\u0e48?",
    title_bivariate_v1   = "\u0e41\u0e1c\u0e19\u0e17\u0e35\u0e48\u0e17\u0e27\u0e34\u0e20\u0e32\u0e04\u0e35: \u0e2b\u0e49\u0e2d\u0e07\u0e27\u0e48\u0e32\u0e07 \u00d7 \u0e04\u0e27\u0e32\u0e21\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e43\u0e19\u0e01\u0e32\u0e23\u0e08\u0e48\u0e32\u0e22 (3\u00d73)",
    title_bivariate_v2   = "\u0e41\u0e1c\u0e19\u0e17\u0e35\u0e48\u0e17\u0e27\u0e34\u0e20\u0e32\u0e04\u0e35\u0e02\u0e22\u0e32\u0e22 (4\u00d74)",

    # Control labels
    label_map_view    = "\u0e21\u0e38\u0e21\u0e21\u0e2d\u0e07\u0e41\u0e1c\u0e19\u0e17\u0e35\u0e48:",
    label_room_size   = "\u0e1b\u0e23\u0e30\u0e40\u0e20\u0e17\u0e2b\u0e49\u0e2d\u0e07:",
    label_analysis    = "\u0e01\u0e32\u0e23\u0e27\u0e34\u0e40\u0e04\u0e23\u0e32\u0e30\u0e2b\u0e4c:",
    label_rent        = "\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32 (CHF/\u0e40\u0e14\u0e37\u0e2d\u0e19)",
    label_share       = "\u0e2a\u0e31\u0e14\u0e2a\u0e48\u0e27\u0e19\u0e04\u0e23\u0e31\u0e27\u0e40\u0e23\u0e37\u0e2d\u0e19",
    label_dorling     = "\u0e41\u0e2a\u0e14\u0e07\u0e1e\u0e37\u0e49\u0e19\u0e17\u0e35\u0e48\u0e15\u0e32\u0e21\u0e2a\u0e31\u0e14\u0e2a\u0e48\u0e27\u0e19\u0e1b\u0e23\u0e30\u0e0a\u0e32\u0e01\u0e23",
    label_information = "\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25",
    label_rent_to_share = "\u0e08\u0e32\u0e01\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e2a\u0e39\u0e48\u0e04\u0e23\u0e31\u0e27\u0e40\u0e23\u0e37\u0e2d\u0e19",
    label_share_to_rent = "\u0e08\u0e32\u0e01\u0e04\u0e23\u0e31\u0e27\u0e40\u0e23\u0e37\u0e2d\u0e19\u0e2a\u0e39\u0e48\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32",

    # Placeholder
    click_placeholder = "\u0e04\u0e25\u0e34\u0e01\u0e17\u0e35\u0e48\u0e40\u0e17\u0e28\u0e1a\u0e32\u0e25\u0e40\u0e1e\u0e37\u0e48\u0e2d\u0e14\u0e39\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25\u0e40\u0e1e\u0e34\u0e48\u0e21\u0e40\u0e15\u0e34\u0e21",

    # Popup labels
    popup_municipality    = "\u0e40\u0e17\u0e28\u0e1a\u0e32\u0e25",
    popup_share_pct       = "\u0e2a\u0e31\u0e14\u0e2a\u0e48\u0e27\u0e19\u0e17\u0e35\u0e48\u0e08\u0e48\u0e32\u0e22\u0e44\u0e14\u0e49 (%)",
    popup_avg_rent        = "\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e40\u0e09\u0e25\u0e35\u0e48\u0e22 (CHF)",
    popup_vacancy_pct     = "\u0e2d\u0e31\u0e15\u0e23\u0e32\u0e2b\u0e49\u0e2d\u0e07\u0e27\u0e48\u0e32\u0e07 (%)",
    popup_gwr_total       = "\u0e17\u0e35\u0e48\u0e2d\u0e22\u0e39\u0e48\u0e2d\u0e32\u0e28\u0e31\u0e22 GWR",
    popup_inserate        = "\u0e1b\u0e23\u0e30\u0e01\u0e32\u0e28",
    popup_inseratequote   = "\u0e04\u0e27\u0e32\u0e21\u0e04\u0e23\u0e2d\u0e1a\u0e04\u0e25\u0e38\u0e21\u0e1b\u0e23\u0e30\u0e01\u0e32\u0e28",
    popup_class           = "\u0e23\u0e30\u0e14\u0e31\u0e1a",
    popup_vacancy_label   = "\u0e2b\u0e49\u0e2d\u0e07\u0e27\u0e48\u0e32\u0e07",
    popup_afford_label    = "\u0e04\u0e27\u0e32\u0e21\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e43\u0e19\u0e01\u0e32\u0e23\u0e08\u0e48\u0e32\u0e22",
    popup_median_rent     = "\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e21\u0e31\u0e18\u0e22\u0e10\u0e32\u0e19",
    popup_n_inserate      = "\u0e08\u0e33\u0e19\u0e27\u0e19\u0e1b\u0e23\u0e30\u0e01\u0e32\u0e28",
    popup_share_households = "\u0e2a\u0e31\u0e14\u0e2a\u0e48\u0e27\u0e19\u0e04\u0e23\u0e31\u0e27\u0e40\u0e23\u0e37\u0e2d\u0e19",
    popup_rent_chf        = "\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32 (CHF)",
    popup_data_coverage   = "\u0e04\u0e27\u0e32\u0e21\u0e04\u0e23\u0e2d\u0e1a\u0e04\u0e25\u0e38\u0e21\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25 (%)",

    # Legend
    legend_no_data    = "\u0e44\u0e21\u0e48\u0e21\u0e35\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25",
    legend_median_prefix = "\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e21\u0e31\u0e18\u0e22\u0e10\u0e32\u0e19",
    legend_share_at   = "\u0e2a\u0e31\u0e14\u0e2a\u0e48\u0e27\u0e19\u0e17\u0e35\u0e48",
    legend_rent_at    = "\u0e04\u0e48\u0e32\u0e40\u0e0a\u0e48\u0e32\u0e17\u0e35\u0e48",

    # Bivariate labels
    biv_low           = "\u0e15\u0e48\u0e33",
    biv_mid           = "\u0e01\u0e25\u0e32\u0e07",
    biv_high          = "\u0e2a\u0e39\u0e07",
    biv_mid_low       = "\u0e01\u0e25\u0e32\u0e07-\u0e15\u0e48\u0e33",
    biv_mid_high      = "\u0e01\u0e25\u0e32\u0e07-\u0e2a\u0e39\u0e07",

    # Legend axis labels
    legend_vacancy       = "\u0e2b\u0e49\u0e2d\u0e07\u0e27\u0e48\u0e32\u0e07",
    legend_affordability = "\u0e04\u0e27\u0e32\u0e21\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e43\u0e19\u0e01\u0e32\u0e23\u0e08\u0e48\u0e32\u0e22",
    legend_unaffordability = "\u0e04\u0e27\u0e32\u0e21\u0e44\u0e21\u0e48\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e43\u0e19\u0e01\u0e32\u0e23\u0e08\u0e48\u0e32\u0e22"
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
