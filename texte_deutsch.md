# Texte Deutsch – GeoViz Shiny App

## Website-Titel

**Neu:** Wohnungsknappheit in der Schweiz (2025)

---

## Layer-Info-Texte

### Layer 1: `vacancy_rate` – Leerstandsziffer

- **Kurzname:** Leerstandsziffer
- **Untertitel:** Anteil leerstehender Wohnungen am Gesamtbestand
- **Info-Text:** Die Leerstandsziffer zeigt den Anteil leerstehender Wohnungen am Gesamtbestand einer Gemeinde. Gemäss dem BWO gilt eine Leerstandsziffer unter 1 % als Wohnungsnot und zwischen 1 und 1,5 % als Wohnungsmangel (vgl. SVIT, 2023; NZZ, 2023). Die divergierende Farbskala bildet diese Schwellenwerte ab. Datenquelle: BFS STAT-TAB, 2025.

---

### Layer 2: `avg_rent` – Durchschnittliche Angebotsmiete

- **Kurzname:** Durchschn. Angebotsmiete
- **Untertitel:** Gewichtete durchschnittliche Angebotsmiete pro Gemeinde
- **Info-Text:** Diese Karte zeigt die durchschnittliche Angebotsmiete pro Gemeinde. Die Berechnung erfolgt auf Basis der Medianpreise aller Zimmerkategorien (Layer «Medianpreis pro Zimmerkategorie»), gewichtet nach dem Wohnungsbestand gemäss dem Gebäude- und Wohnungsregister (GWR). Datenquellen: Lookmove / Watson, 2025.

---

### Layer 3: `median_slider` – Medianpreis pro Zimmerkategorie (Pro)

- **Kurzname:** Medianpreis pro Zimmerkategorie
- **Untertitel:** Medianpreis der inserierten Mietwohnungen nach Zimmerkategorie
- **Info-Text:** Zeigt den Medianpreis der inserierten Mietwohnungen pro Zimmerkategorie und Gemeinde. Die Ausgangsdaten auf Postleitzahlebene wurden proportional auf Gemeindeebene gewichtet und modelliert. Diese Daten bilden die Grundlage für die Berechnung der durchschnittlichen Angebotsmiete. Datenquelle: Lookmove, 2025.

---

### Layer 4: `quote_gesamt` – Inseratequote (Pro)

- **Kurzname:** Inseratequote (Unsicherheit)
- **Untertitel:** Anteil des erfassten Wohnungsbestandes (Unsicherheitsmass)
- **Info-Text:** Für die Berechnung der Angebotsmieten wurden Inserate von Lookmove verwendet. Diese stellen nur eine Stichprobe des tatsächlichen Wohnungsmarktes dar und unterliegen daher Ungenauigkeiten. Diese Karte visualisiert die Datenabdeckung: Sie zeigt, wie viel Prozent des Gesamtwohnungsbestandes einer Gemeinde als Inserat erfasst sind. Tiefe Werte bedeuten eine geringere Datenbasis und höhere Unsicherheit. Datenquellen: Lookmove / GWR, 2025.

---

### Layer 5: `share_avg_rent` – Leistbarkeit der Angebotsmiete

- **Kurzname:** Leistbarkeit (% Haushalte)
- **Untertitel:** Anteil der Haushalte, die sich die durchschnittliche Angebotsmiete leisten können
- **Info-Text:** Dieser Layer verbindet die durchschnittliche Angebotsmiete (Lookmove, 2025) mit Daten zur Leistbarkeit von Mietpreisen (Nachfragemonitor, 2023). Er zeigt, wie viel Prozent der Haushalte sich eine durchschnittliche Wohnung auf dem Markt ihrer Gemeinde leisten könnten. Ein Mietpreis gilt als leistbar, wenn er nicht mehr als 30 % des verfügbaren Nettoeinkommens beträgt. Datenquellen: Lookmove / Nachfragemonitor Mietwohnungen, 2023/2025.

---

### Layer 6: `slider` – Mietpreis-Analyse (Pro)

- **Kurzname:** Mietpreis-Analyse
- **Untertitel:** Wieviele Haushalte können sich wieviel Miete leisten?
- **Option 1:** Von Miete zu Haushalten
- **Option 2:** Von Anteil Haushalten zu Miete
- **Info-Text:** Interaktive Analyse: Einen Mietpreis (CHF) eingeben und sehen, welcher Anteil der Haushalte sich diesen leisten kann – oder umgekehrt einen Haushaltsanteil wählen und die entsprechende Miete ermitteln. Angezeigte Werte sind Bruttomieten (inkl. Nebenkosten). Datenquelle: Nachfragemonitor, 2023

---

### Layer 7: `bivariate_v1` – Leerstand x Leistbarkeit 3x3

- **Kurzname:** Leerstand x Leistbarkeit
- **Untertitel:** Bivariate Darstellung: Leerstand x Leistbarkeit (3x3)
- **Info-Text:** Diese Karte kombiniert die Ergebnisse der vorangehenden Layer: die Leerstandsziffer (Verfügbarkeit) und die Leistbarkeit der durchschnittlichen Angebotsmiete. Die Leerstandsziffer wird anhand fester Schwellenwerte in drei Klassen eingeteilt (0–0,5 %, 0,5–2 %, über 2 %). Die Leistbarkeit wird mittels Quantilen klassiert, sodass jede der drei Klassen gleich viele Gemeinden enthält. Die Kombination ergibt eine 3x3-Matrix: Dunkle Farbtöne kennzeichnen Gemeinden mit tiefem Leerstand und geringer Leistbarkeit – dort ist der Wohnungsmarkt besonders angespannt. Mit der Option «Fläche proportional zur Bevölkerung» werden die Gemeinden als Dorling-Kartogramm dargestellt, um die demografische Relevanz sichtbar zu machen.

---

### Layer 8: `bivariate_v2` – Leerstand x Leistbarkeit 4x4 (Pro)

- **Kurzname:** Leerstand x Leistbarkeit (4x4)
- **Untertitel:** Erweiterte bivariate Darstellung (4x4)
- **Info-Text:** Erweiterte bivariate Darstellung mit einer 4x4-Klassierung für eine differenziertere Betrachtung von Leerstand und Leistbarkeit. Die Leerstandsziffer wird in vier Klassen eingeteilt (0–0,5 %, 0,5–1 %, 1–1,5 %, über 1,5 %), die Leistbarkeit ebenfalls mittels Quantilen in vier Klassen. Die feinere Abstufung ermöglicht es, detailierter zwischen Gemeinden mit Wohnungsnot, bzw. mangel und entspannten Leerstandsziffern zu unterscheiden.

---

## UI-Texte & Labels

### Tab-Namen

| Key | Text |
|-----|------|
| `tab_availability` | Verfügbarkeit |
| `tab_affordability` | Leistbarkeit |
| `tab_result` | Ergebnis |

### Steuerungselemente

| Key | Text |
|-----|------|
| `toggle_pro` | Pro |
| `toggle_easy` | Easy |
| `label_room_size` | Zimmerkategorie: |
| `label_analysis` | Analyse: |
| `label_rent` | Miete (CHF/Monat) |
| `label_share` | Anteil Haushalte |
| `label_dorling` | Fläche proportional zur Bevölkerung anzeigen |
| `label_information` | Information |
| `label_rent_to_share` | Von Miete zu Haushalten |
| `label_share_to_rent` | Von Haushalten zu Miete |

### Platzhalter

| Key | Text |
|-----|------|
| `click_placeholder` | Klicke auf eine Gemeinde um mehr zu erfahren |

---

## Popup-Texte (Klick auf Gemeinde)

| Key | Text |
|-----|------|
| `popup_municipality` | Gemeinde |
| `popup_share_pct` | Anteil leistbar (%) |
| `popup_avg_rent` | Durchschn. Miete (CHF) |
| `popup_vacancy_pct` | Leerstandsziffer (%) |
| `popup_gwr_total` | GWR-Wohnungen |
| `popup_inserate` | Inserate |
| `popup_inseratequote` | Inseratequote |
| `popup_class` | Klasse |
| `popup_vacancy_label` | Leerstand |
| `popup_afford_label` | Leistbarkeit |
| `popup_median_rent` | Medianmiete |
| `popup_n_inserate` | Anzahl Inserate |
| `popup_share_households` | Anteil Haushalte |
| `popup_rent_chf` | Miete (CHF) |
| `popup_data_coverage` | Datenabdeckung (%) |

---

## Legenden-Texte

| Key | Text |
|-----|------|
| `legend_no_data` | Keine Daten |
| `legend_median_prefix` | Medianmiete |
| `legend_share_at` | Anteil bei |
| `legend_rent_at` | Miete bei |
| `legend_vacancy` | Leerstand |
| `legend_affordability` | Leistbarkeit |
| `legend_unaffordability` | Unleistbarkeit |

### Bivariate Klassen-Labels

| Key | Text |
|-----|------|
| `biv_low` | tief |
| `biv_mid` | mittel |
| `biv_high` | hoch |
| `biv_mid_low` | mittel-tief |
| `biv_mid_high` | mittel-hoch |
