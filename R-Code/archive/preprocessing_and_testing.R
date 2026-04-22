library(shiny)
library(tmap)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. DATEN LADEN
gwr <- readRDS("data/new_data/gwr_auswertung.rds")

municipalities <- st_read("data/new_data/merged_gemeinden_ch.gpkg", quiet = TRUE) |>
  rename(municipality = gemeinde_name, rent = miete_vorgabe, share = anteil_haushalte) |>
  st_transform(4326)

# 2. WIDE FORMAT ERSTELLEN
fixed <- municipalities %>%
  group_by(bfs_nr) %>%
  slice_head(n = 1) %>%
  ungroup()

geom_df <- fixed %>% select(bfs_nr, geom)
fixed_nogeom <- fixed %>% st_set_geometry(NULL) %>% select(-rent, -share)

wide_shares <- municipalities %>%
  st_set_geometry(NULL) %>%
  select(bfs_nr, rent, share) %>%
  pivot_wider(names_from = rent, values_from = share, names_prefix = "share_") 

# 3. JOINS
municipalities_final <- fixed_nogeom %>%
  left_join(wide_shares, by = "bfs_nr") %>%
  left_join(gwr, by = c("bfs_nr" = "mun_id")) %>%
  left_join(geom_df, by = "bfs_nr") %>%
  st_as_sf(sf_column_name = "geom")

# 4. HAUPTANALYSE & BEREINIGUNG
# Hier führen wir alle Schritte logisch nacheinander aus
municipalities_analysis <- municipalities_final %>%
  # Inserate-NAs mit 0 ersetzen
  mutate(across(starts_with("n_total_"), ~replace_na(.x, 0))) %>%
  
  # Basis-Metriken berechnen
  mutate(
    sum_gwr = gwr_n_1Zi + gwr_n_2Zi + gwr_n_3Zi + gwr_n_4Zi + gwr_n_5Zi + gwr_n_6plusZi,
    sum_angebote = n_total_1.5zi + n_total_2.5zi + n_total_3.5zi + n_total_4.5zi + n_total_5.5zi + n_total_6zi_plus,
    quote_gesamt = sum_angebote / sum_gwr
  ) %>%
  
  # Share-Nullen zu NA (Fehlende Datengrundlage erkennen)
  mutate(across(starts_with("share_"), ~if_else(share_500 == 0, NA_real_, .x))) %>%
  
  rowwise() %>%
  mutate(
    # Gewichtete Miete berechnen
    weighted_rent_sum = sum(
      median_1.5zi * gwr_pct_1Zi, median_2.5zi * gwr_pct_2Zi,
      median_3.5zi * gwr_pct_3Zi, median_4.5zi * gwr_pct_4Zi,
      median_5.5zi * gwr_pct_5Zi, median_6zi_plus * gwr_pct_6plusZi,
      na.rm = TRUE
    ),
    valid_weights = sum(
      if_else(is.na(median_1.5zi), 0, gwr_pct_1Zi),
      if_else(is.na(median_2.5zi), 0, gwr_pct_2Zi),
      if_else(is.na(median_3.5zi), 0, gwr_pct_3Zi),
      if_else(is.na(median_4.5zi), 0, gwr_pct_4Zi),
      if_else(is.na(median_5.5zi), 0, gwr_pct_5Zi),
      if_else(is.na(median_6zi_plus), 0, gwr_pct_6plusZi),
      na.rm = TRUE
    ),
    
    # THRESHOLDS ANWENDEN
    avg_rent_weighted = if_else(
      valid_weights >= 50 & quote_gesamt >= 0.02,
      weighted_rent_sum / valid_weights,
      NA_real_
    ),
    
    # INTERPOLATION (mit Fehlerabfang für approx)
    Share_avg_rent_weighted = {
      y_values <- c(share_500, share_1000, share_1500, share_2000, share_2500, 
                    share_3000, share_3500, share_4000, share_4500, share_5000, 
                    share_5500, share_6000, share_6500)
      
      if (!is.na(avg_rent_weighted) && sum(!is.na(y_values)) >= 2) {
        approx(x = seq(500, 6500, by = 500), y = y_values, xout = avg_rent_weighted, rule = 2)$y
      } else {
        NA_real_
      }
    }
  ) %>%
  ungroup()

# 5. KONTROLLE
cat("Gemeinden total:", nrow(municipalities_analysis), "\n")
cat("Davon mit gültiger Durchschnittsmiete:", sum(!is.na(municipalities_analysis$avg_rent_weighted)), "\n")

# 6. PLOT (Optional zur Visualisierung der Quoten)
plot_data <- municipalities_analysis %>%
  st_drop_geometry() %>%
  mutate(
    quote_1Zi = if_else(gwr_n_1Zi > 0, n_total_1.5zi / gwr_n_1Zi, NA_real_),
    quote_2Zi = if_else(gwr_n_2Zi > 0, n_total_2.5zi / gwr_n_2Zi, NA_real_),
    quote_3Zi = if_else(gwr_n_3Zi > 0, n_total_3.5zi / gwr_n_3Zi, NA_real_),
    quote_4Zi = if_else(gwr_n_4Zi > 0, n_total_4.5zi / gwr_n_4Zi, NA_real_),
    quote_5Zi = if_else(gwr_n_5Zi > 0, n_total_5.5zi / gwr_n_5Zi, NA_real_),
    quote_6plusZi = if_else(gwr_n_6plusZi > 0, n_total_6zi_plus / gwr_n_6plusZi, NA_real_)
  ) %>%
  select(municipality, starts_with("quote_"), -quote_gesamt) %>%
  pivot_longer(cols = starts_with("quote_"), names_to = "Kategorie", values_to = "Quote") %>%
  filter(!is.na(Quote))

y_limit <- quantile(plot_data$Quote, 0.95, na.rm = TRUE)

ggplot(plot_data, aes(x = Kategorie, y = Quote, fill = Kategorie)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_cartesian(ylim = c(0, y_limit)) +
  theme_minimal() +
  labs(title = "Observations-Quoten (95% Ausschnitt)")


summary(municipalities_analysis$avg_rent_weighted)

summary(municipalities_analysis$Share_avg_rent_weighted)

saveRDS(municipalities_analysis, "data/new_data/municipalities_analysis.rds")


mapview::mapview(municipalities_analysis, zcol = "avg_rent_weighted", legend = TRUE)
mapview::mapview(municipalities_analysis, zcol = "quote_gesamt", legend = TRUE)
