# ============================================================
# preprocess_relief.R
# Relief-Raster einmalig nach WGS84 reprojizieren und speichern.
# Muss nur einmal ausgeführt werden, nicht bei jedem App-Start.
# ============================================================

library(raster)

relief_raw <- raster("../../data/02-relief-georef-clipped-resampled.tif")
cat("Original CRS:", as.character(crs(relief_raw)), "\n")
cat("Reprojecting to WGS84...\n")

relief_wgs84 <- projectRaster(relief_raw, crs = sp::CRS(SRS_string = "EPSG:4326"),
                                method = "bilinear")

out_path <- "../../data/new_data/relief_wgs84.tif"
writeRaster(relief_wgs84, out_path, format = "GTiff", overwrite = TRUE)
cat("Saved to:", out_path, "\n")
cat("Dimensions:", dim(relief_wgs84), "\n")
