# Load data
y <- readRDS(here("Data","Processed","video_monthly_detection_array.RDS")) # [50species, 140 sampling points (devices), 6 months]

sampling_sites <- vect(here("Data","Processed","sampling_points_video_monthly.shp"))
occ.cov <- sampling_sites[, c( "canopy_hei",  "dist_roads", "dist_water",  "permabilit")] ## excluded covariates
occ.cov <- data.matrix(as.data.frame(occ.cov))
det.covs <- readRDS(here("Data","Processed","period_det_covs.RDS")) #must be data matrix
coords <- geom(sampling_sites)[, c("x", "y")]
coords <- data.matrix(coords)

# Load prediction raster files
occ_rast_files <- list.files(here("Data", "Occupa"),
                             pattern = "\\.tif$",
                             full.names = TRUE)
files_to_include <- c("canopyheight_matched.tif", "dist_roads_matched.tif",
                      "permeability.tif","water_dis_matched.tif")
occ_rast_files <- occ_rast_files[basename(occ_rast_files) %in% files_to_include]

occ_rast_list <- list()
counter <- 0
for (r in occ_rast_files){
  counter <- counter+1
  occ_rast_list[[counter]] <- rast(r)
}

occ_rast <- rast(occ_rast_list)
plot(occ_rast)


# Iterate over each species
for (i in 1:dim(y)[1]) {
  print(i)  # Print current species index for tracking progress
  
  # Prepare data for the current species
  y_i <- y[i,,]
  occ_table <- list(y = y_i, occ.covs = occ.cov, det.covs = det.covs, coords = coords)

  # Model formulas
  occ.formula <- ~ I(scale(canopy_hei)) + scale(dist_roads) + scale(dist_water) + scale(permabilit)
  det.formula <- ~ I(scale(month)) + scale(pre) + scale(tem)
  
  print(paste("Fitting model for species", i))
  out <- spPGOcc(occ.formula = occ.formula, det.formula = det.formula, data = occ_table, n.batch = 400,
                 batch.length = 25, accept.rate = 0.43, cov.model = "exponential", NNGP = TRUE, n.neighbors = 1,
                 n.burn = 2000, n.thin = 4, n.chains = 3, verbose = FALSE, k.fold = 9)
  
  print("Model fitting done")

  # Prediction setup
  occ_df <- as.data.frame(occ_rast, cell = TRUE)
  names(occ_df) <- c("cell", "canopy_hei", "dist_roads", "dist_water", "permabilit")
  occ_df <- occ_df[complete.cases(occ_df),]
  
  canopyheight <- (occ_df$canopy_hei - mean(occ_table$occ.covs[, 1])) / sd(occ_table$occ.covs[, 1])
  dist_road <- (occ_df$dist_roads - mean(occ_table$occ.covs[, 2])) / sd(occ_table$occ.covs[, 2])
  water_dist <- (occ_df$dist_water - mean(occ_table$occ.covs[, 3])) / sd(occ_table$occ.covs[, 3])
  func_connectivity <- (occ_df$permabilit - mean(occ_table$occ.covs[, 4])) / sd(occ_table$occ.covs[, 4])

  coords_raster <- xyFromCell(occ_rast, occ_df$cell)
  X.0 <- cbind(canopyheight, dist_road, water_dist, func_connectivity)
  
  out.pred <- predict(out, X.0=X.0, coords.0=coords_raster)
  mean.psi = apply(out.pred$psi.0.samples, 2, mean)
  sd.psi = apply(out.pred$psi.0.samples, 2, sd)
  print("Prediction done")
  plotRaster(mean.psi)
}
  
  # Write predicted occupancy to raster
  z <- rast(here::here("Data", "Occupancy_Species","mask.tif"))
  z[occ_df$cell] <- mean.psi

  z[z > 1] <- NA
    
  writeRaster(z, here("Data", "Occupancy_Species", filename = paste0("predicted_occupancy_species_MEAN_", i, ".tif")), overwrite = TRUE)
  
  
  # Write predicted occupancy to sd raster
  z <- rast(here::here("Data", "Occupancy_Species","mask.tif"))
  z[occ_df$cell] <- sd.psi
  #plot(z)  
  z[z > 1] <- NA
  # plot(z)  
  writeRaster(z, here("Data", "Occupancy_Species", filename = paste0("predicted_occupancy_species_SD_", i, ".tif")), overwrite = TRUE)
  ## DEVIANCES
  deviance_i <- out$k.fold.deviance
  saveRDS(deviance_i, here("Data", "Deviance",paste0("deviance_", i, ".RDS")))
  



##species richness
raster_path <- here("Results", "Mean")
raster_files <- list.files(path = raster_path, pattern = "\\.tif$", full.names = TRUE)
rasters <- rast(raster_files)
raster_sum <- sum(rasters)
writeRaster(raster_sum, filename = here("Species_richness_Occupancy_Northern_cluster.tif"), overwrite=TRUE)


##========================================================================================================================
for (i in 1:dim(y)[1]) {
  print(i)  # Print current species index for tracking progress
  
  # Prepare data for the current species
  y_i <- y[i,,]
  occ_table <- list(y = y_i, occ.covs = occ.cov, det.covs = det.covs, coords = coords)
  
  # Model formulas
  occ.formula <- ~ I(scale(canopy_hei)) + scale(dist_roads) + scale(dist_water) + scale(permabilit)
  det.formula <- ~ I(scale(month)) + scale(pre) + scale(tem)
  
  print(paste("Fitting model for species", i))
  out <- spPGOcc(occ.formula = occ.formula, det.formula = det.formula, data = occ_table, n.batch = 400,
                 batch.length = 25, accept.rate = 0.43, cov.model = "exponential", NNGP = TRUE, n.neighbors = 1,
                 n.burn = 2000, n.thin = 4, n.chains = 3, verbose = FALSE, k.fold = 9)
  
  print("Model fitting done")
  
  # Prediction setup
  occ_df <- as.data.frame(occ_rast, cell = TRUE)
  names(occ_df) <- c("cell", "canopy_hei", "dist_roads", "dist_water", "permabilit")
  occ_df <- occ_df[complete.cases(occ_df),]
  
  canopyheight <- (occ_df$canopy_hei - mean(occ_table$occ.covs[, 1])) / sd(occ_table$occ.covs[, 1])
  dist_road <- (occ_df$dist_roads - mean(occ_table$occ.covs[, 2])) / sd(occ_table$occ.covs[, 2])
  water_dist <- (occ_df$dist_water - mean(occ_table$occ.covs[, 3])) / sd(occ_table$occ.covs[, 3])
  func_connectivity <- (occ_df$permabilit - mean(occ_table$occ.covs[, 4])) / sd(occ_table$occ.covs[, 4])
  
  coords_raster <- xyFromCell(occ_rast, occ_df$cell)
  X.0 <- cbind(canopyheight, dist_road, water_dist, func_connectivity)
  
  out.pred <- predict(out, X.0=X.0, coords.0=coords_raster)
  mean.psi = apply(out.pred$psi.0.samples, 2, mean)
  sd.psi = apply(out.pred$psi.0.samples, 2, sd)
  print("Prediction done")
  plotRaster(mean.psi)
}
