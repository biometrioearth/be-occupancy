# Load data
y <- readRDS(here("Data","Processed","audio_monthly_detection_array.RDS")) # [50species, 241 sampling points (devices), 6 months]
sampling_sites <- load_sampling_sites("audio", "monthly")

occ.cov <- sampling_sites[, c("ID", "device", "month", "bare_spar0", "biomass", "builtup", "canopyhei0", 
                              "cropland", "dem", "dist_roads", "fire_count", "fire_dist0", "grassland", 
                              "herbaceou0", "mangroves", "connectiv0", "dist_prot0", "shrubland", "slope", 
                              "tree_cove0", "tree_cover", "water_dist", "prop_wate0")]

occ.cov <- data.matrix(as.data.frame(occ.cov))
colnames(occ.cov)[colnames(occ.cov) == "_firms"] <- "firms"
det.covs <- readRDS("det_covs.RDS")
coords <- geom(sampling_sites)[, c("x", "y")]
coords <- data.matrix(coords)

# Load prediction raster files
occ_rast_files <- list.files(here("Data", "Covariates", "Monthly", "resampled_images_occ"),
                             pattern = "\\.tif$",
                             full.names = TRUE)

occ_rast_list <- list()
counter <- 0
for (r in occ_rast_files){
  counter <- counter+1
  occ_rast_list[[counter]] <- rast(r)
}

# Define model formulas
occ.formula <- ~ scale(dist2roads) + scale(dis2protec) + scale(ccibiomass) + scale(firms) + scale(aspect) +
  scale(bare_spars) + scale(built_up) + I(scale(canopyheig)^2)  + scale(croplnd) + scale(dist_water) +
  scale(grassland) + scale(mangroves) + scale(connectivi) + scale(shrublnd) + scale(treecover) +
  scale(wetland) + scale(slope) + scale(tree_cover)

det.formula <- ~ I(scale(month)^2) + scale(lai) + scale(pre) + scale(tem) + scale(fapar)

# Iterate over each species
for (i in 1:dim(y)[1]) {
  # i=1
  print(i)
  #attach names to output files
  # Subset data for the current species
  y_i <- y[i,,]
  
  print("species_done")
  # Prepare the data list for the model
  occ_table <- list(y = y_i, occ.covs = occ.cov, det.covs = det.covs, coords = coords)
  
  occ.formula <- ~ scale(dist2roads) + scale(dis2protec) + scale(ccibiomass) + scale(firms) + scale(aspect) +
    scale(bare_spars) + scale(built_up) + I(scale(canopyheig)^2)  + scale(croplnd) + scale(dist_water) +
    scale(grassland) + scale(mangroves) + scale(connectivi) + scale(shrublnd) + scale(treecover) +
    scale(wetland) + scale(slope) + scale(tree_cover)
  
  det.formula <- ~ I(scale(month)^2) + scale(lai) + scale(pre) + scale(tem) + scale(fapar)
  
  out <- spPGOcc(occ.formula = occ.formula,
                 det.formula = det.formula,
                 data = occ_table, n.batch = 400, batch.length = 25,
                 accept.rate = 0.43, cov.model = "exponential", 
                 NNGP = TRUE, n.neighbors = 1, n.burn = 2000, 
                 n.thin = 4, n.chains = 3, verbose = FALSE, k.fold = 9)
  
  print("outmodel_done")
  
  # Run the model
  
  # Predict on whole area
  occ_df <- as.data.frame(occ_rast, cell = TRUE)
  names(occ_df) <- c("cell", "ccibiomass","_firms", "aspect", "bare_spars", 
                     "built_up", "canopyheig", "croplnd", "dis2protec", "dist2roads",
                     "dist_water", "grassland", "mangroves", "connectivi", "shrublnd",
                     "treecover", "wetland","slope","tree_cover")
  occ_df <- occ_df[complete.cases(occ_df),]
  
  ccibiomass <- (occ_df$ccibiomass - mean(occ_table$occ.covs[, 1])) / sd(occ_table$occ.covs[, 1])
  firms <- (occ_df$firms - mean(occ_table$occ.covs[, 2])) / sd(occ_table$occ.covs[, 2])
  aspect <- (occ_df$aspect - mean(occ_table$occ.covs[, 3])) / sd(occ_table$occ.covs[, 3])
  bare_spars <- (occ_df$bare_spars - mean(occ_table$occ.covs[, 4])) / sd(occ_table$occ.covs[, 4])
  built_up <- (occ_df$built_up - mean(occ_table$occ.covs[, 5])) / sd(occ_table$occ.covs[, 5])
  canopyheig <- (occ_df$canopyheig - mean(occ_table$occ.covs[, 6])) / sd(occ_table$occ.covs[, 6])
  croplnd <- (occ_df$croplnd - mean(occ_table$occ.covs[, 7])) / sd(occ_table$occ.covs[, 7])
  dis2protec <- (occ_df$dis2protec - mean(occ_table$occ.covs[, 8])) / sd(occ_table$occ.covs[, 8])
  dist2roads <- (occ_df$dist2roads - mean(occ_table$occ.covs[, 9])) / sd(occ_table$occ.covs[, 9])
  dist_water <- (occ_df$dist_water - mean(occ_table$occ.covs[, 10])) / sd(occ_table$occ.covs[, 10])
  grassland <- (occ_df$grassland - mean(occ_table$occ.covs[, 11])) / sd(occ_table$occ.covs[, 11])
  mangroves <- (occ_df$mangroves - mean(occ_table$occ.covs[, 12])) / sd(occ_table$occ.covs[, 12])
  connectivi <- (occ_df$connectivi - mean(occ_table$occ.covs[, 13])) / sd(occ_table$occ.covs[, 13])
  shrublnd <- (occ_df$shrublnd - mean(occ_table$occ.covs[, 14])) / sd(occ_table$occ.covs[, 14])
  treecover <- (occ_df$treecover - mean(occ_table$occ.covs[, 15])) / sd(occ_table$occ.covs[, 15])
  wetland <- (occ_df$wetland - mean(occ_table$occ.covs[, 16])) / sd(occ_table$occ.covs[, 16])
  slope <- (occ_df$slope - mean(occ_table$occ.covs[, 17])) / sd(occ_table$occ.covs[, 17])
  tree_cover <- (occ_df$tree_cover - mean(occ_table$occ.covs[, 18])) / sd(occ_table$occ.covs[, 18])
  
  coords_raster <- xyFromCell(occ_rast, occ_df$cell)
  
  
  X.0 <- cbind(ccibiomass, firms, aspect, bare_spars, built_up, canopyheig,
               croplnd, dis2protec, dist2roads, dist_water, grassland, mangroves,
               connectivi, shrublnd, treecover, wetland, slope, tree_cover)
  
  out.pred <- predict(out, X.0=X.0, coords.0=coords_raster)
  mean.psi = apply(out.pred$psi.0.samples, 2, mean)
  sd.psi = apply(out.pred$psi.0.samples, 2, sd)
  print("prediction_done")
  
  
  # Write predicted occupancy to raster
  z <- rast(here::here("Data", "Covariates", "Monthly", "resampled_images_occ","_dist2roads_cropped_cropped.tif"))
  z[occ_df$cell] <- mean.psi
  #plot(z)  
  z[z > 1] <- NA
  # plot(z)  
  writeRaster(z, filename = paste0("predicted_occupancy_species_MEAN_", i, ".tif"), overwrite = TRUE)
  
  
  # Write predicted occupancy to sd raster
  z <- rast(here::here("Data", "Covariates", "Monthly", "resampled_images_occ", "_dist2roads_cropped_cropped.tif"))
  z[occ_df$cell] <- sd.psi
  #plot(z)  
  z[z > 1] <- NA
  # plot(z)  
  writeRaster(z, filename = paste0("predicted_occupancy_species_SD_", i, ".tif"), overwrite = TRUE)
  ## DEVIANCES
  deviance_i <- out$k.fold.deviance
  saveRDS(deviance_i, paste0("deviance_", i, ".RDS"))
  
}


##species richness
raster_path <- here("Results", "Mean")
raster_files <- list.files(path = raster_path, pattern = "\\.tif$", full.names = TRUE)
rasters <- rast(raster_files)
raster_sum <- sum(rasters)
writeRaster(raster_sum, filename = here("Species_richness_Occupancy_Northern_cluster.tif"), overwrite=TRUE)
