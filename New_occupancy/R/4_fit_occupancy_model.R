# Load data
y <- readRDS(here("Data","Processed","video_monthly_detection_array.RDS")) # [50species, 37 sampling points (devices), 6 months]

sampling_sites <- vect(here("Data","Processed","sampling_points_video_monthly.shp"))
occ.cov <- sampling_sites[, c( "bare_spar0", "biomass_m0", "builtup_m0", "canopyhei0", 
                              "cropland_0", "dem_match0", "dist_road0", "fire_coun0", "fire_dist0", "grassland0", 
                              "herbaceou0", "mangroves0", "permeabil0", "protected0", "shrubland0", "slope_mat0", 
                              "tree_cove0", "tree_cove1", "water_dis0", "waterbodi0")]

occ.cov <- data.matrix(as.data.frame(occ.cov))
det.covs <- readRDS(here("Data","Processed","monthly_det_covs.RDS")) 
coords <- geom(sampling_sites)[, c("x", "y")]
coords <- data.matrix(coords)

# Load prediction raster files
occ_rast_files <- list.files(here("Data", "Occupa"),
                             pattern = "\\.tif$",
                             full.names = TRUE)

occ_rast_list <- list()
counter <- 0
for (r in occ_rast_files){
  counter <- counter+1
  occ_rast_list[[counter]] <- rast(r)
}

# Define model formulas
occ.formula <- ~ scale(bare_spar0) + scale(biomass_m0) ++ scale(builtup_m0)+ I(scale(canopyhei0)^2)+ 
  scale(cropland_0) +scale(dem_match0) + scale(dist_road0) +scale(fire_coun0) + 
  scale(fire_dist0) +scale(grassland0) + scale(herbaceou0) +scale(mangroves0) +
  scale(permeabil0) +scale(protected0) + scale(shrubland0) + scale(slope_mat0) +
  scale(tree_cove0) + scale(tree_cove1) + scale(water_dis0) + scale(waterbodi0)

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
  
  occ.formula <- ~ scale(bare_spar0) + scale(biomass_m0) ++ scale(builtup_m0)+ I(scale(canopyhei0)^2)+ 
    scale(cropland_0) +scale(dem_match0) + scale(dist_road0) +scale(fire_coun0) + 
    scale(fire_dist0) +scale(grassland0) + scale(herbaceou0) +scale(mangroves0) +
    scale(permeabil0) +scale(protected0) + scale(shrubland0) + scale(slope_mat0) +
    scale(tree_cove0) + scale(tree_cove1) + scale(water_dis0) + scale(waterbodi0)
  
  
  det.formula <- ~ I(scale(month)^2) + scale(lai) + scale(pre) + scale(tem) + scale(fapar)
  
  out <- spPGOcc(occ.formula = occ.formula,
                 det.formula = det.formula,
                 data = occ_table, n.batch = 400, batch.length = 25,
                 accept.rate = 0.43, cov.model = "exponential", 
                 NNGP = TRUE, n.neighbors = 1, n.burn = 2000, 
                 n.thin = 4, n.chains = 3, verbose = FALSE, k.fold = 9)
  
  print("outmodel_done")
  
  # Predict on whole area
  occ_df <- as.data.frame(occ_rast, cell = TRUE)
  names(occ_df) <- c("cell","bare_spar_prop", "biomass", "builtup_prop", "canopyheight", 
                     "cropland_prop", "DEM", "dist_road", "fire_count", "fire_dist", "grassland_prop", 
                     "herbaceous_prop", "mangroves_prop", "func_connectivity", "protected_dist", "shrubland_prop",  
                     "slope","tree_cover_loss", "tree_cover_prop", "water_dist", "waterbod_prop")
  occ_df <- occ_df[complete.cases(occ_df),]
  
  bare_spar_prop <- (occ_df$bare_spar_prop - mean(occ_table$occ.covs[, 1])) / sd(occ_table$occ.covs[, 1])
  biomass <- (occ_df$biomass - mean(occ_table$occ.covs[, 2])) / sd(occ_table$occ.covs[, 2])
  builtup_prop <- (occ_df$builtup_prop - mean(occ_table$occ.covs[, 3])) / sd(occ_table$occ.covs[, 3])
  canopyheight <- (occ_df$canopyheight - mean(occ_table$occ.covs[, 4])) / sd(occ_table$occ.covs[, 4])
  cropland_prop <- (occ_df$cropland_prop - mean(occ_table$occ.covs[, 5])) / sd(occ_table$occ.covs[, 5])
  DEM <- (occ_df$DEM - mean(occ_table$occ.covs[, 6])) / sd(occ_table$occ.covs[, 6])
  dist_road <- (occ_df$dist_road - mean(occ_table$occ.covs[, 7])) / sd(occ_table$occ.covs[, 7])
  fire_count <- (occ_df$fire_count - mean(occ_table$occ.covs[, 8])) / sd(occ_table$occ.covs[, 8])
  fire_dist <- (occ_df$fire_dist - mean(occ_table$occ.covs[, 9])) / sd(occ_table$occ.covs[, 9])
  grassland_prop <- (occ_df$grassland_prop - mean(occ_table$occ.covs[, 10])) / sd(occ_table$occ.covs[, 10])
  herbaceous_prop <- (occ_df$herbaceous_prop - mean(occ_table$occ.covs[, 11])) / sd(occ_table$occ.covs[, 11])
  mangroves_prop <- (occ_df$mangroves_prop - mean(occ_table$occ.covs[, 12])) / sd(occ_table$occ.covs[, 12])
  func_connectivity <- (occ_df$func_connectivity - mean(occ_table$occ.covs[, 13])) / sd(occ_table$occ.covs[, 13])
  protected_dist <- (occ_df$protected_dist - mean(occ_table$occ.covs[, 14])) / sd(occ_table$occ.covs[, 14])
  shrubland_prop <- (occ_df$shrubland_prop - mean(occ_table$occ.covs[, 15])) / sd(occ_table$occ.covs[, 15])
  slope <- (occ_df$slope - mean(occ_table$occ.covs[, 16])) / sd(occ_table$occ.covs[, 16])
  tree_cover_loss <- (occ_df$tree_cover_loss - mean(occ_table$occ.covs[, 17])) / sd(occ_table$occ.covs[, 17])
  tree_cover_prop <- (occ_df$tree_cover_prop - mean(occ_table$occ.covs[, 18])) / sd(occ_table$occ.covs[, 18])
  water_dist <- (occ_df$water_dist - mean(occ_table$occ.covs[, 19])) / sd(occ_table$occ.covs[, 19])
  waterbod_prop <- (occ_df$waterbod_prop - mean(occ_table$occ.covs[, 20])) / sd(occ_table$occ.covs[, 20])
  
  coords_raster <- xyFromCell(occ_rast, occ_df$cell)
  
  
  X.0 <- cbind(bare_spar_prop, biomass, builtup_prop, canopyheight, 
               cropland_prop, DEM, dist_road, fire_count, fire_dist, 
               grassland_prop, herbaceous_prop, mangroves_prop, 
               func_connectivity, protected_dist, shrubland_prop, 
               slope, tree_cover_loss, tree_cover_prop, water_dist, waterbod_prop)
  
  out.pred <- predict(out, X.0=X.0, coords.0=coords_raster)
  mean.psi = apply(out.pred$psi.0.samples, 2, mean)
  sd.psi = apply(out.pred$psi.0.samples, 2, sd)
  print("prediction_done")
  
  
  # Write predicted occupancy to raster
  z <- rast(here::here("Data", "Occupancy_Species","mask.tif"))
  z[occ_df$cell] <- mean.psi
  #plot(z)  
  z[z > 1] <- NA
  # plot(z)  
  writeRaster(z, here("Data", "Occupancy_Species", filename = paste0("predicted_occupancy_species_MEAN_", i, ".tif")), overwrite = TRUE)
  
  
  # Write predicted occupancy to sd raster
  z <- rast(here::here("Data", "Covariates", "Monthly", "resampled_images_occ", "_dist2roads_cropped_cropped.tif"))
  z[occ_df$cell] <- sd.psi
  #plot(z)  
  z[z > 1] <- NA
  # plot(z)  
  writeRaster(z, here("Data", "Occupancy_Species", filename = paste0("predicted_occupancy_species_SD_", i, ".tif")), overwrite = TRUE)
  ## DEVIANCES
  deviance_i <- out$k.fold.deviance
  saveRDS(deviance_i, here("Data", "Deviance",paste0("deviance_", i, ".RDS")))
  
}


##species richness
raster_path <- here("Results", "Mean")
raster_files <- list.files(path = raster_path, pattern = "\\.tif$", full.names = TRUE)
rasters <- rast(raster_files)
raster_sum <- sum(rasters)
writeRaster(raster_sum, filename = here("Species_richness_Occupancy_Northern_cluster.tif"), overwrite=TRUE)
