# Load data
y <- readRDS("y_bioacoustic.RDS") # [225species, 8sampling points, 3 months]
sampling_sites <- vect("sampling_bioacoustics.shp")
occ.cov <- sampling_sites[, c("d2protec", "d2roads", "asterdem", "asterslope", "theight")]
occ.cov <- data.matrix(as.data.frame(occ.cov))
det.covs <- readRDS("bioacoustic_det_covs.RDS")
coords <- geom(sampling_sites)[, c("x", "y")]
coords <- data.matrix(coords)

species_names <- rownames(y)
species_names[is.na(species_names)] <- "Unknown_Species"

# Load prediction raster files
occ_rast_files <- list.files(here("spOccupancy","Data", "500m_resampled","Occu"),
                             pattern = "\\.tif$",
                             full.names = TRUE)


occ_rast_list <- list()
counter <- 0
for (r in occ_rast_files){
  counter <- counter+1
  occ_rast_list[[counter]] <- rast(r)
}

# Define model formulas
occ.formula <- ~ scale(d2roads) + scale(d2protec) + I(scale(asterdem)) + scale(asterslope) + scale(theight)
det.formula <- ~ I(scale(month)) + scale(lai) + scale(pre) + scale(tem)

# Prepare Output folders
folders <- c("Mean", "SD", "Deviance")
lapply(folders, function(x) if (!dir.exists(file.path("Results_bioacoustics", x))) dir.create(file.path("Results_bioacoustics", x), recursive = TRUE))

results_df <- data.frame(Species = character(), Deviance = numeric(), stringsAsFactors = FALSE)

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
  
  occ.formula <- ~ scale(d2roads) + scale(d2protec) + I(scale(asterdem)) + scale(asterslope) + scale(theight)
  det.formula <- ~ I(scale(month)) + scale(lai) + scale(pre) + scale(tem)
  # Run the model
  out <- spPGOcc(occ.formula = occ.formula,
                 det.formula = det.formula,
                 data = occ_table, n.batch = 400, batch.length = 25,
                 accept.rate = 0.43, cov.model = "exponential", 
                 NNGP = TRUE, n.neighbors = 1, n.burn = 2000, 
                 n.thin = 4, n.chains = 3, verbose = FALSE, k.fold = 8)
  
  print("outmodel_done")
  # Predict on whole area
  occ_df <- as.data.frame(occ_rast, cell = TRUE)
  names(occ_df) <- c("cell", "d2protec", "d2roads", "asterdem", "asterslope", "theight")
  occ_df <- occ_df[complete.cases(occ_df),]
  
  d2protec <- (occ_df$d2protec - mean(occ_table$occ.covs[, 1])) / sd(occ_table$occ.covs[, 1])
  d2roads <- (occ_df$d2roads - mean(occ_table$occ.covs[, 2])) / sd(occ_table$occ.covs[, 2])
  asterdem <- (occ_df$asterdem - mean(occ_table$occ.covs[, 3])) / sd(occ_table$occ.covs[, 3])
  asterslope <- (occ_df$asterslope - mean(occ_table$occ.covs[, 4])) / sd(occ_table$occ.covs[, 4])
  theight <- (occ_df$theight - mean(occ_table$occ.covs[, 5])) / sd(occ_table$occ.covs[, 5])
  
  
  
  
  coords_raster <- xyFromCell(occ_rast, occ_df$cell)
  
  X.0 <- cbind(d2roads, d2protec, asterdem, asterslope, asterslope, theight) 
  out.pred <- predict(out, X.0=X.0, coords.0=coords_raster)
  mean.psi = apply(out.pred$psi.0.samples, 2, mean)
  sd.psi = apply(out.pred$psi.0.samples, 2, sd)
  print("prediction_done")
  
  # Write predicted occupancy to raster
  z <- rast(here::here("spOccupancy", "Data", "500m_resampled", "Occu", "_dist2roads_cropped_cropped.tif"))
  z[occ_df$cell] <- mean.psi
  #plot(z)  
  z[z > 1] <- NA
  # plot(z)  
  writeRaster(z, filename = file.path("Results_bioacoustics", "Mean", paste0("predicted_occupancy_species_MEAN_",  species_names[i], ".tif")), overwrite = TRUE)
  
  
  # Write predicted occupancy to sd raster
  z <- rast(here::here("spOccupancy", "Data", "500m_resampled", "Occu", "_dist2roads_cropped_cropped.tif"))
  z[occ_df$cell] <- sd.psi
  #plot(z)  
  z[z > 1] <- NA
  # plot(z)  
  writeRaster(z, filename = file.path("Results_bioacoustics", "SD", paste0("predicted_occupancy_species_SD_",  species_names[i], ".tif")), overwrite = TRUE)
  ## DEVIANCES
  deviance_i <- out$k.fold.deviance
  saveRDS(deviance_i, file.path("Results_bioacoustics", "Deviance", paste0("deviance_",  species_names[i], ".RDS")))
  
  # save all Deviance values a single csv file
  # Append results to data frame
  results_df <- rbind(results_df, data.frame(Species = species_names[i], Deviance = deviance_i))
}

#export csv for deviance
write.csv(results_df, file.path("Results_bioacoustics", "Deviance", "species_deviances.csv"), row.names = FALSE)


##species richness
raster_path <- here("Results_bioacoustics", "Mean")
raster_files <- list.files(path = raster_path, pattern = "\\.tif$", full.names = TRUE)
rasters <- rast(raster_files)
raster_sum <- sum(rasters)
writeRaster(raster_sum, filename = here("Species_richness_BIoacoustics_Northern_cluster.tif"), overwrite=TRUE)
