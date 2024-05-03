library("here")
library("terra")
library("lubridate")
library("spOccupancy")

# We need:

# Create a list with:
# 1
# hbef2015$y has the sp. detections

# 2
# hbef2015$occ.cov a matrix-array with the covariates for the occupancy model

# 3
# hbef2015$det.covs a list with the detectibility covariates

# 4 Matrix array with sites coordinates

# Load y data
y <- readRDS("y.RDS")

# Load occ.cov and sampling sites
sampling_sites <- vect("sampling_points.shp")
occ.cov <- sampling_sites[,c("d2roads", "d2protec", "asterdem", "asterslope", "theight")]
occ.cov <- data.matrix(as.data.frame(occ.cov))

# Load det.covs
det.covs <- readRDS("det_covs.RDS")

# Get the coordinates 
coords <- geom(sampling_sites)[,c("x","y")]
coords <- data.matrix(coords)

# Loop over each species
for (species_index in 1:nrow(y)) {
  # Choose a single species
  y_species <- y[species_index,,]  # Select species at current index
  
  occ_table <- list(y=y_species,
                    occ.covs=occ.cov,
                    det.covs=det.covs,
                    coords=coords)
  
  # Specify model formulas
  occ.formula <- ~ scale(d2roads) + scale(d2protec) + I(scale(asterdem)) + scale(asterslope) + scale(theight)
  det.formula <- ~ I(scale(month)) + scale(lai) + scale(pre) + scale(tem)
  
  # Run the model
  out <- spPGOcc(occ.formula = occ.formula,
                 det.formula = det.formula,
                 data = occ_table, n.batch = 400, batch.length = 25,
                 accept.rate = 0.43, cov.model = "exponential", 
                 NNGP = TRUE, n.neighbors = 1, n.burn = 2000, 
                 n.thin = 4, n.chains = 3, verbose = FALSE, k.fold = 9)
  
  ## Predict on whole area
  occ_rast_files <- list.files("C:/Users/AmaBoakye/OneDrive - biometrio.earth GmbH/Documents/GitHub/be-occupancy/spOccupancy/Data/Selected_files/500m_resampled/Occu",
                               pattern = "\\.tif$",
                               full.names = TRUE)
  
  occ_rast_list <- list()
  counter <- 0
  for (r in occ_rast_files){
    counter <- counter+1
    occ_rast_list[[counter]] <- rast(r)
  }
  
  # To multilayer raster
  occ_rast <- rast(occ_rast_list)
  
  # Occupancy data frame
  occ_df <- as.data.frame(occ_rast, cell=TRUE)
  names(occ_df)<- c("cell", "d2protec", "d2roads","asterdem","esaglc", "asterslope", "theight")
  occ_df <- occ_df[complete.cases(occ_df),]
  
  
  d2protec <- (occ_df$d2protec - mean(occ_table$occ.covs[, 1])) / sd(occ_table$occ.covs[, 1])
  d2roads <- (occ_df$d2roads - mean(occ_table$occ.covs[, 2])) / sd(occ_table$occ.covs[, 2])
  asterdem <- (occ_df$asterdem - mean(occ_table$occ.covs[, 3])) / sd(occ_table$occ.covs[, 3])
  asterslope <- (occ_df$asterslope - mean(occ_table$occ.covs[, 4])) / sd(occ_table$occ.covs[, 4])
  theight <- (occ_df$theight - mean(occ_table$occ.covs[, 5])) / sd(occ_table$occ.covs[, 5])
  
  coords <- xyFromCell(occ_rast, occ_df$cell)
  
  X.0 <- cbind(d2roads, d2protec, asterdem, asterslope, asterslope, theight) 
  out.pred <- predict(out, X.0=X.0, coords.0=coords)
  mean.psi = apply(out.pred$psi.0.samples, 2, mean)
  
  ###write raster
  alt.rs <- rast(here::here("spOccupancy", "Data", "500m_resampled", "Occu", "_dist2roads_cropped_cropped.tif"))
  
  z <- alt.rs
  z[occ_df$cell] <- mean.psi
  plot(z)
  z[z>1] <- NA
  plot(z)
  
  writeRaster(z, filename=paste0("predicted_occupancy_species_", species_index, ".tif"),overwrite=TRUE)
}