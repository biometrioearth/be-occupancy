library("here")
library("spOccupancy")
library("configr")

source(here("R", "occ_functions.R"))

##========================================================================================
## Set paths and parameters
##========================================================================================
file_name <- here("data","In_situ_data", "230620_NHUM_MEX_NC_detections_image_video 3.xlsx")
ref_raster_path <- here("Data", "Occupa")

threshold <- 0.9
data_type <- "video"
period <- "monthly"  # or "biweekly", "monthly"

##========================================================================================
## Create detections table
##========================================================================================

video_monthly <- pre_process_data(file_name, threshold, period)
#dim(video_monthly)
output_file  <- here("Data", "Processed", paste0(data_type, "_", period, "_detection_array.RDS"))

saveRDS(video_monthly, output_file)

##========================================================================================
## Create Occupancy covariates
##========================================================================================

sampling_points <- create_occupancy_covariates(file_name, threshold, period=period) 
# writeVector(sampling_points, here("Data", "Processed", paste0(data_type, "_", period, ".shp")))

##========================================================================================
## Create Detectability covariates
##========================================================================================

## Preprocessing the images
det_rasters <- load_rasters(period, ref_raster_path = paste0(ref_raster_path, "/dist_roads_matched.tif"))

## Load rasters with terra.
det_brick <- rast(det_rasters)

##select monthly sampling sites for audio
sampling_sites <- load_sampling_sites(data_type = data_type, period=period)

# Create raster for fapar
rast_fapar <- det_brick[[1:6]]
plot(rast_fapar)
# Create raster for lai
rast_lai <- det_brick[[7:12]]
plot(rast_lai)
# Create raster for pre
rast_pre <- det_brick[[13:18]]
plot(rast_pre)
# Create raster for tem
rast_tem <- det_brick[[19:24]]
plot(rast_tem)

# Extract by month.
fapar_ext <- data.matrix(extract(rast_fapar, sampling_sites))
fapar_ext <- fapar_ext[,-1]

lai_ext <- data.matrix(extract(rast_lai, sampling_sites))
lai_ext <- lai_ext[,-1]
pre_ext <- data.matrix(extract(rast_pre, sampling_sites))
pre_ext <- pre_ext[,-1]
tem_ext <- data.matrix(extract(rast_tem, sampling_sites))
tem_ext <- tem_ext[,-1]

months <- matrix(0,140,6)

#data_matrix <- create_matrix(sampling_sites, 6)
months[,1] <- 10  # October
months[,2] <- 11  # November
months[,3] <- 12  # December
months[,4] <- 1   # January
months[,5] <- 2   # February
months[,6] <- 3   # March

# Save as list
det_covs <- list(fapar=fapar_ext,
                 lai=lai_ext,
                 pre=pre_ext,
                 tem=tem_ext,
                 month=months)

saveRDS(det_covs, here("data", "Processed", paste0("period", "_det_covs.RDS")))

##========================================================================================
## Fit occupancy model
##========================================================================================

# Load data
y <- readRDS(here("Data","Processed","vid_monthly_detection_array.RDS")) # [50species, 140 sampling points (devices), 6 months]

sampling_sites <- vect(here("Data","Processed","sampling_points_video_monthly.shp"))
occ.cov <- sampling_sites[, c( "bare_spar0", "biomass_m0", "builtup_m0", "canopyhei0", 
                               "cropland_0", "dem_match0", "dist_road0", "fire_coun0", "fire_dist0", "grassland0", 
                               "herbaceou0", "mangroves0", "permeabil0", "protected0", "shrubland0", "slope_mat0", 
                               "tree_cove0", "tree_cove1", "water_dis0", "waterbodi0")]

occ.cov <- data.matrix(as.data.frame(occ.cov))
det.covs <- readRDS(here("Data","Processed","period_det_covs.RDS")) #must be data matrix
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
occ.formula <- ~ scale(water_dis0) + scale(permeabil0) + scale(dist_road0) + scale(canopyhei0)

det.formula <- ~ I(scale(month)) + scale(pre) + scale(tem)

i=1 # species
print(i)
y_i <- y[i,,]

occ_table <- list(y = y_i, occ.covs = occ.cov, det.covs = det.covs, coords = coords)

out <- spPGOcc(occ.formula = occ.formula,
               det.formula = det.formula,
               data = occ_table, n.batch = 50, batch.length = 20,
               accept.rate = 0.4, cov.model = "exponential", 
               NNGP = TRUE, n.neighbors = 1, n.burn = 500, 
               n.thin = 5, n.chains = 3, verbose = FALSE, k.fold = 10)

####

# Predict on whole area
occ_rast_files <- list.files(here("Data", "Occupa"),
                             pattern = "\\.tif$",
                             full.names = TRUE)

print(occ_rast_files)

files_to_include <- c("dist_roads_matched.tif", "water_dis_matched.tif", "permeability.tif", "canopyheight_matched.tif")
occ_rast_files <- occ_rast_files[basename(occ_rast_files) %in% files_to_include]

occ_rast_list <- list()
counter <- 0
for (r in occ_rast_files){
  counter <- counter+1
  occ_rast_list[[counter]] <- rast(r)
}

occ_rast <- rast(occ_rast_list)

occ_df <- as.data.frame(occ_rast, cell = TRUE)
#names(occ_df) <- c("cell","bare_spar_prop", "biomass", "builtup_prop", "canopyheight", 
#                   "cropland_prop", "DEM", "dist_road", "fire_count", "fire_dist", "grassland_prop", 
#                   "herbaceous_prop", "mangroves_prop", "func_connectivity", "protected_dist", "shrubland_prop",  
#                   "slope","tree_cover_loss", "tree_cover_prop", "water_dist", "waterbod_prop")
occ_df <- occ_df[complete.cases(occ_df),]

occ_df_final <- as.data.frame(scale(occ_df))
occ_df_final$cell <- occ_df$cell

coords_raster <- xyFromCell(occ_rast, occ_df$cell)

out.pred <- predict(out, X.0=occ_df, coords.0=coords_raster)
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
