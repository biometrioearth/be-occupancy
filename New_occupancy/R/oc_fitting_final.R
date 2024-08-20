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
period <- "biweekly"  # or "biweekly", "monthly"

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
#det_rasters <- load_rasters(period, ref_raster_path = paste0(ref_raster_path, "/dist_roads_matched.tif"))
det_rast_files <- list.files(here("Data", "Detect"),
                             pattern = "\\.tif$",
                             full.names = TRUE)
det_rast_list <- list()
counter <- 0
for (r in det_rast_files){
  counter <- counter+1
  rast_obj <- rast(r)
  det_rast_list[[counter]] <- rast_obj
}


## Load rasters with terra.
det_brick <- rast(det_rast_list)

##select monthly sampling sites for audio
sampling_sites <- load_sampling_sites(data_type = data_type, period=period)
plot(det_brick)
# Create raster for fapar
#rast_fapar <- det_brick[[1:25]]
#plot(rast_fapar)
# Create raster for lai
#rast_lai <- det_brick[[7:12]]
#plot(rast_lai)
# Create raster for pre
#rast_pre <- det_brick[[1:25]]
#plot(rast_pre)
# Create raster for tem
rast_tem <- det_brick[[1:25]]
plot(rast_tem)

# Extract by week.
#pre_ext <- data.matrix(extract(rast_pre, sampling_sites))
#pre_ext <- pre_ext[,-1]
tem_ext <- data.matrix(extract(rast_tem, sampling_sites))
tem_ext <- tem_ext[,-1]

for (i in 1:25) {
  weeks[,i] <- i  
}

# Save as list
det_covs <- list(tem=tem_ext,
                 week=weeks)

saveRDS(det_covs, here("data", "Processed", paste0(period, "_det_covs.RDS")))

##========================================================================================
## Fit occupancy model
##========================================================================================

# Load data
y <- readRDS(here("Data","Processed","video_weekly_detection_array.RDS")) # [50species, 140 sampling points (devices), 6 months]

sampling_sites <- vect(here("Data","Processed","sampling_points_video_weekly.shp"))
occ.cov <- sampling_sites[, c("ccibiomas0", "canopyhei0" ,"dem_clip_0" , "dis2water0" ,"dis2prote0",
                              "fire_dist0", "mangroves0" ,"permeabil0",
                              "slope_cli0","grassland0", "herbaceou0")]
# "dis2roads0",

#occ.cov <- data.matrix(as.data.frame(occ.cov))
occ.cov <- as.data.frame(occ.cov)
occ.cov[is.na(occ.cov)] <- 0

occ.cov <- data.matrix(occ.cov)


det.covs <- readRDS(here("Data","Processed","period_det_covs.RDS")) #must be data matrix
for (i in seq_along(det.covs)) {
  det.covs[[i]][is.na(det.covs[[i]])] <- 0
}

coords <- geom(sampling_sites)[, c("x", "y")]
coords <- data.matrix(coords)

# Load prediction raster files
occ_rast_files <- list.files(here("Data", "Occupa"),
                             pattern = "\\.tif$",
                             full.names = TRUE)
files_to_exclude<- c("bare_sparse_vege_new_matched_resampled.tif", "builtup_matched_resampled.tif"
                     ,"tree_cover_prop.tif", "tree_cover_loss_proportion.tif",
                     "shrubland_matched.tif", "Fire_count_matched_resampled.tif","dis2roads_resampled.tif",
                     "cropland_matched_resampled.tif","waterbodies_matched_resampled.tif")
#dis2roads_resampled.tif 

occ_rast_files <- occ_rast_files[!basename(occ_rast_files) %in% files_to_exclude]


occ_rast_list <- list()
counter <- 0
for (r in occ_rast_files){
  counter <- counter+1
  occ_rast_list[[counter]] <- rast(r)
}

# Define model formulas

occ.formula <- ~ scale(ccibiomas0) + I(scale(canopyhei0)) + scale(dem_clip_0) + scale(dis2water0) + 
  scale(dis2prote0)  + scale(fire_dist0) + 
  scale(mangroves0) + scale(permeabil0) + scale(slope_cli0) + scale(grassland0) + scale(herbaceou0)

det.formula <- ~ I(scale(week)) + scale(tem)

## model fitting
#for (i in 1:dim(y)[1]) {
i=1 # species
print(i)
y_i <- y[i,,]

occ_table <- list(y = y_i, occ.covs = occ.cov, det.covs = det.covs, coords = coords)

out <- spPGOcc(occ.formula = occ.formula,
               det.formula = det.formula,
               data = occ_table, n.batch = 400, batch.length = 25,
               accept.rate = 0.43, cov.model = "exponential", 
               NNGP = TRUE, n.neighbors = 1, n.burn = 3000, 
               n.thin = 4, n.chains = 4, verbose = FALSE, k.fold = 9)
####ppc.out <- ppcOcc(out, fit.stat = 'freeman-tukey', group = 1)
##summary(ppc.out)


# Predict on whole area
occ_rast_files <- list.files(here("Data", "Occupa"),
                             pattern = "\\.tif$",
                             full.names = TRUE)

print(occ_rast_files)

files_to_exclude<- c("bare_sparse_vege_new_matched_resampled.tif", "builtup_matched_resampled.tif"
                     ,"tree_cover_prop.tif", "tree_cover_loss_proportion.tif",
                     "shrubland_matched.tif", "Fire_count_matched_resampled.tif",
                     "cropland_matched_resampled.tif","waterbodies_matched_resampled.tif","dis2roads_resampled.tif")
#, 

occ_rast_files <- occ_rast_files[!basename(occ_rast_files) %in% files_to_exclude]

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

mean.psi_final = apply(out.pred$psi.0.samples, 2, mean)
sd.psi = apply(out.pred$psi.0.samples, 2, sd)

print("prediction_done")
# Write predicted occupancy to raster
z <- rast(occ_rast_list[10]) #permeability
#plot(z)
z[!is.na(z)]<-NA
 
z[occ_df$cell] <- mean.psi_final
#z[z >= 0] <- 1
#z[z > 1] <- NA

plot(z)  
AOI <- vect(here( "northerncluster_campeche_projectsites.geojson"))
plot(AOI,add=T)
roads <- vect(here( "roads.geojson"))
plot(roads,add=T, col="red")
output_filename <- here("Data", "Occupancy_Species", paste0("predicted_occupancy_species_MEAN_", i, ".tif"))

writeRaster(z, output_filename, overwrite = TRUE)

#predicted sd raster
z <- rast(occ_rast_list[7])
z[occ_df$cell] <- sd.psi
plot(z) 

output_filename_SD <- here("Data", "Deviance", "Standard_deviation", paste0("predicted_occupancy_species_SD_", i, ".tif"))

writeRaster(z, output_filename_SD, overwrite = TRUE)

## DEVIANCES
deviance_i <- out$k.fold.deviance
saveRDS(deviance_i, here("Data", "Deviance", "Deviance_RDS", paste0("deviance_", i, ".RDS")))
#}

##species richness
raster_path <- here("Data", "Occupancy_Species")
raster_files <- list.files(path = raster_path, pattern = "\\.tif$", full.names = TRUE)
rasters <- rast(raster_files)
raster_sum <- sum(rasters)
writeRaster(raster_sum, filename = here("Species_richness_Occupancy_Northern_cluster.tif"), overwrite=TRUE)
