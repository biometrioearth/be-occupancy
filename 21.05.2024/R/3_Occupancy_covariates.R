library("here")
library("terra")
library("sp")
library("lubridate")
library("dplyr")

occ_rast_files <- list.files(here("Data", "Covariates", "Monthly", "resampled_images_occ"),
                             pattern = "\\.tif$",
                             full.names = TRUE)


occ_rast_list <- list()
counter <- 0
for (r in occ_rast_files){
  counter <- counter+1
  rast_obj <- rast(r)
  occ_rast_list[[counter]] <- rast_obj
}
# Combine the aligned rasters into a single SpatRaster object
occ_rast <- rast(occ_rast_list)

# Create sampling points to extract
samp_coords <- video_df[!duplicated(video_df[,c("latitude", "longitude","device")]),
                        c("latitude", "longitude", "device")] ##devices
samp_coords <-as.data.frame(samp_coords)


sampling_points <- vect(samp_coords,
                        geom=c("latitude", "longitude"),
                        crs= "+proj=longlat +datum=WGS84")
sampling_points<- project(sampling_points, "+init=epsg:32615")

# Extract data from rasters to points
extracted_covs <- extract(occ_rast, sampling_points)
names(extracted_covs)
names(extracted_covs) <- c("ID","ccibiomass","_firms", "aspect", "bare_sparse_veg", 
                           "built_up", "canopyheight", "croplnd", "dis2protected", "dist2roads",
                           "dist_water", "grassland", "mangroves", "connectivity", "shrublnd",
                            "treecover", "wetland","slope","tree_cover_loss_prop")

extracted_covs
sampling_points$ID <- 1:19
sampling_points <- merge(sampling_points, extracted_covs, by = "ID")

head(sampling_points)

writeVector(sampling_points, "sampling_points.shp", overwrite=TRUE)
