library("here")
library("terra")
library("sp")
library("lubridate")
library("dplyr")

# We need:

# 2
# hbef2015$occ.cov
# it's a matrix-array with the covariates for the occupancy model
# [sampling sites, covariates]
# In this case [373, 1]

# Load occupancy rasters into a multilayer brick
occ_rast_files <- list.files(here:here("spOccupancy", "Data", "occ"),
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

# Create sampling points to extract
samp_coords <- cam_data[!duplicated(cam_data[,c("latitude", "longitude")]),
                        c("latitude", "longitude", "sampling_area")]

samp_coords_grouped <- group_by(samp_coords, sampling_area)
sampling_points <- summarise(samp_coords_grouped,
                             x = mean(longitude),
                             y = mean(latitude))

sampling_points <- vect(sampling_points,
                        geom=c("x", "y"),
                        crs="+proj=longlat +datum=WGS84")

# Extract data from rasters to points
extracted_covs <- extract(occ_rast, sampling_points)
names(extracted_covs)
names(extracted_covs) <- c("ID","d2roads", "d2protec", "asterdem", "asterslope", "esalc", "theight")
extracted_covs
sampling_points$ID <- 1:9
sampling_points <- merge(sampling_points, extracted_covs, by = "ID")

head(sampling_points)

writeVector(sampling_points, "sampling_points.shp", overwrite=TRUE)
