library("here")
library("terra")
library("sp")
library("readxl")
library("lubridate")
library("dplyr")
library("ggplot2")
library("tools")
library("sf")
library(spOccupancy)



source(here("R", "1_Pre_process.R"))
#source(here("R", "2_Occupancy_covariates.R"))
source("C:/Users/AmaBoakye/Work/be-occupancy/New_occupancy/R/2_Occupancy covariates.R", echo=TRUE)
source(here("R", "3_detectability_covariates.R"))
#source(here("R", "4_fit_occupancy_model.R"))


pre_process_data <- function(file_name, threshold, data_type) {
  dt_df <- read_data(file_name)
  dt_df <- filter_data(dt_df, threshold)  # Assuming 'score' or another column
  dt_df <- date_information(dt_df)
  dt_df_spatial <- create_spatial_data(dt_df, c("species", "longitude", "latitude"))
  create_detection_array(dt_df, "weekly", here("Data", "Processed", paste0(data_type, "_weekly_detection_array.RDS")))
  create_detection_array(dt_df, "biweekly", here("Data", "Processed", paste0(data_type, "_biweekly_detection_array.RDS")))
  create_detection_array(dt_df, "monthly", here("Data", "Processed", paste0(data_type, "_monthly_detection_array.RDS")))
}

##preprocess
pre_process_data("230620_NHUM_MEX_NC_detections_image_video 3.xlsx", 0.9, "video")
pre_process_data("230620_NHUM_MEX_NC_detections_audio.csv", 0.95, "audio")

##========================================================================================
##occupancy covariates
##========================================================================================

file_name <- "230620_NHUM_MEX_NC_detections_image_video 3.xlsx"
threshold <- 0.9
period <- "monthly"  # Change to "weekly" or "biweekly" or "monthly"
sampling_points <- create_occupancy_covariates(file_name, threshold, period)
head(sampling_points)

###audio
file_name <- "230620_NHUM_MEX_NC_detections_audio.csv"
threshold <- 0.95
period <- "monthly"  # Change to "weekly" or "biweekly" or "monthly"
sampling_points <- create_occupancy_covariates(file_name, threshold, period)
head(sampling_points)


##========================================================================================
##detectability covariates
##========================================================================================

##preprocessing the images
ref_raster_path <- "C:/Users/AmaBoakye/Work/be-occupancy/New_occupancy/Data/Occupa/dist_roads_matched.tif"
weekly_rasters <- load_rasters("weekly", ref_raster_path)
biweekly_rasters <- load_rasters("biweekly",ref_raster_path )
monthly_rasters <-load_rasters("monthly",ref_raster_path )
#extents <- lapply(det_rast_list, ext)
#resolutions <- lapply(det_rast_list, res)

## for weekly rasters
det_rast <- rast(monthly_rasters)
plot(det_rast)


##select monthly sampling sites for audio
sampling_sites <- load_sampling_sites("audio", "monthly")

# Create raster for fapar
rast_fapar <- det_rast[[c(1,2,3,4,5,6)]]
plot(rast_fapar)
# Create raster for lai
rast_lai <- det_rast[[c(7,8,9,10,11,12)]]
plot(rast_lai)
# Create raster for pre
rast_pre <- det_rast[[c(13,14,15,16,17,18)]]
plot(rast_pre)
# Create raster for tem
rast_tem <- det_rast[[c(19,20,21,22,23,24)]]
plot(rast_tem)


# Extract by month.
fapar_ext <- data.matrix(extract(rast_fapar, sampling_sites))
lai_ext <- fapar_ext[,-1]

lai_ext <- data.matrix(extract(rast_lai, sampling_sites))
lai_ext <- lai_ext[,-1]
pre_ext <- data.matrix(extract(rast_pre, sampling_sites))
pre_ext <- pre_ext[,-1]
tem_ext <- data.matrix(extract(rast_tem, sampling_sites))
tem_ext <- tem_ext[,-1]

months <- matrix(0,241 , 6)
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

saveRDS(det_covs, "det_covs.RDS")

