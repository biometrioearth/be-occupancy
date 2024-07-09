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
source(here("R","2_Occupancy covariates.R"))
source(here("R", "3_detectability_covariates.R"))
#source(here("R", "4_fit_occupancy_model.R"))


##preprocess

file_name <- "230620_NHUM_MEX_NC_detections_image_video 3.xlsx"
threshold <- 0.9
data_type <- "video"  # or "video"
period <- "monthly"  # or "biweekly", "monthly"

video_monthly<-pre_process_data(file_name, threshold, data_type, period)
output_file <- here("Data", "Processed", paste0(data_type, "_", period, "_detection_array.RDS"))
saveRDS(audio_weekly, output_file)


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
#extents <- lapply(weekly_rasters, ext)
#resolutions <- lapply(det_rast_list, res)

## for weekly rasters
det_rast <- rast(monthly_rasters)
plot(det_rast)

##select monthly sampling sites for audio
sampling_sites <- load_sampling_sites("video", "monthly")

# Create raster for fapar
rast_fapar <- det_rast[[1:6]]
plot(rast_fapar)
# Create raster for lai
rast_lai <- det_rast[[7:12]]
plot(rast_lai)
# Create raster for pre
rast_pre <- det_rast[[13:18]]
plot(rast_pre)
# Create raster for tem
rast_tem <- det_rast[[19:24]]
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

data_matrix <- create_matrix(sampling_sites, 6)
data_matrix[,1] <- 10  # October
data_matrix[,2] <- 11  # November
data_matrix[,3] <- 12  # December
data_matrix[,4] <- 1   # January
data_matrix[,5] <- 2   # February
data_matrix[,6] <- 3   # March

# Save as list
det_covs <- list(fapar=fapar_ext,
                 lai=lai_ext,
                 pre=pre_ext,
                 tem=tem_ext,
                 month=data_matrix)

saveRDS(det_covs, here("Data","Processed",paste0( period, "_det_covs.RDS")))


##============
file_name <- "230620_NHUM_MEX_NC_detections_image_video 3.xlsx"
threshold <- 0.9
period <- "monthly" 
dt_df <- read_data(file_name)

dt_df <- filter_data(dt_df, threshold)
dt_df <- date_information(dt_df)
samp_duplicates <- filter_duplicates(period)
samp_coords_grouped <- group_by(samp_duplicates, device)
