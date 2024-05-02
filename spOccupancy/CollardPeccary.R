library(here)
library(terra)
library(stars)
library(sf)
library(tidyr)
library(lubridate)
library(dplyr)


##=========PRE-PROCESSING=============================
##raster images
img_files <- list.files(path = here("Data", "Features"), pattern = ".tif$", full.names = TRUE)
task_files <- c("northerncluster_biomes__aster_dem_clip_masked.tif", "dist2roads.tif", "dis2protected.tif",
                 "Temp_Oct.tif", "Temp_Nov.tif","Temp_Dec.tif", "Precip_Oct.tif", "Precip_Nov.tif","northernclustercampeche_biometrio_canopyheight_sentinel20m_gedicomplete_2023", "Precip_Dev.tif", "LAI_Oct.tif", "LAI_Nov.tif", "LAI_Dec.tif",
                "northerncluster_biomes__esaworldcover2021_clip_masked.tif", "northerncluster_biomes__aster_slope_clip_masked.tif")
img_filenames <- basename(img_files)
selected_files <- img_files[img_filenames %in% task_files]
selected_files 

#info on images

crs_list <- lapply(selected_files, function(file) {
  r <- rast(file)
  crs(r)
})

# Print CRS of each image
for (i in seq_along(selected_files)) {
  cat("Image", i, "CRS:", crs_list[[i]], "\n")
}
# Print resolutions for all images
resolutions <- lapply(selected_files, res)
for (i in seq_along(resolutions)) {
  cat("Image", i, "Resolution:", paste(resolutions[[i]], collapse = " x "), "\n")
}

###final try
ref_img <- rast(here::here("Data", "Features", "northerncluster_biomes__esaworldcover2021_clip_masked.tif"))
prepared_data <- list()
task_files <- list.files(path = here("Data", "Selected_files"), pattern = ".tif$", full.names = TRUE)
output_folder <- "C:/Users/AmaBoakye/Work/Occupancy_Modelling/spOccupancy/Data/Selected_files/resampled"


for (img_file in task_files) {
  if (file.exists(img_file)) {  # Check if the file exists
    img_to_process <- rast(img_file)
    resampled_img <- resample(img_to_process, ref_img, method = "near")
    cropped_img <- crop(resampled_img, ref_img)
    cropped_img_filename <- paste0(tools::file_path_sans_ext(basename(img_file)), "_cropped.tif")
    writeRaster(cropped_img, filename = file.path(output_folder, cropped_img_filename), overwrite = TRUE)
    prepared_data[[basename(img_file)]] <- cropped_img
    rm(img_to_process, resampled_img, cropped_img)
  } else {
    cat("File does not exist:", img_file, "\n")
  }
}

###camera trap data
d <- read.csv(here::here("Data", "ama_detections_fix.csv"))
data <- d[, c("sampling_area", "species", "longitude", "latitude", "date_captured","site")]
##temporal aggregation'
data <- data %>%
  mutate(year = lubridate::year(as.Date(date_captured)),
         month = lubridate::month(as.Date(date_captured)))

# Pivot the data to have separate columns for each month
data <- data %>%
  pivot_wider(names_from = month, values_from = date_captured)

 OR
#format(as.Date(data$date_captured), "%Y-%m")
#data <- data %>%
  #mutate(year = lubridate::year(as.Date(date_captured)),
        # month = lubridate::month(as.Date(date_captured)))%>%
#select(-date_captured)
 




# Convert to terra spatial object (assuming you have spatial coordinates in your CSV)
coordinates <- c("longitude", "latitude") # Replace with your actual column names
terra_data <- vect(data, coords = coordinates)
d$corine2<-factor(d$corine2)#categorical
d2<-d[,c("Observation","Species","corine","slope","alt")]#selecting variables
write.table(d2,file="dataBase2.csv",sep=";")#save new table
#coordinate sytem, resoluition, alignment, high resolution