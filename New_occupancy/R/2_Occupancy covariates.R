#####this function creates the occupancy covariates
# Create sampling points to extract
filter_duplicates <- function(period) {
  if (period == "monthly") {
    samp_duplicates <- dt_df[!duplicated(dt_df[, c("latitude", "longitude", "device", "month")]),
                             c("latitude", "longitude", "device", "month")]
  } else if (period == "weekly") {
    samp_duplicates <- dt_df[!duplicated(dt_df[, c("latitude", "longitude", "device", "week")]),
                             c("latitude", "longitude", "device", "week")]
  } else if (period == "biweekly") {
    samp_duplicates <- dt_df[!duplicated(dt_df[, c("latitude", "longitude", "device", "biweekly")]),
                             c("latitude", "longitude", "device", "biweekly")]
  } else {
    stop("Invalid period specified. Choose 'monthly', 'weekly', or 'biweekly'.")
  }
  
  return(samp_duplicates)
}

##function for occupancy covariates
create_occupancy_covariates <- function(file_name, threshold, period) {
  # Determine data type based on file name
  data_type <- ifelse(grepl("audio", file_name, ignore.case = TRUE), "audio", "video")
  
  # Read and process data
  dt_df <- read_data(file_name)
  dt_df <- filter_data(dt_df, threshold)
  dt_df <- date_information(dt_df)
  dt_df_spatial <- create_spatial_data(dt_df, c("device", "species", "longitude", "latitude"))
  
 
  # Load raster files
  occ_rast_files <- list.files(here("Data", "Occupa"),
                               pattern = "\\.tif$",
                               full.names = TRUE)
  occ_rast_list <- list()
  for (r in occ_rast_files) {
    rast_obj <- rast(r)
    occ_rast_list[[length(occ_rast_list) + 1]] <- rast_obj
  }
  occ_rast <- rast(occ_rast_list)
  
  # Filter duplicates based on the period
  samp_duplicates <- filter_duplicates(period)
  
  # Create sampling points
  sampling_points <- vect(samp_duplicates,
                          geom = c("longitude", "latitude"),
                          crs = "+proj=longlat +datum=WGS84")
  
  # Extract data from rasters to points
  extracted_covs <- extract(occ_rast, sampling_points)
  names(extracted_covs) <- c("ID", "bare_sparse_vege", "biomass", "builtup", "canopyheight", 
                             "cropland", "dem", "dist_roads", "fire_count", "fire_distance",
                             "grassland", "herbaceous_wetland", "mangroves", "connectivity", "dist_protected",
                             "shrubland", "slope", "tree_cover_loss_prop", "tree_cover", "water_dist", "prop_waterbodies")
  
  # Merge extracted covariates with sampling points
  sampling_points$ID <- 1:nrow(sampling_points)
  sampling_points <- merge(sampling_points, extracted_covs, by = "ID")
  
  # Save the results to a shapefile with period and data type in the name
  shapefile_name <- paste0("sampling_points_", data_type, "_", period, ".shp")
  writeVector(sampling_points, shapefile_name, overwrite = TRUE)
  
  return(sampling_points)
}

