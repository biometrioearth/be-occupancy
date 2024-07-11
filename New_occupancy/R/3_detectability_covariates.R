config <- read.config(here("Config","detectability_cov.yaml"))

extend_resample_to_match <- function(rasters, ref_raster_path) {
  # Load the reference raster
  ref_raster <- rast(ref_raster_path)
  
  # Extend and resample all rasters to match the reference raster
  resampled_rasters <- lapply(rasters, function(r) {
    # Extend the raster to match the reference extent
    extended_raster <- extend(r, ext(ref_raster))
    # Resample the raster to match the reference resolution and extent
    resampled_raster <- resample(extended_raster, ref_raster)
    return(resampled_raster)
  })
  
  return(resampled_rasters)
}

replace_na_with_zero <- function(raster, ref_raster) {
  na_mask <- is.na(raster) & !is.na(ref_raster)
  raster[na_mask] <- 0
  return(raster)
}

load_rasters <- function(frequency, ref_raster_path) {
  # Construct the path pattern based on the frequency type
  path_pattern <- switch(frequency,
                         "weekly" = "Weekly",
                         "biweekly" = "Biweekly",
                         "monthly" = "Monthly",
                         stop("Invalid frequency type"))
  
  # Identify all folders with the specified frequency in their names within the Detectability folder
  det_rast_files <- list.dirs(here("Data", "Detect"), recursive = TRUE, full.names = TRUE)
  det_rast_files <- det_rast_files[grepl(path_pattern, det_rast_files)]
  

  det_rast_list <- list()
  counter <- 0
  
  # Loop through each folder, list the .tif files, and process them
  for (folder in det_rast_files) {
    tif_files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
    
    for (r in tif_files) {
      counter <- counter + 1
      tmpr <- rast(r)
      names(tmpr) <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(r))
      det_rast_list[[counter]] <- tmpr
    }
  }
  
  # Extend and resample rasters to match the reference raster
  det_rast_list <- extend_resample_to_match(det_rast_list, ref_raster_path)
  
  # Replace NAs with 0 where the reference raster has a value
  processed_rast_list <- lapply(det_rast_list, function(r) replace_na_with_zero(r, rast(ref_raster_path)))
  
  return(processed_rast_list)
}

load_sampling_sites <- function(data_type, frequency) {
  # Validate the type
  if (!data_type %in% c("audio", "video")) {
    stop("Invalid type. Must be 'audio' or 'video'.")
  }
  # Validate the frequency
  if (!frequency %in% c("weekly", "biweekly", "monthly")) {
    stop("Invalid frequency. Must be 'weekly', 'biweekly', or 'monthly'.")
  }
  # Construct the filename based on type and frequency
  filename <- paste0("sampling_points_", data_type, "_", frequency, ".shp")
  

  shapefile_path <- here("Data","Processed",filename)
  # Load the shapefile
  sampling_sites <- vect(shapefile_path)
  num_points <- nrow(sampling_sites)
  cat("Number of points in sampling sites:", num_points, "\n")
  
  
  return(sampling_sites)
}

create_matrix <- function(sampling_sites, num_groups) {
  num_sites <- nrow(sampling_sites)
  empty_matrix <- matrix(0, num_sites, num_groups)
  
  return(empty_matrix)
}
