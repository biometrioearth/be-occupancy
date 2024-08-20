
# Load libraries
library("terra")
library("sp")
library("readxl")
library("lubridate")
library("dplyr")
library("ggplot2")
library("tools")
library("sf")

######################### Pre-processing functions

# Function to read files based on extension
read_data <- function(file) {
  ext <- file_ext(file)
  if (ext == "csv") {
    return(read.csv(file))
  } else if (ext == "xlsx") {
    return(read_excel(file))
  } else if (ext == "txt") {
    return(read_delim(file, delim = "\t"))
  } else {
    stop("Unsupported file extension.")
  }
}

# Function to filter data based on threshold
filter_data <- function(df, threshold = 0.9, column_name = "score") {
  return(df[df[[column_name]] > threshold,])
}

# Function to create spatial data
create_spatial_data <- function(df, columns) {
  df_spatial <- df[, columns]
  coordinates(df_spatial) <- ~longitude + latitude
  return(df_spatial)
}

# Function to process date information
date_information <- function(df) {
  df$lubdate <- ymd_hms(df$local_datetime)
  df$year <- year(df$lubdate)
  df$month <- month(df$lubdate)
  df$day <- day(df$lubdate)
  start_date <- min(df$lubdate, na.rm = TRUE)
  df$week <- as.integer((as.numeric(difftime(df$lubdate, start_date, units = "days")) %/% 7) + 1)
  df$biweekly <- as.integer((as.numeric(difftime(df$lubdate, start_date, units = "days")) %/% 14) + 1)
  return(df)
}

# Function to create detection array
create_detection_array <- function(dt_df, period) {
  species <- unique(dt_df$species)
  sites <- unique(dt_df$sampling_point_id)
  
  if (period == "weekly") {
    weeks <- unique(dt_df$week)
    nweeks <- 1:length(weeks)
    data_video <- array(0, c(length(species), length(sites), length(nweeks)))
    row.names(data_video) <- species
    
    for (week in nweeks) {
      for (sp in 1:length(species)) {
        for (site in 1:length(sites)) {
          week_name <- weeks[week]
          species_name <- species[sp]
          site_name <- sites[site]
          
          cam_data_subset <- dt_df[dt_df$week == week_name &
                                     dt_df$species == species_name &
                                     dt_df$sampling_point_id == site_name, ]
          if (nrow(cam_data_subset) != 0) {
            data_video[sp, site, week] <- 1 
          }
        }
      }
    }
  } else if (period == "biweekly") {
    biweeks <- unique(dt_df$biweekly)
    nbiweeks <- 1:length(biweeks)
    data_video <- array(0, c(length(species), length(sites), length(nbiweeks)))
    row.names(data_video) <- species
    
    for (biweek in nbiweeks) {
      for (sp in 1:length(species)) {
        for (site in 1:length(sites)) {
          biweek_name <- biweeks[biweek]
          species_name <- species[sp]
          site_name <- sites[site]
          
          cam_data_subset <- dt_df[dt_df$biweekly == biweek_name &
                                     dt_df$species == species_name &
                                     dt_df$sampling_point_id == site_name, ]
          if (nrow(cam_data_subset) != 0) {
            data_video[sp, site, biweek] <- 1
          }
        }
      }
    }
  } else if (period == "monthly") {
    months <- unique(dt_df$month)
    nmonths <- 1:length(months)
    data_video <- array(0, c(length(species), length(sites), length(nmonths)))
    row.names(data_video) <- species
    
    for (month in nmonths) {
      for (sp in 1:length(species)) {
        for (site in 1:length(sites)) {
          month_name <- months[month]
          species_name <- species[sp]
          site_name <- sites[site]
          
          cam_data_subset <- dt_df[dt_df$month == month_name &
                                     dt_df$species == species_name &
                                     dt_df$sampling_point_id == site_name, ]
          if (nrow(cam_data_subset) != 0) {
            data_video[sp, site, month] <- 1
          }
        }
      }
    }
  } else {
    stop("Invalid period specified. Choose 'weekly', 'biweekly', or 'monthly'.")
  }
  
  return(data_video)
}

# Function to preprocess data
pre_process_data <- function(file_name, threshold=0.9, period='weekly') {
  dt_df <- read_data(file_name)
  dt_df <- filter_data(dt_df, threshold=threshold)
  dt_df <- date_information(dt_df)
  dt_df_spatial <- create_spatial_data(dt_df, c("species", "longitude", "latitude"))
  
  detection_array <- create_detection_array(dt_df, period=period)
  return(detection_array)
}

#########################

######################### Occupancy covariates functions

# Create sampling points to extract
filter_duplicates <- function(dt_df, period) {
  if (period == "monthly") {
    samp_duplicates <- dt_df[!duplicated(dt_df[, c("latitude", "longitude", "sampling_point_id")]),
                             c("latitude", "longitude", "sampling_point_id")]
  } else if (period == "weekly") {
    samp_duplicates <- dt_df[!duplicated(dt_df[, c("latitude", "longitude", "sampling_point_id")]),
                             c("latitude", "longitude", "sampling_point_id")]
  } else if (period == "biweekly") {
    samp_duplicates <- dt_df[!duplicated(dt_df[, c("latitude", "longitude", "sampling_point_id")]),
                             c("latitude", "longitude", "sampling_point_id")]
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
  dt_df <- filter_data(dt_df, threshold = threshold)
  dt_df <- date_information(dt_df)
  dt_df_spatial <- create_spatial_data(dt_df, c("sampling_point_id", "species", "longitude", "latitude"))
  
  
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
  samp_duplicates <- filter_duplicates(dt_df=dt_df, period = period)
  
  # Create sampling points
  sampling_points <- vect(samp_duplicates,
                          geom = c("longitude", "latitude"),
                          crs = "+proj=longlat +datum=WGS84")
  
  # Extract data from rasters to points
  extracted_covs <- extract(occ_rast, sampling_points)
  #names(extracted_covs) <- c("ID", "bare_sparse_vege", "biomass", "builtup", "canopyheight", 
  # "cropland", "dem", "dist_roads", "fire_count", "fire_distance",
  #"grassland", "herbaceous_wetland", "mangroves", "connectivity", "dist_protected",
  #"shrubland", "slope", "tree_cover_loss_prop", "tree_cover", "water_dist", "prop_waterbodies")
  
  # Merge extracted covariates with sampling points
  sampling_points$ID <- 1:nrow(sampling_points)
  sampling_points <- merge(sampling_points, extracted_covs, by = "ID")
  
  # Save the results to a shapefile with period and data type in the name
  shapefile_name <- paste0("sampling_points_", data_type, "_", period, ".shp")
  output_file <- here("Data", "Processed", shapefile_name)
  writeVector(sampling_points, output_file, overwrite = TRUE)
  
  return(sampling_points)
}

#########################

######################### Detectability covariates functions

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

load_sampling_sites <- function(data_type, period) {
  # Validate the type
  if (!data_type %in% c("audio", "video")) {
    stop("Invalid type. Must be 'audio' or 'video'.")
  }
  # Validate the period
  if (!period %in% c("weekly", "biweekly", "monthly")) {
    stop("Invalid period. Must be 'weekly', 'biweekly', or 'monthly'.")
  }
  # Construct the filename based on type and period
  filename <- paste0("sampling_points_", data_type, "_", period, ".shp")
  
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


VIFcalc<-function(d)
{
  result<-data.frame(var=c(names(d)),
                     VIF=numeric(length(d[1,])))
  for(i in (1:length(d[1,]))) 
  {
    result$VIF[i] <-1/(1-summary(lm(d[,i] ~ .,data=d[,names(d)!=names(d)[i]]))$r.squared)
  }
  result<-result[sort(result$VIF,decreasing=T,index.return=T)$ix,]
  return(result)
}

##creates occupancy coviates with species names
create_occ_species<- function(file_name, threshold, period) {
  
  data_type <- ifelse(grepl("audio", file_name, ignore.case = TRUE), "audio", "video")
  dt_df <- read_data(file_name)
  dt_df <- filter_data(dt_df, threshold = threshold)
  dt_df <- date_information(dt_df)
  dt_df_spatial <- create_spatial_data(dt_df, c("sampling_point_id", "species", "longitude", "latitude"))
  species_name <- unique(dt_df$species)
  species_name <- gsub(" ", "_", species_name[1])  
  
  occ_rast_files <- list.files(here("Data", "Occupa"),
                               pattern = "\\.tif$",
                               full.names = TRUE)
  
  occ_rast_list <- list()
  
  for (r in occ_rast_files) {
    rast_obj <- rast(r)
    occ_rast_list[[length(occ_rast_list) + 1]] <- rast_obj
  }
  
  occ_rast <- rast(occ_rast_list)
  samp_duplicates <- filter_duplicates(dt_df = dt_df, period = period)
  
  sampling_points <- vect(samp_duplicates,
                          geom = c("longitude", "latitude"),
                          crs = "+proj=longlat +datum=WGS84")
  
  extracted_covs <- extract(occ_rast, sampling_points)
  
  sampling_points$ID <- 1:nrow(sampling_points)
  
  sampling_points <- merge(sampling_points, extracted_covs, by = "ID")
  
  output_file <- here("Data", "Output_minmax", paste0(species_name, "_", data_type, "_", period, ".shp"))
  writeVector(sampling_points, output_file, overwrite = TRUE)
  
  return(sampling_points)
}

##other functions to help with selected species
load_shp <- function(data_type, period) {
  if (!data_type %in% c("audio", "video")) {
    stop("Invalid type. Must be 'audio' or 'video'.")
  }
  if (!period %in% c("weekly", "biweekly", "monthly")) {
    stop("Invalid period. Must be 'weekly', 'biweekly', or 'monthly'.")
  }
  directory_path <- here("Data", "Output_minmax", "Most_species") ## usually change this part
  shapefiles <- list.files(directory_path, pattern = "\\.shp$", full.names = TRUE)
  selected_shapefiles <- shapefiles[grepl(data_type, shapefiles) & grepl(period, shapefiles)]
  if (length(selected_shapefiles) == 0) {
    stop("No shapefile found matching the specified data type and period.")
  }
  sampling_sites <- vect(selected_shapefiles[1])
  
  num_points <- nrow(sampling_sites)
  cat("Number of points in sampling sites:", num_points, "\n")
  
  return(sampling_sites)
}


# find number of given species
get_species_column_number <- function(data, species_name) {
  col_index <- which(colnames(data) == species_name)
  if (length(col_index) == 0) {
    cat("Species not found!\n")
    return(NA)
  } else {
    return(col_index)
  }
}
