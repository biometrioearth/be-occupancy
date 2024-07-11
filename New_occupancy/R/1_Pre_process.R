# Function to read files based on extension
read_data <- function(file) {
  ext <- file_ext(file)
  if (ext == "csv") {
    return(read.csv(here("Data", "In_situ_data", file)))
  } else if (ext == "xlsx") {
    return(read_excel(here("Data", "In_situ_data", file)))
  } else if (ext == "txt") {
    return(read_delim(here("Data", "In_situ_data", file), delim = "\t"))
  } else {
    stop("Unsupported file extension")
  }
}

# Function to filter data based on threshold
filter_data <- function(df, threshold) {
  column_name <- if ("score" %in% names(df)) {
    "score"
  } else if ("confidence" %in% names(df)) {
    "confidence"
  } else {
    stop("Neither 'score' nor 'confidence' columns are present in the dataframe.")
  }
  return(df[df[[column_name]] > threshold, ])
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
pre_process_data <- function(file_name, threshold, data_type, period) {
  dt_df <- read_data(file_name)
  dt_df <- filter_data(dt_df, threshold)
  dt_df <- date_information(dt_df)
  dt_df_spatial <- create_spatial_data(dt_df, c("species", "longitude", "latitude"))
  
  detection_array <- create_detection_array(dt_df, period)
  return(detection_array)
}