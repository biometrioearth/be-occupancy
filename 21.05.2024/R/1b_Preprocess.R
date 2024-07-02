library(here)
library(terra)
library(sp)
library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)
# the code snippet selects the week and biweek with the most unique species detections for each month. 
# Dataset from video, camera and data mining
audio_df <- read.csv(here("Data_final", "230620_NHUM_MEX_NC_detections_audio.csv"))
video_df <- read_excel(here("Data_final", "230620_NHUM_MEX_NC_detections_image_video 3.xlsx"))
aoi_df <- read.csv(here("Data_final", "aoi.csv"))

##date information
calculate_week_biweek_month <- function(df, datetime_col) {
  df <- df %>%
    mutate(lubdate = ymd_hms(!!sym(datetime_col)))
  
  start_date <- min(df$lubdate, na.rm = TRUE)
  
  df <- df %>%
    mutate(
      week = as.integer((as.numeric(difftime(lubdate, start_date, units = "days")) %/% 7) + 1),
      biweek = as.integer((as.numeric(difftime(lubdate, start_date, units = "days")) %/% 14) + 1),
      month = as.integer((year(lubdate) - year(start_date)) * 12 + month(lubdate) - month(start_date) + 1)
    )
  
  return(df)
}

video_df <- calculate_week_biweek_month(video_df, "date_captured")
audio_df <- calculate_week_biweek_month(audio_df, "datetime")
aoi_df <- calculate_week_biweek_month(aoi_df, "occurrence_eventDate")


# Extract species names
audio_set <- unique(audio_df$species)
video_set <- unique(video_df$species)
aoi_set <- unique(aoi_df$taxon_species)

# Find species common 
common_species_all <- intersect(intersect(audio_set, video_set), aoi_set) #all 3 datasets
common_species_video_audio <- setdiff(intersect(video_set, audio_set), aoi_set) #video and audio only
common_species_video_aoi <- setdiff(intersect(video_set, aoi_set), audio_set)
common_species_aoi_audio <- setdiff(intersect(aoi_set, audio_set), video_set)

selected_species <- c("Nyctidromus albicollis", "Ortalis vetula", "Leptotila verreauxi", "Dumetella carolinensis")

audio_selected <- audio_df[audio_df$species %in% selected_species, ]
video_selected <- video_df[video_df$species %in% selected_species, ]
aoi_selected <- aoi_df[aoi_df$taxon_species %in% selected_species, ]

# Function to get the most unique species detected per week and biweek grouped by month
get_unique_species_per_period <- function(df, datetime_col, period) {
  df <- df %>%
    group_by(month, !!sym(period)) %>%
    summarise(unique_species = n_distinct(species), .groups = 'drop') %>%
    group_by(month) %>%
    top_n(1, wt = unique_species) %>%
    ungroup()
  
  return(df)
}

# video
unique_species_week_audio <- get_unique_species_per_period(audio_selected, "week", "week")
unique_species_biweek_audio <- get_unique_species_per_period(audio_selected, "biweek", "biweek")

#audio
unique_species_week_video <- get_unique_species_per_period(video_selected, "week", "week")
unique_species_biweek_video <- get_unique_species_per_period(video_selected, "biweek", "biweek")

#aoi
unique_species_week_aoi <- get_unique_species_per_period(aoi_selected, "week", "week")
unique_species_biweek_aoi <- get_unique_species_per_period(aoi_selected, "biweek", "biweek")

# Filter the data to include only rows 
audio_filtered <- audio_selected %>%
  filter(week %in% unique_species_week_audio$week | biweek %in% unique_species_biweek_audio$biweek)

video_filtered <- video_selected %>%
  filter(week %in% unique_species_week_video$week | biweek %in% unique_species_biweek_video$biweek)

aoi_filtered <- aoi_selected %>%
  filter(week %in% unique_species_week_aoi$week | biweek %in% unique_species_biweek_aoi$biweek)

# Export the filtered datasets to CSV files
write.csv(audio_filtered, here("Data_final", "audio_filtered.csv"), row.names = FALSE)
write.csv(video_filtered, here("Data_final", "video_filtered.csv"), row.names = FALSE)
write.csv(aoi_filtered, here("Data_final", "aoi_filtered.csv"), row.names = FALSE)


