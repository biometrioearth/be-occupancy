library("here")
library("terra")
library("sp")
library("readxl")
library("lubridate")
library("dplyr")
library("ggplot2")


#datasets
audio_df <- read.csv(here("Data","Data_final","230620_NHUM_MEX_NC_detections_audio.csv"))
video_df <- read_excel(here("Data","Data_final", "230620_NHUM_MEX_NC_detections_image_video 3.xlsx"))
video_df <- video_df[video_df$score>0.90,]



audio_df_spatial <- audio_df[,c("confidence","species","local_datetime","year","month","day", "latitude", "longitude", "sampling_area", "device","date_deployment","date_collected")]
video_df_spatial <- video_df[,c("date_captured","device", "latitude", "longitude", "sampling_area",  "local_datetime", "date_deployment","date_collected", "species")]


##date information
video_df$lubdate <- ymd_hms(video_df$local_datetime)
video_df$year = year(video_df$lubdate)
video_df$month = month(video_df$lubdate)
video_df$day = day(video_df$lubdate)

#video_df$week <- week(video_df$lubdate)
start_date <- min(video_df$lubdate, na.rm = TRUE)
video_df$week <- as.integer((as.numeric(difftime(video_df$lubdate, start_date, units = "days")) %/% 7) + 1)
video_df$biweekly <- as.integer((as.numeric(difftime(video_df$lubdate, start_date, units = "days")) %/% 14) + 1)


coordinates(video_df_spatial) <- ~longitude + latitude


##Empty array video
data_video_weekly <- array(0, c(length(unique(video_df$species)),
                length(unique(video_df$device)),
                length(unique(video_df$week))))

data_video_biweekly <- array(0, c(length(unique(video_df$species)),
                                  length(unique(video_df$device)),
                                  length(unique(video_df$biweekly))))

data_video_monthly <- array(0, c(length(unique(video_df$species)),
                                  length(unique(video_df$device)),
                                  length(unique(video_df$month))))
#  fill in the detections in loops
weeks<- unique(video_df$week) 
nweeks <- 1:length(weeks)
biweeks <- unique(video_df$biweekly) ##biweekly
nbiweeks <- 1:length(biweeks)
species <- unique(video_df$species)
nsp <- 1:length(species) ##number of species
sites <- unique(video_df$device)
nsites <- 1:length(sites) #devices
months <- unique(video_df$month)
nmonths <-1:6

# Set row names with the species we found in the full sampling effort
row.names(data_video_weekly) <- unique(video_df$species)
video_df$week

# weekly
for (week in nweeks){
  for (sp in nsp){
    for (site in nsites){
      week_name <- weeks[week]
      species_name = species[sp]
      site_name = sites[site]
      
      cam_data_subset <- video_df[video_df$week==week_name &
                                    video_df$species==species_name &
                                    video_df$device==site_name,]
      if (nrow(cam_data_subset)!=0){
        data_video_weekly[sp,site,week]<-1 
      }
    }
  }
}

saveRDS(data_video_weekly, "data_video_weekly.RDS")

##biweekly detection loop
row.names(data_video_biweekly) <- unique(video_df$species)
video_df$biweekly

for (biweek in nbiweeks) {
  for (sp in nsp) {
    for (site in nsites) {
      biweek_name <- biweeks[biweek]
      species_name <- species[sp]
      site_name <- sites[site]
      cam_data_subset <- video_df[video_df$biweekly == biweek_name & 
                                    video_df$species == species_name & 
                                    video_df$device == site_name,]
      if (nrow(cam_data_subset) != 0) {
        data_video_biweekly[sp, site, biweek] <- 1
      }
    }
  }
}

# Save the biweekly data matrix
saveRDS(data_video_biweekly, "data_video_biweekly.RDS")



##monthly
row.names(data_video_monthly) <- unique(video_df$species)
video_df$month

for (month in nmonths) {
  for (sp in nsp) {
    for (site in nsites) {
      month_name <- months[month]
      species_name <- species[sp]
      site_name <- sites[site]
      cam_data_subset <- video_df[video_df$month == month_name & 
                                    video_df$species == species_name & 
                                    video_df$device == site_name,]
      if (nrow(cam_data_subset) != 0) {
        data_video_monthly[sp, site, month] <- 1
      }
    }
  }
}

# Save the monthly data matrix
saveRDS(data_video_monthly, "data_video_monthly.RDS")




##==================================================================================================================
#              bioacoustics
##==================================================================================================================


audio_df$lubdate <- ymd_hms(audio_df$local_datetime)
start_date_audio <- min(audio_df$lubdate, na.rm = TRUE)
audio_df$week <- as.integer((as.numeric(difftime(audio_df$lubdate, start_date_audio, units = "days")) %/% 7) + 1)
audio_df$biweekly <- as.integer((as.numeric(difftime(audio_df$lubdate, start_date_audio, units = "days")) %/% 14) + 1)


coordinates(audio_df_spatial) <- ~longitude + latitude
##Empty array audio
data_audio_weekly <- array(0, c(length(unique(audio_df$species)),
                                length(unique(audio_df$device)),
                                length(unique(audio_df$week))))

data_audio_biweekly <- array(0, c(length(unique(audio_df$species)),
                                  length(unique(audio_df$device)),
                                  length(unique(audio_df$biweekly))))

data_audio_monthly <- array(0, c(length(unique(audio_df$species)),
                                 length(unique(audio_df$device)),
                                 length(unique(audio_df$month))))
#rownames


row.names(data_audio_monthly) <- unique(audio_df$species)

# Weekly detection loop for audio

weeks_audio <- unique(audio_df$week)
nweeks_audio <- 1:length(weeks_audio)
species_audio <-unique(audio_df$species)
nsp_audio <-1:length(species_audio)
sites_audio <-unique(audio_df$device)
nsites_audio<-1:length(sites_audio)
biweeks_audio<-unique(audio_df$biweekly)
nbiweekly <-1:length(biweeks_audio)
months_audio <- unique(audio_df$month)
nmonths_audio <-1:length(months_audio)

# weekly
row.names(data_audio_weekly) <- unique(audio_df$species)
audio_df$week
for (week in nweeks_audio){
  for (sp in nsp_audio){
    for (site in nsites_audio){
      week_name <- weeks_audio[week]
      species_name <- species_audio[sp]
      site_name <-sites_audio[site]
      
      cam_data_subset <- audio_df[audio_df$week == week_name &
                                    audio_df$species == species_name &
                                    audio_df$device == site_name, ]
      if (nrow(cam_data_subset) != 0){
        data_audio_weekly[sp, site, week] <- 1 
      }
    }
  }
}



# Biweekly detection loop for audio
row.names(data_audio_biweekly) <- unique(audio_df$species)
audio_df$biweekly

for (biweek in nbiweekly) {
  for (sp in nsp_audio){
    for (site in nsites_audio){
      biweek_name <- biweeks_audio[biweek]
      species_name <- species_audio[sp]
      site_name <- sites_audio[site]
      
      cam_data_subset <- audio_df[audio_df$biweekly == biweek_name & 
                                    audio_df$species == species_name & 
                                    audio_df$device == site_name,]
      if (nrow(cam_data_subset) != 0) {
        data_audio_biweekly[sp, site, biweek] <- 1
      }
    }
  }
}

# Monthly detection loop for audio
row.names(data_audio_monthly) <- unique(audio_df$species)
audio_df$month

for (month in nmonths_audio) {
  for (sp in nsp_audio){
    for (site in nsites_audio){
      month_name <- months_audio[month]
      species_name <- species_audio[sp]
      site_name <- sites_audio[site]
      
      cam_data_subset <- audio_df[audio_df$month == month_name & 
                                    audio_df$species == species_name & 
                                    audio_df$device == site_name,]
      if (nrow(cam_data_subset) != 0) {
        data_audio_monthly[sp, site, month] <- 1
      }
    }
  }
}

# Save the data matrices
saveRDS(data_audio_weekly, "data_audio_weekly.RDS")
saveRDS(data_audio_biweekly, "data_audio_biweekly.RDS")
saveRDS(data_audio_monthly, "data_audio_monthly.RDS")




