library("here")
library("terra")
library("sp")
library("lubridate")

# We need:

# 1
# hbef2015$y has the sp. detections
# it's a 3 dimensional array
# [species, sampling sites, sampling surveys]
# In this case [12, 373, 3]



cam_data <- read.csv(here("spOccupancy", "Data", "bioacoustic.csv")) 

cam_data <- cam_data[cam_data$confidence>0.80,]

cam_data_spatial <- cam_data[,c("species", "day","month","year","latitude", "longitude", "sampling_area")]

coordinates(cam_data_spatial)=~longitude+latitude

# Species will be species
# Sampling sites will be sampling area (nodes)
# Sampling surveys will be months (10, 11, 12)

# for us it will be (species, nodes, sampling surveys)
y <- array(0, c(length(unique(cam_data$species)),
                length(unique(cam_data$sampling_area)),
                length(unique(cam_data$month))))

# Set row names with the species we found in the full sampling effort
row.names(y) <- unique(cam_data$species)

# Now lets fill in the detections with 3 loops
#months<-unique(cam_data$month)
months <- 10:12 ##October to December
nmonths <- 1:3
species <- unique(cam_data$species)
nsp <- 1:length(species) ##number of species
sites <- unique(cam_data$sampling_area) ## nodes were rather 8 for bioacoustic
nsites <- 1:length(sites) #nodes

# Add month variable to camera data 
cam_data$month 

for (month in nmonths){
  for (sp in nsp){
    for (site in nsites){
      month_name <- months[month]
      species_name = species[sp]
      site_name = sites[site]
      
      cam_data_subset <- cam_data[cam_data$month==month_name &
                                    cam_data$species==species_name &
                                    cam_data$sampling_area==site_name,]
      if (nrow(cam_data_subset)!=0){
        y[sp,site,month]<-1 
      }
    }
  }
}

saveRDS(y, "y_bioacoustic.RDS")
