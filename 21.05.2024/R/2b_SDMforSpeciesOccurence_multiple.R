library(terra)
library(here)
library(tools)
library(sf)
library(dplyr)
library(fs)
if(!require('sdm')) {
  install.packages('sdm')
  library('sdm')
}
# installAll()

preprocess_images <- function(image_files, ref_image_path) {
  ref_img <- terra::rast(ref_image_path)
  for (img_file in image_files) {
    img_to_resample <- terra::rast(img_file)
    resampled_img <- terra::resample(img_to_resample, ref_img, method = "near")
    print(paste("Resampled", img_file))
    
    cropped_img <- terra::crop(resampled_img, ref_img, mask = TRUE)
    
    transformed_img_filename <- paste0(file_path_sans_ext(basename(img_file)), "_transformed.tif")
    terra::writeRaster(cropped_img, filename = file.path(dirname(img_file), transformed_img_filename), overwrite = TRUE)
    rm(img_to_resample, resampled_img, cropped_img)
  }
}

# Preprocess images
base_dir <- here("Covariates", "4SDM")
ref_image_path <- file.path(base_dir, "dem.tif") # Reference image
image_files <- list.files(path = base_dir, pattern = "\\.tif$", full.names = TRUE)
preprocess_images(image_files, ref_image_path)

#preprocess species occurence 
aoi_df <- read.csv(here("Data","Data_final", "aoi.csv"))

# Filter to retain only species occurring 10 or more times
aoi_df <- aoi_df %>%
  filter(!is.na(taxon_species) & taxon_species != "")

species_counts <- aoi_df %>%
  group_by(taxon_species) %>%
  summarise(count = n())

frequent_species <- species_counts %>%
  filter(count >= 10) %>%
  pull(taxon_species) 

aoi_df <- aoi_df %>%
  filter(taxon_species %in% frequent_species)

##generating 1,0 for each shapefile for each species detected in the opportunistic data
species_path <- here("Data","Data_final", "New_directory for species")

aoi_df <- aoi_df[, c("occurrence_latitude", "occurrence_longitude", "taxon_species")] %>%
  st_as_sf(coords = c("occurrence_longitude", "occurrence_latitude"), crs = 4326)
unique_species <- unique(aoi_df$taxon_species)

for (species in unique_species) {
  # Create a dataframe for the current species with a presence column and select necessary columns
   species_df <- aoi_df %>% #  1 if the species matches, 0 otherwise
    mutate(presence = ifelse(taxon_species == species, 1, 0))%>%
    dplyr::select(presence) 
   # Write the shapefile for each species
   shapefile_path <- shapefile_path <- file.path(species_path, paste0(species, ".shp"))
   st_write(species_df, shapefile_path, delete_layer = TRUE)
    # st_write(species_df, file.path(species_path, paste0(species, ".shp")), delete_layer = TRUE)
   
   species_vect <- terra::vect(shapefile_path)
   presence_counts <- sum(values(species_vect)[, "presence"])
   
   if (presence_counts == 0) {
     cat("No presence records found in:", shapefile_path, "\n")
     # Delete the shapefile and its associated files
     file_remove(path_ext_set(shapefile_path, c("shp", "shx", "dbf", "prj", "cpg")))
   } else {
     cat("Presence records found in:", shapefile_path, "\n")
   }
}
##SDM
output_dir <-  here("Data","Data_final", "Output_SDM")



##all species  shapefiles generated
species_files <- list.files(path = species_path, pattern = "\\.shp$", full.names = TRUE)
for (species_file in seq_along(species_files)) {
  species <- vect(species_files[[species_file]])
 
    lst <- list.files(path = here("Data", "Covariates", "4SDM"), pattern = "_transformed\\.tif$", full.names = TRUE)
    preds <- rast(lst)
    
    d <- sdmData(formula = presence ~ ., train = species, predictors = preds)
    
    m[[species_file]] <- sdm(presence ~ ., data = d, methods = c('rf', 'svm','brt'), replication = "boot", n = 3, test.p = 30)
    
    p1 <- ensemble(m, newdata = preds, setting = list(method = 'weighted', stat = 'AUC'))
    
    # Plot and save the plot
    plot_title <- paste('Habitat Suitability in Geographic Space for', basename(species_file))
    plot(p1, main = plot_title)
    
    # Save the raster output
    raster_output_path <- file.path(output_dir, paste0(basename(species_file, ".shp"), "_SDM.tif"))
    writeRaster(p1, filename = raster_output_path, format = "GTiff", overwrite = TRUE)
    
  } 

aoi <- st_read(here('Data',"Data_final",'northerncluster_campeche_projectsites.geojson'))

vect_list <- lapply(species_files, vect)

vect_list_cropped <- list()

for (i in seq_along(vect_list)) {
  vect_list_cropped[[i]] <- terra::crop(vect_list[[i]], vect(aoi))
}






