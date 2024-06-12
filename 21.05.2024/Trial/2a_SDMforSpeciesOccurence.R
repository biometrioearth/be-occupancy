library(terra)
library(here)
library(tools)
library(sf)
library(dplyr)
if(!require('sdm')) {
  install.packages('sdm')
  library('sdm')
}
installAll()

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
ref_image_path <- file.path(base_dir, "ESA_LC.tif") # Reference image
image_files <- list.files(path = base_dir, pattern = "\\.tif$", full.names = TRUE)
preprocess_images(image_files, ref_image_path)

#preprocess species occurence 
aoi_df <- read.csv(here("Data_final", "aoi.csv"))

# Filter to retain only species occurring 2 or more times
aoi_df <- aoi_df %>%
  filter(!is.na(taxon_species) & taxon_species != "")

species_counts <- aoi_df %>%
  group_by(taxon_species) %>%
  summarise(count = n())

frequent_species <- species_counts %>%
  filter(count >= 2) %>%
  pull(taxon_species) 

aoi_df <- aoi_df %>%
  filter(taxon_species %in% frequent_species)

##generating 1,0 for each species
species_path <- here("Data_final", "New_directory for species")

aoi_df <- aoi_df[, c("occurrence_latitude", "occurrence_longitude", "taxon_species")] %>%
  st_as_sf(coords = c("occurrence_longitude", "occurrence_latitude"), crs = 4326)
unique_species <- unique(aoi_df$taxon_species)

for (species in unique_species) {
  species_df <- aoi_df %>% #  1 if the species matches, 0 otherwise
    mutate(presence = ifelse(taxon_species == species, 1, 0))%>%
    select(presence)  
  
  # Write the shapefile for each species
  st_write(species_df, file.path(species_path, paste0(species, ".shp")))
}


##SDM

#if (FALSE) {

  species <- vect(here("Data_final","New_directory for species","Acrostichum danaeifolium.shp")) # read the shapefile
  lst <- list.files(path = here("Covariates", "4SDM"),pattern= "_transformed\\.tif$", full.names = T)#list the name of the raster files 
  # to read/create a multi-layers raster dataset
  preds <- rast(lst) # making a raster object
  names(preds) # 4 environmental variables are used!
  d <- sdmData(formula=Occurrence~., train=species, predictors=preds)
  
  d
  
  # fit models:
  m <- sdm(presence~.,data=d,methods=c('rf','glm','brt'))
  
  # ensemble using weighted averaging based on AUC statistic:    
  p1 <- ensemble(m, newdata=preds,setting=list(method='weighted',stat='AUC'))
  plot(p1, main='Habitat Suitability in Geographic Space')
  
  # Mapping Ecological Niche using selected two variables
  niche(x=preds, h=p1, c('precipitation','temperature'))
  
  niche(x=preds, h=p1, c('vegetation','temperature'))
  
  # in case if you do not have the habitat suitability map but species data:
  
  niche(x=preds, h=species, c('vegetation','temperature','Occurrence'))
  
  
  niche(x=preds, h=d, n=c('vegetation','temperature','Occurrence'), rnd=2) 
  # rnd is the argument specifies the decimal degrees to which the values on axis rounded.
  
  
  
#}

