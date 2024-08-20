library("here")
library("spOccupancy")
library("configr")

source(here("R", "occ_functions.R"))

## most species weekly, biweekly, monthly
file_name <- "230620_NHUM_MEX_NC_detections_image_video 3.xlsx"
threshold <- 0.9
data_type <- "video"  # or "video"

video<-read_data(here("Data","In_situ_data",file_name))
video<-filter_data(video,threshold = 0.9)
video<-date_information(video)

##loop through to identify most, median and least species detected
# Extract species names
video_species <- unique(video$species)
#species_counts1 <- table(video$species)
species_counts <- video %>%
  group_by(species) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


#most, medium, least detected species
most_species <- species_counts %>% slice(1)

#medium and least
Q1 <- quantile(species_counts$count, 0.25)
Q3 <- quantile(species_counts$count, 0.75)
IQR <- Q3 - Q1
medium_detected_species <- species_counts %>%
  filter(count >= Q1 & count <= Q3)
medium_species <- mediumly_detected_species %>% slice(1)
least_species<- mediumly_detected_species %>% slice(16)
##dataframes
most_df <- video %>% filter(species == "Pecari tajacu")
medium_df <- video %>% filter(species == "Leopardus pardalis")
least_df <- video %>% filter(species == "Dumetella carolinensis")

#exporting csv files
most_species <- here("Data", "Processed", "Pecari_tajacu_video.csv")
medium_species <- here("Data", "Processed", "Leopardus_pardalis_video.csv")
least_species <- here("Data", "Processed", "Dumetella carolinensis_video.csv")

write.csv(most_df, most_species, row.names = FALSE)
write.csv(medium_df, medium_species, row.names = FALSE)
write.csv(least_df, least_species, row.names = FALSE)


# Specify the species name  with index number
species_name <- "Leopardus pardalis"
column_number <- get_species_column_number(y, species_name)
print(column_number)


##========================================================================================
#Export arrays
##========================================================================================
file_name <- medium_species
period <- "weekly"  # or "biweekly", "monthly"

video_monthly <- pre_process_data(file_name, threshold, period)
output_file  <- here("Data","Output_minmax", paste0("least_species", "_", period, "_detection_array.RDS"))
saveRDS(video_monthly, output_file)


##========================================================================================
## Create Occupancy covariates
##========================================================================================
sampling_points <- create_occ_species(file_name, threshold, period=period) 


##========================================================================================
## Create Detectability covariates
##========================================================================================

det_rast_files <- list.files(here("Data", "Detect","Weekly_detectability_try"),
                             pattern = "\\.tif$",
                             full.names = TRUE)
det_rast_list <- list()
counter <- 0
for (r in det_rast_files){
  counter <- counter+1
  rast_obj <- rast(r)
  det_rast_list[[counter]] <- rast_obj
}


## Load rasters with terra.
det_brick <- rast(det_rast_list)

#select monthly sampling sites for audio

data_type <- "video"
period <- "weekly"
sampling_sites <- load_shp(data_type = data_type, period=period)

plot(det_brick)
# Create raster for fapar
rast_fapar <- det_brick[[1:25]]
plot(rast_fapar)
# Create raster for lai
rast_lai <- det_brick[[26:50]]
plot(rast_lai)
# Create raster for pre
rast_pre <- det_brick[[51:75]]
plot(rast_pre)
# Create raster for tem
rast_tem <- det_brick[[76:100]]
plot(rast_tem)

# Extract by week.
fapar_ext <- data.matrix(extract(rast_fapar, sampling_sites))
fapar_ext <- fapar_ext[,-1]

lai_ext <- data.matrix(extract(rast_lai, sampling_sites))
lai_ext <- lai_ext[,-1]
pre_ext <- data.matrix(extract(rast_pre, sampling_sites))
pre_ext <- pre_ext[,-1]
tem_ext <- data.matrix(extract(rast_tem, sampling_sites))
tem_ext <- tem_ext[,-1]

weeks <- matrix(0,24,25)


for (i in 1:25) {
  weeks[,i] <- i  
}

# Save as list
det_covs <- list(fapar=fapar_ext,
                 lai=lai_ext,
                 pre=pre_ext,
                 tem=tem_ext,
                 week=weeks)

saveRDS(det_covs, here("Data", "Output_minmax", "Medium_species", paste0(period, "_medium_det_covs.RDS")))


# Extract by month.
fapar_ext <- data.matrix(extract(rast_fapar, sampling_sites))
fapar_ext <- fapar_ext[,-1]

lai_ext <- data.matrix(extract(rast_lai, sampling_sites))
lai_ext <- lai_ext[,-1]
pre_ext <- data.matrix(extract(rast_pre, sampling_sites))
pre_ext <- pre_ext[,-1]
tem_ext <- data.matrix(extract(rast_tem, sampling_sites))
tem_ext <- tem_ext[,-1]

months <- matrix(0,65,6)

#data_matrix <- create_matrix(sampling_sites, 6)
months[,1] <- 10  # October
months[,2] <- 11  # November
months[,3] <- 12  # December
months[,4] <- 1   # January
months[,5] <- 2   # February
months[,6] <- 3   # March

##========================================================================================
## Model Fitting
##========================================================================================

a<-as.data.frame(importance_list[[1]])
fit_list[[1]] 

#correlation and VIF

## select first 10 variables
sorted_importance <- sort(average_importance, decreasing = TRUE)
top_13_variables <- names(sorted_importance)[1:10]
occ.cov <- occ_cov[, top_13_variables]

# Convert to a data frame and handle NA values
occ.cov_df <- as.data.frame(occ.cov)
occ.cov_df[is.na(occ.cov_df)] <- 0
occ.cov_df[] <- lapply(occ.cov_df, as.numeric)

#  VIF calculation function
VIFcalc <- function(d) {
  result <- data.frame(var = colnames(d), VIF = numeric(ncol(d)))
  for (i in 1:ncol(d)) {
    formula <- as.formula(paste(colnames(d)[i], "~ ."))
    lm_model <- lm(formula, data = as.data.frame(d))
    r_squared <- summary(lm_model)$r.squared
    result$VIF[i] <- 1 / (1 - r_squared)
  }
  result <- result[order(result$VIF, decreasing = TRUE), ]
  return(result)
}

# Convert the data frame to a matrix
occ.cov_matrix <- as.matrix(occ.cov_df)
colnames(occ.cov_matrix) <- colnames(occ.cov_df)

# Calculate the VIF values 
vif_results <- VIFcalc(occ.cov_matrix)
print(vif_results)

#check correlation
cor_matrix <- cor(occ.cov_df)

##exclude VIF values 10 and above
high_vif_vars <- vif_results$var[vif_results$VIF >= 10]
occ.cov <- occ.cov_df[, !colnames(occ.cov_df) %in% high_vif_vars]
VIFcalc(occ.cov)
cor_matrix <- cor(occ.cov)



## occupancy modelling and prediction for most species
y <- readRDS(here("Data", "Output_minmax", "Most_species","most_species_biweekly_detection_array.RDS"))
data_type <- "video"
period <- "biweekly"  
sampling_sites <- load_shp(data_type = data_type, period=period)

column_names <- names(occ.cov)
column_names_quoted <- paste0('"', column_names, '"')
column_names_str <- paste(column_names_quoted, collapse = ", ")

occ.cov <- sampling_sites[, column_names]
occ.cov <- as.data.frame(occ.cov)
occ.cov[is.na(occ.cov)] <- 0

occ.cov <- data.matrix(occ.cov)

formula_str <- paste("~", paste0("scale(", column_names, ")", collapse = " + "))
occ.formula <- as.formula(formula_str)
#print(occ.formula)

det.covs <- readRDS(here("Data", "Output_minmax", "Most_species", "weekly_most_det_covs.RDS")) #must be data matrix
for (i in seq_along(det.covs)) {
  det.covs[[i]][is.na(det.covs[[i]])] <- 0
}
det.formula <- ~ I(scale(week))+ scale(tem)
#det.formula <- ~ I(scale(week)) + scale(tem) + scale(fapar) + scale(lai)+ scale(pre)

coords <- geom(sampling_sites)[, c("x", "y")]
coords <- data.matrix(coords)

i=1 # species
print(i)

y_i <- y[i,,]


occ_table_pca <- list(y = y_i, occ.covs = occ.cov, det.covs = det.covs, coords = coords)


out_pca <- spPGOcc(occ.formula = occ.formula,
                   det.formula = det.formula,
                   data = occ_table_pca, n.batch = 400, batch.length = 25,
                   accept.rate = 0.43, cov.model = "exponential", 
                   NNGP = TRUE, n.neighbors = 1, n.burn = 2000, 
                   n.thin = 4, n.chains = 3, verbose = FALSE, k.fold = 9)

ppc.out <- ppcOcc(out_pca, fit.stat = 'freeman-tukey', group =2 )
summary(ppc.out)
waicOcc(out_pca, by.sp = TRUE) # per species

# Predict on whole area
occ_rast_files <- list.files(here("Data", "Occupa"),
                             pattern = "\\.tif$",
                             full.names = TRUE)

print(occ_rast_files)

files_to_exclude<- c("bare_sparse_vege_new_matched_resampled.tif", "builtup_matched_resampled.tif","grassland_matched_resampled.tif","mangroves_matched.tif",
                     "tree_cover_loss_proportion.tif","shrubland_matched.tif", "Fire_count_matched_resampled.tif","dis2protected_resampled.tif",
                     "cropland_matched_resampled.tif","waterbodies_matched_resampled.tif","herbaceous_wetland.tif")
#, "tree_cover_prop.tif","dis2roads_resampled.tif"

occ_rast_files <- occ_rast_files[!basename(occ_rast_files) %in% files_to_exclude]

occ_rast_list <- list()
counter <- 0
for (r in occ_rast_files){
  counter <- counter+1
  occ_rast_list[[counter]] <- rast(r)
}

occ_rast <- rast(occ_rast_list)

occ_df <- as.data.frame(occ_rast, cell = TRUE)

occ_df <- occ_df[complete.cases(occ_df),]

occ_df_final <- as.data.frame(scale(occ_df))

occ_df_final$cell <- occ_df$cell

coords_raster <- xyFromCell(occ_rast, occ_df$cell)
out.pred <- predict(out_pca, X.0=occ_df, coords.0=coords_raster)

mean.psi_final = apply(out.pred$psi.0.samples, 2, mean)
sd.psi = apply(out.pred$psi.0.samples, 2, sd)
print("prediction_done")

# Write predicted occupancy to raster
z <- rast(occ_rast_list[7]) #permeability
#plot(z)
z[!is.na(z)]<-NA

z[occ_df$cell] <- mean.psi_final
#z[z >= 0] <- 1
#z[z > 1] <- NA

plot(z)  
AOI <- vect(here( "northerncluster_campeche_projectsites.geojson"))
plot(AOI,add=T)
roads <- vect(here( "roads.geojson"))
plot(roads,add=T, col="red")
output_filename <- here("Data", "Output_minmax", "Most_species", paste0("predicted_occupancy_species_MEAN_", i, ".tif"))

writeRaster(z, output_filename, overwrite = TRUE)

#predicted sd raster
z <- rast(occ_rast_list[7])
z[occ_df$cell] <- sd.psi
plot(z) 

output_filename_SD <- here("Data", "Output_minmax", "Most_species", paste0("predicted_occupancy_species_SD_", i, ".tif"))

writeRaster(z, output_filename_SD, overwrite = TRUE)

## DEVIANCES
deviance_i <- out_pca$k.fold.deviance
saveRDS(deviance_i, here("Data", "Output_minmax", "Most_species", paste0("deviance_", i, ".RDS")))
