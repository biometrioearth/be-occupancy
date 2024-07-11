
library(terra)
library(randomForest)

# datasets
y <- readRDS(here("Data","Processed","video_monthly_detection_array.RDS")) # [50species, 140 sampling points (devices), 6 months]
sampling_sites <- vect(here("Data","Processed","sampling_points_video_monthly.shp"))

# Extract and process covariates
occ.cov <- sampling_sites[, c(  "biomass_m0", "canopyhei0", 
                                "dem_match0", "dist_road0", "fire_coun0", "fire_dist0",
                                 "permeabil0", "protected0", "shrubland0", "slope_mat0", 
                               "tree_cove0", "tree_cove1", "water_dis0", "waterbodi0")]
occ.cov <- as.data.frame(occ.cov)
occ.cov <-scale(occ.cov, scale = FALSE)
# Read detection covariates
det.covs <- readRDS(here("Data","Processed","period_det_covs.RDS"))

##reshape detections
y <- rowSums(y, dims = 2)
y[y>0] <- 1
y<-t(y)
#y_reshaped <- matrix(y, ncol = dim(y)[1], byrow = TRUE)
#y_reshaped <- as.data.frame(y_reshaped)

# Repeat occ.cov to match the length of y_reshaped
#occ.cov_repeated <- occ.cov[rep(seq_len(nrow(occ.cov)), each = dim(y)[3]), ]

# Fit a linear model for each species
fit_list <- list()
for (i in 1:ncol(y)) {
  fit_list[[i]] <- lm(y[, i] ~ ., data = occ.cov)
}

# Calculate permutation importance for each species
#

importance_list <- list()
for (i in 1:ncol(y)) {
  rf_model <- randomForest(x = occ.cov, y = as.factor(y[, i]))
  print(rf_model$confusion)
  importance_list[[i]] <- importance(rf_model)
}

importance_combined <- do.call(cbind, importance_list)

# importance combined (average importance across species)
average_importance <- rowMeans(importance_combined)
View(as.data.frame(average_importance))
