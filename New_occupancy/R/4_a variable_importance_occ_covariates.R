library(terra)
library(randomForest)
library("here")
# Load datasets
y <- readRDS(here("Data", "Processed", "video_weekly_detection_array.RDS")) # [50 species, 140 sampling points (devices), 25 weeks]
sampling_sites <- vect(here("Data", "Processed", "sampling_points_video_weekly.shp"))
initial_columns <- names(sampling_sites)

exclude_columns <- c("ID", "sampling_0")
selected_columns <- setdiff(initial_columns, exclude_columns)
occ_cov <- sampling_sites[, selected_columns]

occ_cov <- as.data.frame(occ_cov)
#occ_cov <- scale(occ_cov, scale = FALSE)

# Remove rows with NA values
complete_cases <- complete.cases(occ_cov)
occ_cov <- occ_cov[complete_cases, ]

# Reshape detections
y <- rowSums(y, dims = 2, na.rm = TRUE)
y[y > 0] <- 1
y <- t(y)
y <- y[complete_cases, ]

# Fit a linear model for each species
fit_list <- list()
for (i in 1:ncol(y)) {
  fit_list[[i]] <- lm(y[, i] ~ ., data = occ_cov)
}

# Calculate variable importance using Random Forest for each species
importance_list <- list()
for (i in 1:ncol(y)) {
  rf_model <- randomForest(x = occ_cov, y = as.factor(y[, i]))
  print(rf_model$confusion)
  importance_list[[i]] <- importance(rf_model)
}

# Combine importance scores and calculate the average importance across species
importance_combined <- do.call(cbind, importance_list)
average_importance <- rowMeans(importance_combined)
View(as.data.frame(average_importance))

###==================================================================================
###Correlation and VIF
##==================================================================================

## select first 10 variables
sorted_importance <- sort(average_importance, decreasing = TRUE)
top_10_variables <- names(sorted_importance)[1:10]
occ.cov <- occ_cov[, top_10_variables]

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

# Calculate the VIF values using the VIFcalc function
vif_results <- VIFcalc(occ.cov_matrix)

# Print the VIF results
print(vif_results)

cor_matrix <- cor(occ.cov_df)

View(as.data.frame(cor_matrix))

##exclude VIF values 10 and above
high_vif_vars <- vif_results$var[vif_results$VIF >= 10]
occ.cov <- occ.cov_df[, !colnames(occ.cov_df) %in% high_vif_vars]
VIFcalc(occ.cov)
cor_matrix <- cor(occ.cov)

View(as.data.frame(cor_matrix))
##=============================================
## stepwise regression
##------------------------

# Fit a stepwise regression model for each species
stepwise_list <- list()
for (i in 1:ncol(y)) {
  # Fit the full model
  full_model <- glm(y[, i] ~ ., data = occ.cov, family = "binomial")
  
  stepwise_model <- step(full_model, direction = "backward")
  stepwise_list[[i]] <- stepwise_model
}
summary(stepwise_list[[1]])


all_selected_vars <- list()

# Loop through each stepwise model and extract the selected variables
for (i in 1:length(stepwise_list)) {
  model_vars <- names(coef(stepwise_list[[i]]))[-1]  # Exclude the intercept
  all_selected_vars <- c(all_selected_vars, model_vars)
}
var_frequency <- table(unlist(all_selected_vars))
var_frequency_sorted <- sort(var_frequency, decreasing = TRUE)
print(var_frequency_sorted)


##=========================================================================================
##Principal Component Analysis
##============================================================================================

occ_cov_matrix <- occ.cov

pca_result <- prcomp(occ_cov_matrix, center = TRUE, scale. = TRUE)

# Summary of PCA results
summary(pca_result)
eigenvalues <- (pca_result$sdev)^2
eigenvalues
proportion_variance <- eigenvalues / sum(eigenvalues)
proportion_variance_percent <- proportion_variance * 100

## occupancy modelling
occ_pca <- pca_result$x[, 1:4]
occ_pca_df <- as.data.frame(occ_pca)
occ.formula_pca <- as.formula(paste("~", paste(names(occ_pca_df), collapse = " + ")))


det.covs <- readRDS(here("Data","Processed","weekly_det_covs.RDS")) #must be data matrix
for (i in seq_along(det.covs)) {
  det.covs[[i]][is.na(det.covs[[i]])] <- 0
}

coords <- geom(sampling_sites)[, c("x", "y")]
coords <- data.matrix(coords)

det.formula <- ~ I(scale(week))+ scale(tem)



y <- readRDS(here("Data","Processed","video_weekly_detection_array.RDS"))
i=1 # species
print(i)

y_i <- y[i,,]

i=1 # species
print(i)

occ_table_pca <- list(y = y_i, occ.covs = occ_pca_df, det.covs = det.covs, coords = coords)


out_pca <- spPGOcc(occ.formula = occ.formula_pca,
                   det.formula = det.formula,
                   data = occ_table_pca, n.batch = 400, batch.length = 25,
                   accept.rate = 0.43, cov.model = "exponential", 
                   NNGP = TRUE, n.neighbors = 1, n.burn = 2000, 
                   n.thin = 4, n.chains = 3, verbose = FALSE, k.fold = 9)

ppc.out <- ppcOcc(out_pca, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.out)
waicOcc(out_pca, by.sp = TRUE) # per species

