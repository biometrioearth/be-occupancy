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
