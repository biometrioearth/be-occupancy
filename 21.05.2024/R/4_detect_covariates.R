library("here")
library("terra")
library("lubridate")


# Load occupancy rasters into a multilayer brick

det_rast_files <- list.files(here("Data", "Covariates", "Monthly",  "new_detect"),
                             pattern = "\\.tif$",
                             full.names = TRUE)
det_rast_list <- list()
counter <- 0
for (r in det_rast_files){
  counter <- counter+1
  tmpr <- rast(r)
  names(tmpr) <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(r))
  det_rast_list[[counter]] <- tmpr
}

# To multilayer raster.
det_rast <- rast(det_rast_list)
plot(det_rast)
num_layers <- nlyr(det_rast)

# Plot each layer in a loop
for (i in 1:num_layers) {
  plot(det_rast[[i]], main = names(det_rast)[i])
}

# Create raster for lai
rast_lai <- det_rast[[c(7,8,9,10,11,25)]]
plot(rast_lai)
# Create raster for pre
rast_pre <- det_rast[[c(12,13,14,15,16,17)]]
plot(rast_pre)
# Create raster for tem
rast_tem <- det_rast[[c(18,19,20,21,22,23)]]
plot(rast_tem)

# Create raster for fapar
rast_fapar <- det_rast[[c(1,2,3,4,5,6)]]
plot(rast_tem)

# Load our sampling sites.
sampling_sites <- vect("sampling_points.shp")

# Extract by month.
lai_ext <- data.matrix(extract(rast_lai, sampling_sites))
lai_ext <- lai_ext[,-1]
pre_ext <- data.matrix(extract(rast_pre, sampling_sites))
pre_ext <- pre_ext[,-1]
tem_ext <- data.matrix(extract(rast_tem, sampling_sites))
tem_ext <- tem_ext[,-1]
fapar_ext <- data.matrix(extract(rast_fapar, sampling_sites))
fapar_ext <- rast_fapar[,-1]


months <- matrix(0,37,6)
months[,1]<-10
months[,2]<-11
months[,3]<-12
months[,4]<-1
months[,5]<-2
months[,6]<-3
# Save as list
det_covs <- list(lai=lai_ext,
                 pre=pre_ext,
                 tem=tem_ext,
                 fapar=fapar_ext,
                 month=months)

saveRDS(det_covs, "det_covs.RDS")
