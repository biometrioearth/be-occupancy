library("here")
library("terra")
library("lubridate")

# We need:

# 3
# hbef2015$det.covs 
# it's a list with the detectibility covariates
# covariates * [[sampling surveys, sampling sites]]
# In this case [2] * [3, 373]

# Load occupancy rasters into a multilayer brick
det_rast_files <- list.files(here("spOccupancy", "data", "det"),
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

# Create raster for lai
rast_lai <- det_rast[[c(1,2,3)]]
plot(rast_lai)
# Create raster for pre
rast_pre <- det_rast[[c(4,5,6)]]
plot(rast_pre)
# Create raster for tem
rast_tem <- det_rast[[c(7,8,9)]]
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

months <- matrix(0,9,3)
months[,1]<-12
months[,2]<-11
months[,3]<-10

# Save as list
det_covs <- list(lai=lai_ext,
                 pre=pre_ext,
                 tem=tem_ext,
                 month=months)

saveRDS(det_covs, "det_covs.RDS")
