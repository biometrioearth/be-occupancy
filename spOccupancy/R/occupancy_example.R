## preprocessing the data

##Occupancy
library(spOccupancy)
data(hbef2015)

# What do we need to fit a model?

# 1
# hbef2015$y has the sp. detections
# it's a 3 dimensional array
# [species, sampling sites, sampling surveys]
# In this case [12, 373, 3]

# 2
# hbef2015$occ.cov
# it's a matrix-array with the covariates for the occupancy model
# [sampling sites, covariates]
# In this case [373, 1]

# 3
# hbef2015$det.covs 
# it's a list with the detectibility covariates
# covariates * [[sampling surveys, sampling sites]]
# In this case [2] * [3, 373]

# 4 
# hbef2015$coords
# Matrix array with sites coordinates

class(hbef2015)

hbef2015$coords


head(hbef2015$det.covs)
class(hbef2015$occ.covs)
sp.names <- dimnames(hbef2015$y)[[1]]
btbwHBEF <- hbef2015
btbwHBEF$y <- btbwHBEF$y[sp.names == "BTBW", , ]

Elevation

# Specify model formulas
btbw.occ.formula <- ~ scale(Elevation) + I(scale(Elevation)^2)
btbw.det.formula <- ~ scale(day) + scale(tod) + I(scale(day)^2)
# Run the model
out <- spPGOcc(occ.formula = btbw.occ.formula,
               det.formula = btbw.det.formula,
               data = btbwHBEF, n.batch = 400, batch.length = 25,
               accept.rate = 0.43, cov.model = "exponential", 
               NNGP = TRUE, n.neighbors = 5, n.burn = 2000, 
               n.thin = 4, n.chains = 3, verbose = FALSE, k.fold = 2)
summary(out)
ppc.out <- ppcOcc(out, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.out)