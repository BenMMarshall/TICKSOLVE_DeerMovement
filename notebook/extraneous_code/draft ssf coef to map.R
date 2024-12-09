
library(here)
library(dplyr)
library(survival)
library(boot)
library(tidyr)
library(sjmisc)
library(sf)
library(terra)
library(tidyterra)
library(gdistance)
library(ggplot2)

targets::tar_load("tar_ssf_models")
targets::tar_load("tar_ssf_data")
targets::tar_load("tar_deerData")
targets::tar_load("tar_patchList")
targets::tar_load("tar_landuseList")

ssfData <- tar_ssf_data
ssfModels <- tar_ssf_models
landuseList <- tar_landuseList
patchList <- tar_patchList
deerData <- tar_deerData

focalPatches <- patchList$Aberdeen
focalDeer <- deerData %>%
  filter(Animal_ID == "Roe04_F")
focalData <- ssfData$Roe04_F$steps
focalModel <- ssfModels$Roe04_F
focalRoads <- landuseList$Aberdeen$roads
focalDistance <- terra::rast(landuseList$Aberdeen$distanceWoodland)
focalLanduse <- terra::rast(landuseList$Aberdeen$landuse)
focalRoadsTerra <- focalLanduse
coef(focalModel$model)

plot(terra::rasterize(focalRoads, focalRoadsTerra, fun = "mean"))

focalRoadsTerra <- terra::rasterize(st_buffer(focalRoads, 10), focalRoadsTerra,
                 fun = "max", background = 0, touches = TRUE, cover = TRUE)

# st_buffer(focalRoads, 10) %>%
#   ggplot() +
#   geom_sf()
#
# ggplot() +
#   geom_spatraster(data = focalRoadsTerra) +
#   geom_sf(data = focalRoads, alpha = 0.1)

# focalLanduse <- focalLanduse %>%
#   mutate(landuse = factor(case_when(
#     LCM_1 %in% 1:2 ~ "Woodland",
#     LCM_1 %in% 3 ~ "Arable",
#     LCM_1 %in% 4:7 ~ "Grasslands",
#     LCM_1 %in% 9:10 ~ "Heathland",
#     LCM_1 %in% c(11,14) ~ "Aquatic",
#     LCM_1 %in% 20:21 ~ "Human Settlements",
#     TRUE ~ "Other"
#   ), levels = c(
#     "Woodland",
#     "Grasslands",
#     "Heathland",
#     "Aquatic",
#     "Arable",
#     "Human Settlements",
#     "Other"
#   )))

dataLanduse <- as.data.frame(terra::values(focalLanduse)) %>%
  dplyr::select(LCM_1) %>%
  dplyr::mutate(landuse = factor(case_when(
    LCM_1 %in% 1:2 ~ "Woodland",
    LCM_1 %in% 3 ~ "Arable",
    LCM_1 %in% 4:7 ~ "Grasslands",
    LCM_1 %in% 9:10 ~ "Heathland",
    LCM_1 %in% c(11,14) ~ "Aquatic",
    LCM_1 %in% 20:21 ~ "Human Settlements",
    TRUE ~ "Other"
  ), levels = c(
    "Woodland",
    "Grasslands",
    "Heathland",
    "Aquatic",
    "Arable",
    "Human Settlements",
    "Other"
  ))) %>%
  dplyr::select(landuse)#  %>%
  # to_dummy()

# colnames(dataLanduse) <- levels(focalLanduse$landuse)[[1]]$label

dataDistance <- as.data.frame(terra::values(focalDistance))

dataMatrix <- cbind(dataLanduse, dataDistance)

dataMatrix$roadCrossings <- terra::values(focalRoadsTerra)
dataMatrix <- na.omit(dataMatrix)
dataMatrix$step_id_ <- 4
dataMatrix$sl_ <- mean(focalData$sl_)
dataMatrix$ta_ <- mean(focalData$ta_)

dataMatrix <- dataMatrix %>%
  mutate(sl_ = case_when(
    landuse == "Grasslands" ~ sl_ * ifelse(is.na(exp(coef(focalModel$model)["landuseGrasslands:log(sl_)"])), 1,
                                           exp(coef(focalModel$model)["landuseGrasslands:log(sl_)"])),
    landuse == "Heathland" ~ sl_ * ifelse(is.na(exp(coef(focalModel$model)["landuseHeathland:log(sl_)"])), 1,
                                          exp(coef(focalModel$model)["landuseHeathland:log(sl_)"])),
    landuse == "Aquatic" ~ sl_ * ifelse(is.na(exp(coef(focalModel$model)["landuseAquatic:log(sl_)"])), 1,
                                        exp(coef(focalModel$model)["landuseAquatic:log(sl_)"])),
    landuse == "Arable" ~ sl_ * ifelse(is.na(exp(coef(focalModel$model)["landuseArable:log(sl_)"])), 1,
                                       exp(coef(focalModel$model)["landuseArable:log(sl_)"])),
    landuse == "Human Settlements" ~ sl_ * ifelse(is.na(exp(coef(focalModel$model)["landuseHuman Settlements:log(sl_)"])), 1,
                                                  exp(coef(focalModel$model)["landuseHuman Settlements:log(sl_)"])),
    landuse == "Other" ~ sl_ * ifelse(is.na(exp(coef(focalModel$model)["landuseOther:log(sl_)"])), 1,
                                      exp(coef(focalModel$model)["landuseOther:log(sl_)"])),
    TRUE ~ mean(focalData$sl_)
  ))

hist(focalData$sl_)
hist(dataMatrix$sl_)

# coef(focalModel$model)["sl_"]
# coef(focalModel$model)["log(sl_)"]
# exp(coef(focalModel$model)["landuseGrasslands:log(sl_)"])

modelMatriX <- model.matrix(focalModel$model, dataMatrix)

# predvals <- (modelMatriX %*% coef(focalModel$model)[match(colnames(modelMatriX), names(coef(focalModel$model)))])[,1]

not_na_inf <- function(x) all(!is.na(x) & !is.infinite(x))

modelMatriX <- modelMatriX %>%
  as.data.frame() %>%
  select_if(not_na_inf) %>%
  as.matrix()

modelMatriX_trim <- modelMatriX[,colnames(modelMatriX) %in% names(coef(focalModel$model)[!is.na(coef(focalModel$model))])]

coef_trim <- coef(focalModel$model)[names(coef(focalModel$model)) %in% colnames(modelMatriX_trim)]
colnames(modelMatriX_trim)

predvals <- (modelMatriX_trim %*% coef_trim)[,1]
predvals <- predvals-4 ##change intercept
## Convert to 0-1 scale
dataMatrix$pred <- inv.logit(predvals)
## Previous analyses showed that animals never used into urban areas, nor do they cross railways
# dataMatrix$pred <- ifelse(d$prop7_lc == 1, 0, d$predi)
# make the woodland highly used and easily moved via
dataMatrix$pred <- ifelse(dataMatrix$landuse %in% 1:2, 1, dataMatrix$pred)

## To avoid problems with 0's in RSP, we set them to a very small value
dataMatrix$pred <- ifelse(dataMatrix$pred == 0, 0.0000000000001, dataMatrix$pred)

focalLanduse$pred <- dataMatrix$pred

predictionTerra <- focalLanduse %>%
  dplyr::select(pred)

# ## Place back the predicted values in the raster
# keep$pred[d$order] <- dataMatrix$pred
# values(predMap) <- keep$pred
# ## Verify prediction map
ggplot() +
  geom_spatraster(data = predictionTerra, aes(fill = pred)) +
  geom_sf(data = focalRoads, aes(), alpha = 0.2)

terra::writeRaster(predictionTerra,
                   filename = here("data", "GIS data", "predictionTerra.tif"), overwrite = TRUE)

predictionTerra <- raster::raster(here("data", "GIS data", "predictionTerra.tif"))

## Agregate pixels to speed up the process during preliminary analyses. No not aggregate to reproduce the high resolution map illustrate in the paper
rtr <- raster::aggregate(predictionTerra, fact = 20, fun = mean)

terra::writeRaster(rtr,
                   filename = here("data", "GIS data", "predictionTerra_agg20.tif"), overwrite = TRUE)

rtr <- raster::raster(here("data", "GIS data", "predictionTerra_agg20.tif"))

plot(rtr)

## Make a transition map (format needed by gdistance to represent the conductance map, i.e. 1/friction)
tr <- transition(rtr, transitionFunction = min, directions = 8) # conductance between pixels = 1/friction map
tr #class TransitionLayer
plot(raster(tr))

tr <- geoCorrection(tr, multpl = FALSE, scl = TRUE)

# Create the random points (here, 5 random points for each polygon)
set.seed(2024)

focalPatches <- focalPatches %>%
  filter(!duplicated(Ptch_ID))

startPoints <- st_sample(focalPatches, size = c(1,1), type = "random")
endPoints <- st_sample(focalPatches, size = c(1,1), type = "random")

# focalPatches %>%
#   dplyr::select(Ptch_ID) %>%
focalPatches %>%
  dplyr::select(Ptch_ID) %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = startPoints, colour = "green") +
  geom_sf(data = endPoints, colour = "red")

nPatches <- nrow(focalPatches)

pas <- NULL
set.seed(0)
sttim <- Sys.time()
for (i in c(1:nPatches)){
  # i <- 2
  print(paste(i, "/", nPatches))
  # pt_win <- sample(rndpts$id, size=1, prob = (rndpts$val_win)) #draw random points (random pairs)
  # pt_cal <- sample(rndpts$id, size=1, prob = (rndpts$val_cal))
  # pt_win <- xyFromCell(kud_win, pt_win) #get xy for each point
  # pt_cal <- xyFromCell(kud_cal, pt_cal)
  stPoint <- as(sample(startPoints, size = 1), "Spatial")
  edPoint <- as(sample(endPoints, size = 1), "Spatial")
  # make map
  pasT <- try(passage(tr, stPoint, edPoint, theta = 0.001), silent = FALSE)
  # NB: to obtain the different models described in the paper, change theta values as described in the
  # paper. For simplicity here we produce only one model
  if (class(pasT)!="try-error"){
    if (mean(values(pasT)<Inf, na.rm=T)>0.1){
      if (is.null(pas)) pas <- pasT
      if (!is.null(pas)) pas <- pas + pasT
    }}
}
(endtim <- Sys.time()-sttim)
# par(mfrow=c(1,2))
# plot(raster(tr), col=grey(c(100:0)/100))
# plot(pas, col=grey(c(100:0)/100))

pas2 <- NULL
set.seed(0)
sttim <- Sys.time()
for (i in c(1:nPatches)){
  # i <- 2
  print(paste(i, "/", nPatches))
  # pt_win <- sample(rndpts$id, size=1, prob = (rndpts$val_win)) #draw random points (random pairs)
  # pt_cal <- sample(rndpts$id, size=1, prob = (rndpts$val_cal))
  # pt_win <- xyFromCell(kud_win, pt_win) #get xy for each point
  # pt_cal <- xyFromCell(kud_cal, pt_cal)
  stPoint <- as(sample(startPoints, size = 1), "Spatial")
  edPoint <- as(sample(endPoints, size = 1), "Spatial")
  # make map
  pasT <- try(passage(tr, stPoint, edPoint, theta = 0.0001), silent = FALSE)
  # NB: to obtain the different models described in the paper, change theta values as described in the
  # paper. For simplicity here we produce only one model
  if (class(pasT)!="try-error"){
    if (mean(values(pasT)<Inf, na.rm=T)>0.1){
      if (is.null(pas2)) pas2 <- pasT
      if (!is.null(pas2)) pas2 <- pas2 + pasT
    }}
}
(endtim <- Sys.time()-sttim)
# par(mfrow=c(1,2))
# plot(raster(tr), col=grey(c(100:0)/100))
# plot(pas2, col=grey(c(100:0)/100))

par(mfrow=c(1,2))
plot(pas, col=grey(c(100:0)/100))
plot(pas2, col=grey(c(100:0)/100))

pas2_terra <- as(pas2, "SpatRaster")

ggplot() +
  geom_spatraster(data = pas2_terra, aes(fill = layer)) +
  geom_sf(data = focalPatches, alpha = 0.1) +
  geom_sf(data = focalRoads, colour = "#000000") +
  coord_sf(xlim = c(max(c(ext(focalPatches)[1],
                          ext(pas2_terra)[1])),
                    min(c(ext(focalPatches)[2],
                          ext(pas2_terra)[2]))),
           ylim = c(max(c(ext(focalPatches)[3],
                          ext(pas2_terra)[3])),
                    min(c(ext(focalPatches)[4],
                          ext(pas2_terra)[4]))),
           expand = 0)


### NEED dBBMMs to calibrate the model

library(move)

# As our most infrequent tracking is 168 hours (1 week), we will set the
# window to the number of data points collected over 168 hours, and a margin
# of 48 hours. In cases where that is not possible set window and margin to minimum
windowSize <- 29 #~ a week
marginSize <- 5 #~ a day

crs27700 <- sp::CRS(SRS_string = "EPSG:27700")

moveObj <- move(x = focalDeer$x, y = focalDeer$y,
                      time = focalDeer$datetime,
                      proj = crs27700@projargs)

# set_grid.ext <- 4
# set_dimsize <- 640
dbbmm <- brownian.bridge.dyn(object = moveObj,
                             raster = raster::raster(here("data", "GIS data", "predictionTerra.tif")),
                             location.error = 0.1,
                             # ext = set_grid.ext,
                             # dimSize = set_dimsize,
                             window.size = windowSize,
                             margin = marginSize)


values(dbbmm) <- values(dbbmm)/sum(values(dbbmm), na.rm = TRUE)

resampledDBBMM <- resample(dbbmm, raster::raster(here("data", "GIS data", "predictionTerra_agg20.tif")))
values(pas) <- ifelse(is.na(values(pas)), 0, values(pas))
values(pas) <- values(pas)/sum(values(pas), na.rm=T) # rescale pas model
sum(values(pas)) #1
MSE <- mean((values(resampledDBBMM)-values(pas))^2) ## for each pixel, calculate the Mean Square Error MSE. Compute the results for different models and compare
MSE

values(pas2) <- ifelse(is.na(values(pas2)), 0, values(pas2))
values(pas2) <- values(pas)/sum(values(pas2), na.rm=T) # rescale pas model
sum(values(pas2)) #1
MSE <- mean((values(resampledDBBMM)-values(pas2))^2) ## for each pixel, calculate the Mean Square Error MSE. Compute the results for different models and compare
MSE

