
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


ssfCoefs <- do.call(rbind, lapply(names(ssfModels), function(x){
  mod <- ssfModels[[x]]
  coef <- coef(mod$model)
  conf <- confint(mod$model)
  modelSummary <- cbind(coef, conf) %>%
    as.data.frame()
  modelSummary$Animal_ID <- x
  return(modelSummary)
}))
ssfCoefs$term <- gsub("[[:digit:]]{0,2}$", "", row.names(ssfCoefs))

naiveMeanSsfCoefs <- ssfCoefs %>%
  group_by(term) %>%
  summarise(meanEffect = median(coef, na.rm = TRUE))

ssfMeans <- naiveMeanSsfCoefs$meanEffect
names(ssfMeans) <- naiveMeanSsfCoefs$term

meanSL_ <- mean(unlist(lapply(ssfData, function(x) x$steps$sl_)))
meanTA_ <- mean(unlist(lapply(ssfData, function(x) x$steps$ta_)))


# plot(terra::rasterize(focalRoads, focalRoadsTerra, fun = "mean"))

focalRoadsTerra <- terra::rasterize(st_buffer(focalRoads, 20), focalRoadsTerra,
                                    fun = "max", background = 0, touches = TRUE, cover = TRUE)
terra::values(focalRoadsTerra) <- ifelse(terra::values(focalRoadsTerra) == 0, 0, 1)

# st_buffer(focalRoads, 10) %>%
#   ggplot() +
#   geom_sf()

# remove land use info from the road areas
focalLanduse <- focalLanduse %>%
  mutate(roadCrossings = terra::values(focalRoadsTerra))

ggplot() +
  geom_spatraster(data = focalLanduse, aes(fill = LCM_1))# +
  # geom_sf(data = focalRoads, alpha = 0.1)

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
  dplyr::select(LCM_1, roadCrossings) %>%
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
  dplyr::select(landuse, roadCrossings)#  %>%
# to_dummy()

# colnames(dataLanduse) <- levels(focalLanduse$landuse)[[1]]$label

dataDistance <- as.data.frame(terra::values(focalDistance))

dataMatrix <- cbind(dataLanduse, dataDistance)

dataMatrix <- na.omit(dataMatrix)
# dataMatrix$roadCrossings <- ifelse(terra::values(focalRoadsTerra) == 0, 0, 1)
# dataMatrix <- dataMatrix %>%
#   mutate(roadCrossings =
#            case_when(
#              landuse == "roadCrossings" ~ 1,
#              TRUE ~ 0
#            ),
#          landuse = case_when(
#            landuse == "roadCrossings" ~ "Grasslands",
#            TRUE ~ landuse
#          ))
dataMatrix$step_id_ <- 4
dataMatrix$sl_ <- meanSL_
dataMatrix$ta_ <- meanTA_

# dataMatrix <- dataMatrix %>%
#   mutate(sl_ = case_when(
#     landuse == "Grasslands" ~ ssfMeans["log(sl_)"] * ifelse(is.na(exp(ssfMeans["landuseGrasslands:log(sl_)"])), 1,
#                                                             exp(ssfMeans["landuseGrasslands:log(sl_)"])),
#     landuse == "Heathland" ~ ssfMeans["log(sl_)"] * ifelse(is.na(exp(ssfMeans["landuseHeathland:log(sl_)"])), 1,
#                                                            exp(ssfMeans["landuseHeathland:log(sl_)"])),
#     landuse == "Aquatic" ~ ssfMeans["log(sl_)"] * ifelse(is.na(exp(ssfMeans["landuseAquatic:log(sl_)"])), 1,
#                                                          exp(ssfMeans["landuseAquatic:log(sl_)"])),
#     landuse == "Arable" ~ ssfMeans["log(sl_)"] * ifelse(is.na(exp(ssfMeans["landuseArable:log(sl_)"])), 1,
#                                                         exp(ssfMeans["landuseArable:log(sl_)"])),
#     landuse == "Human Settlements" ~ ssfMeans["log(sl_)"] * ifelse(is.na(exp(ssfMeans["landuseHuman Settlements:log(sl_)"])), 1,
#                                                                    exp(ssfMeans["landuseHuman Settlements:log(sl_)"])),
#     landuse == "Other" ~ ssfMeans["log(sl_)"] * ifelse(is.na(exp(ssfMeans["landuseOther:log(sl_)"])), 1,
#                                                        exp(ssfMeans["landuseOther:log(sl_)"])),
#     TRUE ~ meanSL_
#   ))

hist(focalData$sl_)
hist(dataMatrix$sl_)

# coef(focalModel$model)["sl_"]
# coef(focalModel$model)["log(sl_)"]
# exp(coef(focalModel$model)["landuseGrasslands:log(sl_)"])

modelMatriX <- model.matrix(focalModel$model, dataMatrix)
# sum(modelMatriX[,"roadCrossings"] == 1)
# modelMatriX[,"roadCrossings"] <- dataMatrix$roadCrossings
# predvals <- (modelMatriX %*% coef(focalModel$model)[match(colnames(modelMatriX), names(coef(focalModel$model)))])[,1]
colnames(modelMatriX)[colnames(modelMatriX) == "roadCrossings"] <- "roadCrossingsTRUE"

not_na_inf <- function(x) all(!is.na(x) & !is.infinite(x))
modelMatriX <- modelMatriX %>%
  as.data.frame() %>%
  select_if(not_na_inf) %>%
  as.matrix()

modelMatriX_trim <- modelMatriX[,colnames(modelMatriX) %in% names(ssfMeans[!is.na(ssfMeans)])]
coef_trim <- ssfMeans[names(ssfMeans) %in% colnames(modelMatriX_trim)]

# colnames() bit so that they are ordered the same
predvals <- (modelMatriX_trim %*% coef_trim[colnames(modelMatriX_trim)])[,1]
# predvals <- predvals-4 ##change intercept
## Convert to 0-1 scale
# dataMatrix$pred <- scales::rescale(predvals, to = c(0, 1))
dataMatrix$pred <- inv.logit(predvals)

## Previous analyses showed that animals never used into urban areas, nor do they cross railways
# dataMatrix$pred <- ifelse(d$prop7_lc == 1, 0, d$predi)
# make the woodland highly used and easily moved via
# dataMatrix$pred <- ifelse(dataMatrix$landuse %in% c("Woodland"), 1, dataMatrix$pred)

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
  # plot(pasT)
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
  # geom_sf(data = focalPatches, alpha = 0.1) +
  # geom_sf(data = focalRoads, colour = "#000000", alpha = 0.25) +
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

