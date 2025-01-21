
library(ggplot2)
library(dplyr)
library(terra)
library(sf)
library(amt)
library(stringr)
library(tidyterra)

# targets::tar_load("tar_connectSSF_list")
# targets::tar_load("tar_msePois_df")
# THETA <- 0.1
# MSEdf <- tar_msePois_df
# connectRasterLocations <- tar_connectSSF_list
# connectTerra <- terra::rast(tar_connectSSF_list[[1]])
# targets::tar_source()
# targets::tar_load("tar_deerData")
# targets::tar_load("tar_akdeLists")
# deerData <- tar_deerData
# akdeLists <- tar_akdeLists

# extract_connectivity_locations <- function(
    # connectlayersAber,
    # connectlayersWessex,
    # MSEdf,
    # deerData,
    # conAvail,
    # nAvail,
    # typeAvial,
    # seed = 2025,
# ){
#
# }


set.seed(seed)

conAvail <- "99%"
nAvail <- 10
typeAvial <- "random"

meanMSE <- MSEdf %>%
  group_by(theta) %>%
  summarise(meanMSE = mean(mse))

bestTheta <- meanMSE[meanMSE$meanMSE == min(meanMSE$meanMSE),]$theta


extractedDataList <- vector("list", length = length(names(akdeLists$sf)))
names(extractedDataList) <- names(akdeLists$sf)
# for(id in names(akdeLists$sf)){
  # id <- names(akdeLists$sf)[8]
  print(id)
  focalDeer <- deerData %>%
    filter(Animal_ID == id) %>%
    st_as_sf(coords = c("x", "y"), remove = FALSE, crs = 27700)

  focalHR <- akdeLists$sf[[id]]
  # focalHR <- listSF[[id]]
  print(focalHR)
  focalHR <- focalHR %>%
    filter(level == conAvail, ci == "est")
  print(focalHR)

  ############### NEEDS UPDATING TO PROPER LOCATIONS
#### needs changing to appropriate location
##########################################


connectTerraAddr <- connectRasterLocations[str_detect(names(connectRasterLocations),
                                                      sub("e-", "e.", as.character(bestTheta)))]
connectTerra <- terra::rast(connectTerraAddr[[1]])

  # if(focalRegion == "Aberdeenshire"){
  #   focalConnect <- terra::rast(connectRasterLocations)
focalConnect <- connectTerra
  # } else {
  #   focalConnect <- terra::rast(connectRasterLocations)
  # }

# }

availPoints <- sf::st_sample(focalHR, size = nrow(focalDeer) * nAvail,
                             type = typeAvial)
availPoints <- st_transform(availPoints, crs = 27700) %>%
  st_coordinates()
# could also be defined as an area surrounding the points
availConnect <- terra::extract(focalConnect, availPoints)
availPoints <- as.data.frame(availPoints) %>%
  mutate(availConnect)
names(availPoints) <- c("x", "y", "connectivity")
availPoints$case_ <- 0
availPoints$area <- "homerange"

availLandscapePoints <- sf::st_sample(st_bbox(connectTerra), size = nrow(focalDeer) * nAvail,
                             type = typeAvial)
st_crs(availLandscapePoints) <- st_crs(focalDeer)
availLandscapePoints <- availLandscapePoints %>%
  st_coordinates()
# could also be defined as an area surrounding the points
availLandscapeConnect <- terra::extract(focalConnect, availLandscapePoints)
availLandscapePoints <- as.data.frame(availLandscapePoints) %>%
  mutate(availLandscapeConnect)
names(availLandscapePoints) <- c("x", "y", "connectivity")
availLandscapePoints$case_ <- NA
availLandscapePoints$area <- "landscape"

usedConnect <- terra::extract(focalConnect, focalDeer)
usedPoints <- focalDeer %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(usedConnect[,2])
names(usedPoints) <- c("x", "y", "connectivity")
usedPoints$case_ <- 1
usedPoints$area <- "knownlocations"

usedAvailPoints <- rbind(availPoints, usedPoints)
usedAvailPoints <- rbind(usedAvailPoints, availLandscapePoints)

usedAvailPoints$id <- id

library(ggridges)

usedAvailPoints %>%
  ggplot() +
  geom_density_ridges(aes(x = connectivity, y = area)) +
  geom_point(aes(x = connectivity, y = area))


# ggplot() +
#   geom_spatraster(data = connectTerra) +
#   # geom_point(data = availPoints, aes(x = x, y = y), colour = "red") +
#   geom_point(data = availLandscapePoints, aes(x = x, y = y)) +
#   # geom_sf(data = focalDeer, aes(x = x, y = y)) +
#   # geom_sf(data = focalHR, alpha = 0.25) +
#   coord_sf(xlim = range(focalDeer$x), ylim = range(focalDeer$y),
#            datum = sf::st_crs(27700))


