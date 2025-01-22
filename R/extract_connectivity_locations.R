#' Extract data from connectivity layers to compare to movement data
#'
#' @name extract_connectivity_locations
#' @description Extract data from connectivity maps
#' @return A list of available and used dataframes.
#'
#' @export
extract_connectivity_locations <- function(
    connectRasterLocations,
    connectRasterLocationWessex,
    akdeLists,
    deerData,
    MSEdf,
    conAvail,
    nAvail,
    typeAvial,
    seed = 2025
){

  # library(ggplot2)
  # library(dplyr)
  # library(terra)
  # library(sf)
  # library(amt)
  # library(stringr)
  # library(tidyterra)
  # library(ggridges)
  #
  # targets::tar_load("tar_connectPois_list")
  # targets::tar_load("tar_connectPois_locationWessex")
  # targets::tar_load("tar_msePois_df")
  # connectRasterLocations <- tar_connectPois_list
  # connectRasterLocationWessex <- tar_connectPois_locationWessex
  # THETA <- 0.1
  # MSEdf <- tar_msePois_df
  # targets::tar_source()
  # targets::tar_load("tar_deerData")
  # targets::tar_load("tar_akdeLists")
  # deerData <- tar_deerData
  # akdeLists <- tar_akdeLists

  set.seed(seed)

  # conAvail <- "99%"
  # nAvail <- 10
  # typeAvial <- "random"

  meanMSE <- MSEdf %>%
    group_by(theta) %>%
    summarise(meanMSE = mean(mse))

  bestTheta <- meanMSE[meanMSE$meanMSE == min(meanMSE$meanMSE),]$theta


  extractedDataList <- vector("list", length = length(names(akdeLists$sf)))
  names(extractedDataList) <- names(akdeLists$sf)
  for(id in names(akdeLists$sf)){
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

    focalRegion <- deerData[deerData$Animal_ID == id,]$region[1]

    if(focalRegion == "Aberdeenshire"){

      connectTerraAddr <- connectRasterLocations[str_detect(names(connectRasterLocations),
                                                            sub("e-", "e.", as.character(bestTheta)))]

      focalConnect <- terra::rast(here::here("data", "GIS data", tail(str_split(connectTerraAddr, "/")[[1]], 1)))

    } else {

      focalConnect <- terra::rast(here::here("data",
                                             "GIS data",
                                             tail(str_split(connectRasterLocationWessex, "/")[[1]], 1)))
    }

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

    availLandscapePoints <- sf::st_sample(st_bbox(focalConnect), size = nrow(focalDeer) * nAvail,
                                          type = typeAvial)
    st_crs(availLandscapePoints) <- st_crs(focalDeer)
    availLandscapePoints <- availLandscapePoints %>%
      st_coordinates()
    # could also be defined as an area surrounding the points
    availLandscapeConnect <- terra::extract(focalConnect, availLandscapePoints)
    availLandscapePoints <- as.data.frame(availLandscapePoints) %>%
      mutate(availLandscapeConnect)
    names(availLandscapePoints) <- c("x", "y", "connectivity")
    availLandscapePoints$case_ <- 0
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
    usedAvailPoints$region <- focalRegion

    extractedDataList[[id]] <- usedAvailPoints
  }
  usedAvailAll <- do.call(rbind, extractedDataList)

  return(usedAvailAll)

}

