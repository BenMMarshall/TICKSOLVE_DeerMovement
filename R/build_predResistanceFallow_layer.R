#' Create raster based on the mean SSF coefs
#'
#' @name build_predResistanceFallow_layer
#' @description abc
#' @return abc
#'
#' @export
build_predResistanceFallow_layer <- function(ssfData, ssfPoismodel, projLayer,
                                             prelimAggFact, inlaFormula = NULL){

  # library(here)
  # library(dplyr)
  # library(survival)
  # library(boot)
  # library(tidyr)
  # library(sjmisc)
  # library(sf)
  # library(terra)
  # library(tidyterra)
  # library(gdistance)
  # library(ggplot2)
  #
  # targets::tar_load("tar_poisFallow_model")
  # targets::tar_load("tar_proj_layer")
  # targets::tar_load("tar_deerData")
  # targets::tar_load("tar_patchList")
  # targets::tar_load("tar_landuseList")
  #
  # ssfPoismodel <- tar_poisFallow_model
  # landuseList <- tar_landuseList
  # patchList <- tar_patchList
  # deerData <- tar_deerData
  # projLayer <- tar_proj_layer

  # REGION <- "Wessex"
  # REGION <- "Aberdeenshire"
  # prelimAggFact <- NA

  # focalData <- ssfData$Roe04_F$steps
  # focalModel <- ssfModels$Roe04_F

  # duplicate landuse raster for reference when rasterising the road data
  projTerra <- terra::rast(projLayer)
  focalRoadsTerra <- projTerra

  roadsWessex_SU <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_SU.gml"),
                            layer = "RoadLink")
  roadsWessex_ST <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_ST.gml"),
                            layer = "RoadLink")
  roadsWessex_SZ <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_SZ.gml"),
                            layer = "RoadLink")
  roadsWessex_SY <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_SY.gml"),
                            layer = "RoadLink")
  roadsWessexCrop_SU <- sf::st_crop(roadsWessex_SU, st_bbox(projTerra))
  roadsWessexCrop_ST <- sf::st_crop(roadsWessex_ST, st_bbox(projTerra)) %>%
    dplyr::select(-name2)
  roadsWessexCrop_SZ <- sf::st_crop(roadsWessex_SZ, st_bbox(projTerra))
  roadsWessexCrop_SY <- sf::st_crop(roadsWessex_SY, st_bbox(projTerra))

  roadsWessexCrop <- rbind(roadsWessexCrop_SU,
                           rbind(roadsWessexCrop_ST,
                                 rbind(roadsWessexCrop_SZ, roadsWessexCrop_SY)))

  roadsWessexCrop <- roadsWessexCrop %>%
    mutate(roadSize = case_when(
      roadFunction == "A Road" ~ "A roads",
      roadFunction == "B Road" ~ "B roads",
      roadFunction %in% c("Local Access Road", "Restricted Local Access Road",
                          "Local Road", "Minor Road", "Secondary Access Road") ~ "C roads",
      TRUE ~ "Other"
    )) %>%
    mutate(roadSize = factor(roadSize,
                             levels = c("A roads", "B roads", "C roads", "Other")))

  poisSumm <- summary(ssfPoismodel)
  poisCoefDF <- as.data.frame(poisSumm$fixed)
  poisCoefDF$term <- rownames(poisCoefDF)
  poisCoef <- poisCoefDF$mean
  names(poisCoef) <- poisCoefDF$term
  roadCrossProb <- plogis(poisCoef[["roadCrossings"]])

  focalRoads <- roadsWessexCrop

  # plot(terra::rasterize(focalRoads, focalRoadsTerra, fun = "mean"))
  ########################################################################################################################
  ######### ROAD BUFFERED TO APPEAR ON AGG LANDSCAPE, CAN BE MINIMISED FOR HIGHER RES LANDSCAPE ##########################
  ########################################################################################################################
  if(is.na(prelimAggFact) | is.null(prelimAggFact)){
    focalRoadsTerra <- terra::rasterize(st_buffer(focalRoads, 2), focalRoadsTerra,
                                        fun = "max", background = 0, touches = TRUE)
  } else {
    focalRoadsTerra <- terra::rasterize(st_buffer(focalRoads, prelimAggFact+2), focalRoadsTerra,
                                        fun = "max", background = 0, touches = TRUE)
  }
  terra::values(focalRoadsTerra) <- ifelse(terra::values(focalRoadsTerra) == 0, 0, roadCrossProb)
  ########################################################################################################################
  ########################################################################################################################
  ########################################################################################################################
  # st_buffer(focalRoads, 10) %>%
  #   ggplot() +
  #   geom_sf()

  # ggplot() +
  #   geom_spatraster(data = focalLanduse, aes(fill = landuse))# +
  # geom_sf(data = focalRoads, alpha = 0.1)

  # plot(terra::cover(focalRoadsTerra, projTerra, values = NA))

  # where conductance is greater than the road permeability, reduce the conductance by the road permeability,
  # if the conductance is already very low due to other factor, retain those low values
  terra::values(projTerra)[which(terra::values(projTerra) > roadCrossProb)] <-
    terra::values(projTerra)[which(terra::values(projTerra) > roadCrossProb)] -
    terra::values(focalRoadsTerra)[which(terra::values(projTerra) > roadCrossProb)]

  ## To avoid problems with 0's in RSP, we set them to a very small value
  values(projTerra) <- ifelse(values(projTerra) == 0, 0.0000000000001, values(projTerra))
  # invert conductance into resistance layer
  values(projTerra) <- 1/values(projTerra)

  names(projTerra) <- "resistance"

  predRasterLoc <- here("data", "GIS data", "predictionTerraFallow.tif")

  terra::writeRaster(projTerra,
                     filename = predRasterLoc,
                     overwrite = TRUE)

  return(predRasterLoc)
  # predictionTerra <- raster::raster(predRasterLoc)

}
