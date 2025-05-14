#' Create raster based on the mean SSF coefs
#'
#' @name build_predResistance_layer_WREN
#' @description abc
#' @return abc
#'
#' @export
build_predResistance_layer_WREN <- function(ssfData, ssfPoismodel, landuseList, patchList, deerData, REGION,
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
  # targets::tar_load("tar_ssf_models")
  # targets::tar_load("tar_pois_model")
  # targets::tar_load("tar_ssf_data")
  # targets::tar_load("tar_deerData")
  # targets::tar_load("tar_patchList")
  # targets::tar_load("tar_landuseList")
  #
  # ssfData <- tar_ssf_data
  # ssfPoismodel <- tar_ssf_models
  # ssfPoismodel <- tar_pois_model
  # landuseList <- tar_landuseList
  # patchList <- tar_patchList
  # deerData <- tar_deerData

  # REGION <- "Wessex"
  # REGION <- "Aberdeenshire"

  focalPatches <- patchList[[sub("shire", "", REGION)]]

  focalDeer <- deerData %>%
    filter(region == REGION)
  # focalData <- ssfData$Roe04_F$steps
  # focalModel <- ssfModels$Roe04_F

  focalRoads <- landuseList[[sub("shire", "", REGION)]]$roads
  focalDistanceWoodland <- terra::rast(landuseList[[sub("shire", "", REGION)]]$distanceWoodland)
  focalDistanceHedges <- terra::rast(landuseList[[sub("shire", "", REGION)]]$distanceHedges)
  focalLanduse <- terra::rast(landuseList[[sub("shire", "", REGION)]]$landuse)
  # duplicate landuse raster for reference when rasterising the road data
  focalRoadsTerra <- focalLanduse
  focalDeerIDs <- unique(focalDeer$Animal_ID)

  if(!class(ssfPoismodel) == "inla"){

    ssfCoefs <- do.call(rbind, lapply(names(ssfPoismodel), function(x){
      mod <- ssfPoismodel[[x]]
      coef <- coef(mod$model)
      conf <- confint(mod$model)
      modelSummary <- cbind(coef, conf) %>%
        as.data.frame()
      modelSummary$Animal_ID <- x
      return(modelSummary)
    }))
    ssfCoefs$term <- gsub("[[:digit:]]+$", "", row.names(ssfCoefs))

    naiveMeanSsfCoefs <- ssfCoefs %>%
      # can remove deer for other region
      # filter(Animal_ID %in% focalDeerIDs) %>%
      group_by(term) %>%
      summarise(meanEffect = median(coef, na.rm = TRUE))

    ssfMeans <- naiveMeanSsfCoefs$meanEffect
    names(ssfMeans) <- naiveMeanSsfCoefs$term

  } else if(class(ssfPoismodel) == "inla"){

    poisSumm <- summary(ssfPoismodel)
    poisCoefDF <- as.data.frame(poisSumm$fixed)
    poisCoefDF$term <- rownames(poisCoefDF)
    poisCoef <- poisCoefDF$mean
    names(poisCoef) <- poisCoefDF$term

  }

  # get the mean step and turn for focal deer, again region specific
  meanSL_ <- mean(unlist(lapply(ssfData[names(ssfData) %in% focalDeerIDs], function(x) x$steps$sl_)))
  meanlog_SL_ <- mean(log(unlist(lapply(ssfData[names(ssfData) %in% focalDeerIDs], function(x) x$steps$sl_))))
  meanTA_ <- mean(unlist(lapply(ssfData[names(ssfData) %in% focalDeerIDs], function(x) x$steps$ta_)))
  meancosTA_ <- mean(cos(unlist(lapply(ssfData[names(ssfData) %in% focalDeerIDs], function(x) x$steps$ta_))))

  # plot(terra::rasterize(focalRoads, focalRoadsTerra, fun = "mean"))
  ########################################################################################################################
  ######### ROAD BUFFERED TO APPEAR ON AGG LANDSCAPE, CAN BE MINIMISED FOR HIGHER RES LANDSCAPE ##########################
  ########################################################################################################################
  if(is.na(prelimAggFact)){
    focalRoadsTerra <- terra::rasterize(st_buffer(focalRoads, 2), focalRoadsTerra,
                                        fun = "max", background = 0, touches = TRUE)
  } else {
    focalRoadsTerra <- terra::rasterize(st_buffer(focalRoads, prelimAggFact+2), focalRoadsTerra,
                                        fun = "max", background = 0, touches = TRUE)
  }
  terra::values(focalRoadsTerra) <- ifelse(terra::values(focalRoadsTerra) == 0, 0, 1)
  ########################################################################################################################
  ########################################################################################################################
  ########################################################################################################################
  # st_buffer(focalRoads, 10) %>%
  #   ggplot() +
  #   geom_sf()

  # remove land use info from the road areas
  focalLanduse <- focalLanduse %>%
    mutate(roadCrossings = terra::values(focalRoadsTerra))

  # ggplot() +
  #   geom_spatraster(data = focalLanduse, aes(fill = landuse))# +
  # geom_sf(data = focalRoads, alpha = 0.1)

  dataLanduse <- as.data.frame(terra::values(focalLanduse)) %>%
    dplyr::select(landuse, roadCrossings)

  dataDistanceWoodland <- as.data.frame(terra::values(focalDistanceWoodland))
  dataDistanceHedges <- as.data.frame(terra::values(focalDistanceHedges))

  dataMatrix <- cbind(dataLanduse, dataDistanceWoodland)
  dataMatrix <- cbind(dataMatrix, dataDistanceHedges)

  dataMatrix <- na.omit(dataMatrix)
  dataMatrix$step_id_ <- 4
  dataMatrix$sl_ <- meanSL_
  dataMatrix$log_sl <- meanlog_SL_
  dataMatrix$ta_ <- meanTA_
  dataMatrix$cos_ta <- meancosTA_

  dataMatrix <- dataMatrix %>%
    mutate(landuse = as.character(case_when(
      landuse == 1 ~ "Deciduous Broadleaf Forest",
      landuse == 2 ~ "Evergreen Needleleaf Forest",
      landuse == 3 ~ "Cropland",
      landuse == 4 ~ "Tall Grassland",
      landuse == 5 ~ "Short Grassland",
      landuse == 6 ~ "Open Shrubland",
      landuse == 7 ~ "Barren",
      landuse == 8 ~ "Permanent Wetland",
      landuse == 9 ~ "Human Settlements",
      landuse == 10 ~ "Other"
    ))
    )

  # hist(focalData$sl_)
  # hist(dataMatrix$sl_)
  if(!class(ssfPoismodel) == "inla"){

    modelMatriX <- model.matrix(ssfPoismodel[[1]]$model, dataMatrix)
    colnames(modelMatriX)[colnames(modelMatriX) == "roadCrossings"] <- "roadCrossingsTRUE"

    not_na_inf <- function(x) all(!is.na(x) & !is.infinite(x))
    modelMatriX <- modelMatriX %>%
      as.data.frame() %>%
      select_if(not_na_inf) %>%
      as.matrix()

    modelMatriX_trim <- modelMatriX[,colnames(modelMatriX) %in% names(ssfMeans[!is.na(ssfMeans)])]
    coef_trim <- ssfMeans[names(ssfMeans) %in% colnames(modelMatriX_trim)]

    rm(modelMatriX)

  } else if(class(ssfPoismodel) == "inla"){

    landuseNames <- gsub(" ", "\\.", levels(focalLanduse$landuse)[[1]]$label)
    landuseNames <- landuseNames[!landuseNames %in% c("Barren", "Other")]
    landuseWide <- dataMatrix %>%
      to_dummy(landuse)
    names(landuseWide) <- landuseNames
    modelMatriX <- cbind(dataMatrix, landuseWide)

    textForm <- gsub("\n", "", as.character(ssfPoismodel$.args$formula)[3])
    textForm <- str_remove(textForm, " \\+ f\\((.*?$)")
    textForm <- paste0("~ ", textForm)

    modelMatriX_trim <- model.matrix(as.formula(textForm),
                                     data = modelMatriX)

    # modelMatriX_trim <- modelMatriX[,colnames(modelMatriX_trim) %in% names(poisCoef[!is.na(poisCoef)])]
    modelMatriX_trim <- as.matrix(modelMatriX_trim)
    coef_trim <- poisCoef[names(poisCoef) %in% colnames(modelMatriX_trim)]

    rm(modelMatriX)
  }

  # colnames() bit so that they are ordered the same
  predvals <- (modelMatriX_trim %*% coef_trim[colnames(modelMatriX_trim)])[,1]
  ## Convert to 0-1 scale
  dataMatrix$pred <- inv.logit(predvals)

  ## To avoid problems with 0's in RSP, we set them to a very small value
  dataMatrix$pred <- ifelse(dataMatrix$pred == 0, 0.0000000000001, dataMatrix$pred)

  focalLanduse$pred <- dataMatrix$pred

  predictionTerra <- focalLanduse %>%
    dplyr::select(pred)

  ggplot() +
    geom_spatraster(data = predictionTerra, aes(fill = pred)) +
    geom_sf(data = focalRoads, aes(), alpha = 0.2)

  if(!class(ssfPoismodel) == "inla"){

    predRasterLoc <- here("data", "GIS data", paste0("predictionTerraSSF_", sub("shire", "", REGION), ".tif"))

  } else if(class(ssfPoismodel) == "inla"){

    predRasterLoc <- here("data", "GIS data", paste0("predictionTerraPois_", sub("shire", "", REGION), ".tif"))

  }

  terra::writeRaster(predictionTerra,
                     filename = predRasterLoc,
                     overwrite = TRUE)

  return(predRasterLoc)
  # predictionTerra <- raster::raster(predRasterLoc)

}
