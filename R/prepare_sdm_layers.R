#' Read land use and other land data and make rasters
#'
#' @name prepare_sdm_layer
#' @description Reads in the UKCEH land use data
#' @return A list of raster file locations
#'
#' @export
prepare_sdm_layer <- function(prelimAggFact = NULL){

  # library(here)
  # library(sf)
  # library(dplyr)
  # library(terra)
  # library(tidyterra)
  # prelimAggFact <- 10

  if(is.null(prelimAggFact) | is.na(prelimAggFact)){
    prelimAggFact <- 0
  }

  rescale <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}

  landuseGB <- terra::rast(here("data", "GIS data", "UKCEH_Landcover",
                                "UKCEH_Landcover2023_GB_25res",
                                "data", "gblcm2023_25m.tif"))

  names(landuseGB) <- c("LCM_1", "LCM_2", "LCM_3")
  landuseGB <- landuseGB %>%
    dplyr::select(LCM_1)
  # landuseGB <- terra::crop(landuseGB, st_bbox(patchList$GB))

  # pull out OTHER to trim the other layers, remove sea etc
  seaNAcut <- classify(landuseGB,
                       rcl = matrix(c(1, 21, 1), nrow = 1, ncol = 3))

  if(prelimAggFact >= 1 & !is.null(prelimAggFact) & !is.na(prelimAggFact)){
    seaNAcut <- terra::aggregate(seaNAcut, fact = prelimAggFact,
                                 fun = "max")
  }
  print("Sea NA complete")

  # WOODLAND ----------------------------------------------------------------
  distanceWoodlandGBLocation <- here("data", "GIS data", "SDM Layers", "distanceWoodlandGB.tif")

  if(!distanceWoodlandGBLocation %in% list.files(here("data", "GIS data", "SDM Layers"), full.names = TRUE)){

    binaryWoodlandGB <- landuseGB
    binaryWoodlandGB <- subst(binaryWoodlandGB, 1, 1)
    binaryWoodlandGB <- subst(binaryWoodlandGB, 2, 1)
    binaryWoodlandGB <- subst(binaryWoodlandGB, NA, 9999)
    binaryWoodlandGB <- classify(binaryWoodlandGB,
                                 rcl = matrix(c(2, 21, NA), nrow = 1, ncol = 3))

    if(prelimAggFact >= 1 & !is.null(prelimAggFact) & !is.na(prelimAggFact)){
      binaryWoodlandGB <- terra::aggregate(binaryWoodlandGB, fact = prelimAggFact,
                                           fun = "mean")
    }

    # landuseGB <- terra::crop(landuseGB, st_bbox(patchList$GB))
    distanceWoodlandGB <- terra::distance(binaryWoodlandGB, exclude = 9999)
    names(distanceWoodlandGB) <- "distanceWoodland"

    distanceWoodlandGB <- terra::crop(distanceWoodlandGB, st_bbox(c(xmin = 30000,
                                                                    xmax = 680000,
                                                                    ymin = 0,
                                                                    ymax = 1065000),
                                                                  crs = st_crs(27700)))

    writeRaster(distanceWoodlandGB,
                filename = distanceWoodlandGBLocation, overwrite = TRUE)

    print("Woodland complete")
  } else {
    print("Woodland already present")
  }

  # HEDGES ------------------------------------------------------------------
  distanceHedgesGBLocation <- here("data", "GIS data", "SDM Layers", "distanceHedgesGB.tif")

  if(!distanceHedgesGBLocation %in% list.files(here("data", "GIS data", "SDM Layers"), full.names = TRUE)){

    # grassDir <- "C:/Program Files/GRASS GIS 8.4" # Windows
    # fasterRaster::faster(grassDir = grassDir)

    # hedgerowData <- st_read(here("data", "GIS data", "UKCEH_Hedgerow", "GB_WLF_V1_0.gdb"))
    # hedgerowFast <- fasterRaster::fast(hedgerowData)

    # distanceHedgesGB <- fasterRaster::fast(landuseGB)
    # width in meters because CRS is projected
    # distanceHedgesGB <- fasterRaster::distance(distanceHedgesGB, hedgerowFast)
    # names(distanceHedgesGB) <- "distanceHedges"

    distanceHedgesGB <- rast(here("data", "GIS data", "UKCEH_Hedgerow", "hedgerowDistance.tif"))
    distanceHedgesGB <- extend(distanceHedgesGB, ext(c(-2000, 8e+05, -2000, 1600000)), fill = NA)
    distanceHedgesGB <- resample(distanceHedgesGB, seaNAcut)

    distanceHedgesGB_naTrim <- distanceHedgesGB*seaNAcut

    distanceHedgesGB_naTrim <- terra::crop(distanceHedgesGB_naTrim, st_bbox(c(xmin = 30000,
                                                                              xmax = 680000,
                                                                              ymin = 0,
                                                                              ymax = 1065000),
                                                                            crs = st_crs(27700)))
    names(distanceHedgesGB_naTrim) <- "distanceHedgerow"

    writeRaster(distanceHedgesGB_naTrim,
                filename = distanceHedgesGBLocation, overwrite = TRUE)

    print("Hedges complete")
  } else {
    print("Hedges already present")
  }

  # HUMAN SETTLEMTENTS ------------------------------------------------------

  distanceHumanSettlementGBLocation <- here("data", "GIS data", "SDM Layers", "distanceHumanSettlementGB.tif")

  if(!distanceHumanSettlementGBLocation %in% list.files(here("data", "GIS data", "SDM Layers"), full.names = TRUE)){

    binaryHumanSettlementGB <- landuseGB
    binaryHumanSettlementGB <- subst(binaryHumanSettlementGB, NA, 9999)
    binaryHumanSettlementGB <- classify(binaryHumanSettlementGB,
                                        rcl = matrix(c(1, 19, NA), nrow = 1, ncol = 3))
    binaryHumanSettlementGB <- subst(binaryHumanSettlementGB, 20, 1)
    binaryHumanSettlementGB <- subst(binaryHumanSettlementGB, 21, 1)

    if(prelimAggFact >= 1 & !is.null(prelimAggFact) & !is.na(prelimAggFact)){
      binaryHumanSettlementGB <- terra::aggregate(binaryHumanSettlementGB, fact = prelimAggFact,
                                                  fun = "mean")
    }

    # landuseGB <- terra::crop(landuseGB, st_bbox(patchList$GB))
    distanceHumanSettlementGB <- terra::distance(binaryHumanSettlementGB, exclude = 9999)
    names(distanceHumanSettlementGB) <- "distanceHumanSettlement"

    distanceHumanSettlementGB <- terra::crop(distanceHumanSettlementGB, st_bbox(c(xmin = 30000,
                                                                                  xmax = 680000,
                                                                                  ymin = 0,
                                                                                  ymax = 1065000),
                                                                                crs = st_crs(27700)))

    writeRaster(distanceHumanSettlementGB,
                filename = distanceHumanSettlementGBLocation, overwrite = TRUE)

    print("HumanSettlement complete")
  } else {
    print("HumanSettlement already present")
  }

  # BINARY LAND USE ---------------------------------------------------------

  landUseDF <- data.frame(LCM_1 = 1:21) %>%
    mutate(landuse = factor(case_when(
      LCM_1 %in% 1 ~ "Deciduous Broadleaf Forest",
      LCM_1 %in% 2 ~ "Evergreen Needleleaf Forest",
      LCM_1 %in% 3 ~ "Cropland",
      LCM_1 %in% 4 ~ "Tall Grassland",
      LCM_1 %in% 5:7 ~ "Short Grassland",
      LCM_1 %in% 9:10 ~ "Open Shrubland",
      LCM_1 %in% c(12,15,16,17,18) ~ "Barren",
      LCM_1 %in% c(8,11,19) ~ "Permanent Wetland",
      LCM_1 %in% 20:21 ~ "Human Settlement",
      TRUE ~ "Other"
    )))

  landuseTypes <- landUseDF$landuse

  for(land in landuseTypes){

    saveFile <- here("data", "GIS data", "SDM Layers",
                     paste0("landuseGB_", gsub(" ", "_", land), ".tif"))

    if(!saveFile %in% list.files(here("data", "GIS data", "SDM Layers"), full.names = TRUE)){
      # land <- "Barren"
      # land <- "Human Settlements"
      # land <- "Other"
      binaryLandUseGB <- landuseGB
      # binaryLandUseGB <- subst(binaryLandUseGB, NA, 9999)

      luNumbers <- landUseDF$LCM_1[landUseDF$landuse == land]
      reclassMat <- matrix(c(1:21, rep(0, 21)), ncol = 2)
      reclassMat[reclassMat[,1] %in% luNumbers,2] <- 1

      binaryLandUseGB <- classify(binaryLandUseGB,
                                  rcl = reclassMat)

      if(land == "Human Settlement"){
        {next}
      }

      names(binaryLandUseGB) <- gsub(" ", "_", land)

      if(prelimAggFact >= 1 & !is.null(prelimAggFact) & !is.na(prelimAggFact)){
        binaryLandUseGB <- terra::aggregate(binaryLandUseGB, fact = prelimAggFact, fun = "min")
      }

      binaryLandUseGB <- terra::crop(binaryLandUseGB, st_bbox(c(xmin = 30000,
                                                                xmax = 680000,
                                                                ymin = 0,
                                                                ymax = 1065000),
                                                              crs = st_crs(27700)))

      writeRaster(binaryLandUseGB,
                  filename = saveFile,
                  overwrite = TRUE)

      print(paste0(land, " complete"))
    } else {
      print(paste0(land, " already present"))
    }
  }

  print("Land use complete")

  # ROADS -------------------------------------------------------------------

  # distanceRoadsWessexLocation <- here("data", "GIS data", "SDM Layers", "distanceRoadsWessex.tif")

  # roadsWessex_SU <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_SU.gml"),
  #                           layer = "RoadLink")
  # roadsWessex_ST <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_ST.gml"),
  #                           layer = "RoadLink")
  # roadsWessex_SZ <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_SZ.gml"),
  #                           layer = "RoadLink")
  # roadsWessex_SY <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_SY.gml"),
  #                           layer = "RoadLink")
  # roadsWessexCrop_SU <- sf::st_crop(roadsWessex_SU, st_bbox(landuseGB))
  # roadsWessexCrop_ST <- sf::st_crop(roadsWessex_ST, st_bbox(landuseGB)) %>%
  #   dplyr::select(-name2)
  # roadsWessexCrop_SZ <- sf::st_crop(roadsWessex_SZ, st_bbox(landuseGB))
  # roadsWessexCrop_SY <- sf::st_crop(roadsWessex_SY, st_bbox(landuseGB))
  #
  # roadsWessexCrop <- rbind(roadsWessexCrop_SU,
  #       rbind(roadsWessexCrop_ST,
  #             rbind(roadsWessexCrop_SZ, roadsWessexCrop_SY)))
  #
  # roadsWessexCrop <- roadsWessexCrop %>%
  #   mutate(roadSize = case_when(
  #     roadFunction == "A Road" ~ "A roads",
  #     roadFunction == "B Road" ~ "B roads",
  #     roadFunction %in% c("Local Access Road", "Restricted Local Access Road",
  #                         "Local Road", "Minor Road", "Secondary Access Road") ~ "C roads",
  #     TRUE ~ "Other"
  #   )) %>%
  #   mutate(roadSize = factor(roadSize,
  #                            levels = c("A roads", "B roads", "C roads", "Other")))
  #
  # distanceRoadsWessex <- terra::rast(landuseGB)
  # if(prelimAggFact >= 1 & !is.null(prelimAggFact) & !is.na(prelimAggFact)){
  #   distanceRoadsWessex <- terra::aggregate(distanceRoadsWessex, fact = prelimAggFact,
  #                                           fun = "mean")
  # }
  # distanceRoadsWessex <- terra::rasterize(st_buffer(roadsWessexCrop, prelimAggFact+2), distanceRoadsWessex,
  #                                         fun = "max", background = NA, touches = TRUE) %>%
  #   terra::distance() %>%
  #   rename(distanceRoads = layer)
  #
  # print("Roads complete")


  # distanceRoadsWessex <- distanceRoadsWessex %>%
  #   crop(singleUse)
  # distanceRoadsWessex <- distanceRoadsWessex * seaNAcut
  # distanceRoadsWessex <- distanceRoadsWessex %>%
  #   mutate(distanceRoads = rescale(distanceRoads))

  # distanceWoodlandGB <- distanceWoodlandGB %>%
  #   crop(singleUse)
  # distanceWoodlandGB <- distanceWoodlandGB * seaNAcut
  # distanceWoodlandGB <- distanceWoodlandGB %>%
  #   mutate(distanceWoodland = rescale(distanceWoodland))
  #
  # distanceHedgesGB <- distanceHedgesGB %>%
  #   crop(singleUse)
  # distanceHedgesGB <- distanceHedgesGB * seaNAcut
  # distanceHedgesGB <- distanceHedgesGB %>%
  #   mutate(distanceHedges = rescale(distanceHedges))
  #
  # distanceHumanSettlementsGB <- distanceHumanSettlementsGB %>%
  #   crop(singleUse)
  # distanceHumanSettlementsGB <- distanceHumanSettlementsGB * seaNAcut
  # distanceHumanSettlementsGB <- distanceHumanSettlementsGB %>%
  #   mutate(distanceHumanSettlement = rescale(distanceHumanSettlement))

  # writeRaster(distanceRoadsWessex,
  #             filename = distanceRoadsWessexLocation, overwrite = TRUE)

  return(list(
    # distanceRoadsWessexLocation = distanceRoadsWessexLocation,
    distanceWoodlandGBLocation = distanceWoodlandGBLocation,
    distanceHedgesGBLocation = distanceHedgesGBLocation,
    distanceHumanSettlementGBLocation = distanceHumanSettlementGBLocation
  ))

}

#' @export
read_stack_layers <- function(layerLoc = here("data", "GIS data", "SDM Layers"), ...){

  sdmFiles <- list.files(layerLoc, pattern = ".tif")

  i <- 0
  for(f in sdmFiles){
    i <- i+1
    # f <- sdmFiles[1]
    readFile <- rast(here("data", "GIS data", "SDM Layers", f))
    print(f)
    # print(st_bbox(readFile))
    # assign(paste0("layer_", names(readFile)), readFile)
    if(i == 1){
      explLayers <- readFile
    } else {
      explLayers <- c(explLayers, readFile)
    }
  }

  return(explLayers)
}
