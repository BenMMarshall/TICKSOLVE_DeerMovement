#' Read land use and other land data and make rasters
#'
#' @name read_deer_data
#' @description Reads in the UKCEH land use data for the two areas and also OS road data.
#' @return A list of raster file locations
#'
#' @export
prepare_sdm_layer <- function(prelimAggFact = NULL){

  # prelimAggFact <- 10

  if(is.null(prelimAggFact) | is.na(prelimAggFact)){
    prelimAggFact <- 0
  }

  rescale <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}

  landuseGB <- terra::rast(here("data", "GIS data", "UKCEH_Landcover",
                                    "UKCEH_Landcover2023_GB_25res",
                                    "data", "gblcm2023_25m.tif"))

  names(landuseGB) <- c("LCM_1", "LCM_2", "LCM_3")
  # landuseGB <- terra::crop(landuseGB, st_bbox(patchList$GB))

  landuseGB <- landuseGB %>%
    mutate(LCM_1_cat = paste0("LCM_", LCM_1))

  distanceWoodlandGB <- landuseGB %>%
    select(LCM_1) %>%
    filter(LCM_1 %in% 1:2) %>%
    mutate(LCM_1 = case_when(
      LCM_1 %in% 1:2 ~ 1,
      TRUE ~ NA)) %>%
    terra::distance() %>%
    rename(distanceWoodland = LCM_1)

  if(prelimAggFact >= 1 & !is.null(prelimAggFact) & !is.na(prelimAggFact)){
    distanceWoodlandGB <- terra::aggregate(distanceWoodlandGB, fact = prelimAggFact,
                                               fun = "mean")
  }

  hedgerowData <- st_read(here("data", "GIS data", "UKCEH_Hedgerow", "GB_WLF_V1_0.gdb"))

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

  hedgesGB <- st_crop(hedgerowData, landuseGB)
  distanceHedgesGB <- rast(landuseGB)
  if(prelimAggFact >= 1 & !is.null(prelimAggFact) & !is.na(prelimAggFact)){
    distanceHedgesGB <- terra::aggregate(distanceHedgesGB, fact = prelimAggFact,
                                             fun = "mean")
  }
  distanceHedgesGB <- terra::rasterize(st_buffer(hedgesGB, prelimAggFact+2), distanceHedgesGB,
                                           fun = "max", background = NA, touches = TRUE) %>%
    terra::distance() %>%
    rename(distanceHedges = layer)

  print("Hedges complete")

  landuseGB <- landuseGB %>%
    mutate(landuse = factor(case_when(
      LCM_1 %in% 1 ~ "Deciduous Broadleaf Forest",
      LCM_1 %in% 2 ~ "Evergreen Needleleaf Forest",
      LCM_1 %in% 3 ~ "Cropland",
      LCM_1 %in% 4 ~ "Tall Grassland",
      LCM_1 %in% 5:7 ~ "Short Grassland",
      LCM_1 %in% 9:10 ~ "Open Shrubland",
      LCM_1 %in% c(12,15,16,17,18) ~ "Barren",
      LCM_1 %in% c(8,11,19) ~ "Permanent Wetland",
      LCM_1 %in% 20:21 ~ "Human Settlements",
      TRUE ~ "Other"
    ), levels = c(
      "Deciduous Broadleaf Forest",
      "Evergreen Needleleaf Forest",
      "Cropland",
      "Tall Grassland",
      "Short Grassland",
      "Open Shrubland",
      "Barren",
      "Permanent Wetland",
      "Human Settlements",
      "Other"
    )))

  landuseTypes <- unique(landuseGB$landuse)$landuse
  # landuseTypes <- landuseTypes[!landuseTypes == "Other"]

  # pull out OTHER to trim the other layers, remove sea etc
  seaNAcut <- landuseGB %>%
    # invert so we can cut sea and water from other layers
    mutate(landuse = ifelse(!is.na(LCM_1), 1, NA)) %>%
    dplyr::select(landuse)

  if(prelimAggFact >= 1 & !is.null(prelimAggFact) & !is.na(prelimAggFact)){
    seaNAcut <- terra::aggregate(seaNAcut, fact = prelimAggFact,
                                 fun = "max")
  }

  print("Sea NA complete")

  for(land in landuseTypes){
    # land <- "Cropland"
    # land <- "Human Settlements"
    # land <- "Other"

    singleUse <- landuseGB %>%
      filter(landuse == land) %>%
      dplyr::select(landuse) %>%
      mutate(landuse = ifelse(!is.na(landuse), 1, 0))

    if(land == "Human Settlements"){
      distanceHumanSettlementsGB <- singleUse %>%
        mutate(landuse = ifelse(landuse == 0, NA, 1)) %>%
        terra::distance() %>%
        rename(distanceHumanSettlement = landuse)

      if(prelimAggFact >= 1 & !is.null(prelimAggFact) & !is.na(prelimAggFact)){
        distanceHumanSettlementsGB <- terra::aggregate(distanceHumanSettlementsGB, fact = prelimAggFact,
                                                           fun = "mean")
      }

      distanceHumanSettlementsGB <- distanceHumanSettlementsGB * seaNAcut
    }

    names(singleUse) <- gsub(" ", "_", land)

    if(prelimAggFact >= 1 & !is.null(prelimAggFact) & !is.na(prelimAggFact)){
      singleUse <- terra::aggregate(singleUse, fact = prelimAggFact, fun = "min")
    }

    # that will trim the NA areas IE other
    singleUse <- singleUse * seaNAcut

    writeRaster(singleUse,
                filename = here("data", "GIS data", "SDM Layers",
                                paste0("landuseGB_", gsub(" ", "_", land), ".tif")),
                overwrite = TRUE)
  }

  print("Land use complete")

  # distanceRoadsWessexLocation <- here("data", "GIS data", "SDM Layers", "distanceRoadsWessex.tif")
  distanceWoodlandGBLocation <- here("data", "GIS data", "SDM Layers", "distanceWoodlandGB.tif")
  distanceHedgesGBLocation <- here("data", "GIS data", "SDM Layers", "distanceHedgesGB.tif")
  distanceHumanSettlementsGBLocation <- here("data", "GIS data", "SDM Layers", "distanceHumanSettlementsGB.tif")

  # distanceRoadsWessex <- distanceRoadsWessex %>%
  #   crop(singleUse)
  # distanceRoadsWessex <- distanceRoadsWessex * seaNAcut
  # distanceRoadsWessex <- distanceRoadsWessex %>%
  #   mutate(distanceRoads = rescale(distanceRoads))

  distanceWoodlandGB <- distanceWoodlandGB %>%
    crop(singleUse)
  distanceWoodlandGB <- distanceWoodlandGB * seaNAcut
  distanceWoodlandGB <- distanceWoodlandGB %>%
    mutate(distanceWoodland = rescale(distanceWoodland))

  distanceHedgesGB <- distanceHedgesGB %>%
    crop(singleUse)
  distanceHedgesGB <- distanceHedgesGB * seaNAcut
  distanceHedgesGB <- distanceHedgesGB %>%
    mutate(distanceHedges = rescale(distanceHedges))

  distanceHumanSettlementsGB <- distanceHumanSettlementsGB %>%
    crop(singleUse)
  distanceHumanSettlementsGB <- distanceHumanSettlementsGB * seaNAcut
  distanceHumanSettlementsGB <- distanceHumanSettlementsGB %>%
    mutate(distanceHumanSettlement = rescale(distanceHumanSettlement))

  # writeRaster(distanceRoadsWessex,
  #             filename = distanceRoadsWessexLocation, overwrite = TRUE)

  writeRaster(distanceWoodlandGB,
              filename = distanceWoodlandGBLocation, overwrite = TRUE)

  writeRaster(distanceHedgesGB,
              filename = distanceHedgesGBLocation, overwrite = TRUE)

  writeRaster(distanceHumanSettlementsGB,
              filename = distanceHumanSettlementsGBLocation, overwrite = TRUE)

  return(list(
    # distanceRoadsWessexLocation = distanceRoadsWessexLocation,
    distanceWoodlandGBLocation = distanceWoodlandGBLocation,
    distanceHedgesGBLocation = distanceHedgesGBLocation,
    distanceHumanSettlementsGBLocation = distanceHumanSettlementsGBLocation
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
