#' Read land use and road data
#'
#' @name read_deer_data
#' @description Reads in the UKCEH land use data for the two areas and also OS road data.
#' @return A list of Aberdeen and Wessex land data.
#'
#' @export
read_landuse_data <- function(deerData, patchList, prelimAggFact){

  # targets::tar_load("tar_deerData")
  # deerData <- tar_deerData
  # targets::tar_load("tar_patchList")
  # patchList <- tar_patchList

  sfDeer <- st_as_sf(deerData, coords = c("x","y"),
                     crs = 27700)

  hedgerowData <- st_read(here("data", "GIS data", "UKCEH_Hedgerow", "GB_WLF_V1_0.gdb"))

  # ABERDEEN ----------------------------------------------------------------

  landRastAberdeen <- terra::rast(here("data", "GIS data", "UKCEH_Landcover",
                                       "UKCEH_Landcover2023_ab_25res",
                                       "data", "LCM.tif"))

  landuseAberdeen <- terra::crop(landRastAberdeen, st_bbox(patchList$Aberdeen)) #+
  #c(-2000, -2000, 2000, 2000)

  landuseAberdeen <- landuseAberdeen %>%
    mutate(LCM_1_cat = paste0("LCM_", LCM_1))

  roadsAberdeen_NO <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_NO.gml"),
                              layer = "RoadLink")
  roadsAberdeen_NJ <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_NJ.gml"),
                              layer = "RoadLink")
  roadsAberdeen <- rbind(roadsAberdeen_NO, roadsAberdeen_NJ)
  roadsAberdeenCrop <- sf::st_crop(roadsAberdeen, st_bbox(patchList$Aberdeen))
  roadsAberdeenCrop <- roadsAberdeenCrop %>%
    mutate(roadSize = case_when(
      roadFunction == "A Road" ~ "A roads",
      roadFunction == "B Road" ~ "B roads",
      roadFunction %in% c("Local Access Road", "Restricted Local Access Road",
                          "Local Road", "Minor Road", "Secondary Access Road") ~ "C roads",
      TRUE ~ "Other"
    )) %>%
    mutate(roadSize = factor(roadSize,
                             levels = c("A roads", "B roads", "C roads", "Other")))

  # roadsAberdeenRast <- terra::rast(landuseAberdeen)
  # roadsAberdeenRast <- terra::rasterize(st_buffer(roadsAberdeenCrop, prelimAggFact+2), roadsAberdeenRast,
  #                                        fun = "max", background = NA, touches = TRUE) %>%
  #   terra::distance() %>%
  #   rename(distanceRoads = layer)
  # writeRaster(roadsAberdeenRast,
  #             filename = here("data", "GIS data", "tempExtras", "roadsAberdeenRast.tif"), overwrite = TRUE)
  # st_write(roadsAberdeenCrop, here("data", "GIS data", "tempExtras", "roadsAberdeen.geoJSON"),
  #          driver = "geoJSON", append = FALSE)

  distanceWoodlandAberdeen <- landuseAberdeen %>%
    select(LCM_1) %>%
    filter(LCM_1 %in% 1:2) %>%
    mutate(LCM_1 = case_when(
      LCM_1 %in% 1:2 ~ 1,
      TRUE ~ NA)) %>%
    terra::distance() %>%
    rename(distanceWoodland = LCM_1)

  hedgesAberdeen <- st_crop(hedgerowData, landuseAberdeen)
  hedgesAberdeenRast <- rast(landuseAberdeen)
  if(is.na(prelimAggFact)){
    hedgesAberdeenRast <- terra::rasterize(st_buffer(hedgesAberdeen, 0+2), hedgesAberdeenRast,
                                           fun = "max", background = NA, touches = TRUE) %>%
      terra::distance() %>%
      rename(distanceHedges = layer)
  } else {
    hedgesAberdeenRast <- terra::rasterize(st_buffer(hedgesAberdeen, prelimAggFact+2), hedgesAberdeenRast,
                                           fun = "max", background = NA, touches = TRUE) %>%
      terra::distance() %>%
      rename(distanceHedges = layer)
  }

  landuseAberdeen <- landuseAberdeen %>%
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

  # WESSEX ------------------------------------------------------------------

  landRastWessex <- terra::rast(here("data", "GIS data", "UKCEH_Landcover",
                                     "UKCEH_Landcover2023_nf_25res",
                                     "data", "LCM.tif"))

  landuseWessex <- terra::crop(landRastWessex, st_bbox(patchList$Wessex))

  landuseWessex <- landuseWessex %>%
    mutate(LCM_1_cat = paste0("LCM_", LCM_1))

  distanceWoodlandWessex <- landuseWessex %>%
    select(LCM_1) %>%
    filter(LCM_1 %in% 1:2) %>%
    mutate(LCM_1 = case_when(
      LCM_1 %in% 1:2 ~ 1,
      TRUE ~ NA)) %>%
    terra::distance() %>%
    rename(distanceWoodland = LCM_1)

  roadsWessex_SU <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_SU.gml"),
                            layer = "RoadLink")
  roadsWessex_ST <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_ST.gml"),
                            layer = "RoadLink")
  roadsWessex_SZ <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_SZ.gml"),
                            layer = "RoadLink")
  roadsWessex_SY <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_SY.gml"),
                            layer = "RoadLink")
  roadsWessexCrop_SU <- sf::st_crop(roadsWessex_SU, st_bbox(patchList$Wessex))
  roadsWessexCrop_ST <- sf::st_crop(roadsWessex_ST, st_bbox(patchList$Wessex)) %>%
    dplyr::select(-name2)
  roadsWessexCrop_SZ <- sf::st_crop(roadsWessex_SZ, st_bbox(patchList$Wessex))
  roadsWessexCrop_SY <- sf::st_crop(roadsWessex_SY, st_bbox(patchList$Wessex))

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

  hedgesWessex <- st_crop(hedgerowData, landuseWessex)
  hedgesWessexRast <- rast(landuseWessex)
  if(is.na(prelimAggFact)){
    hedgesWessexRast <- terra::rasterize(st_buffer(hedgesWessex, 0+2), hedgesWessexRast,
                                         fun = "max", background = NA, touches = TRUE) %>%
      terra::distance() %>%
      rename(distanceHedges = layer)
  } else {
    hedgesWessexRast <- terra::rasterize(st_buffer(hedgesWessex, prelimAggFact+2), hedgesWessexRast,
                                         fun = "max", background = NA, touches = TRUE) %>%
      terra::distance() %>%
      rename(distanceHedges = layer)
  }

  landuseWessex <- landuseWessex %>%
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

  # EXPORT -----------------------------------------------------


  writeRaster(landuseAberdeen,
              filename = here("data", "GIS data", "landuseAberdeen.tif"), overwrite = TRUE)

  writeRaster(landuseWessex,
              filename = here("data", "GIS data", "landuseWessex.tif"), overwrite = TRUE)


  writeRaster(distanceWoodlandAberdeen,
              filename = here("data", "GIS data", "distanceWoodlandAberdeen.tif"), overwrite = TRUE)

  writeRaster(distanceWoodlandWessex,
              filename = here("data", "GIS data", "distanceWoodlandWessex.tif"), overwrite = TRUE)


  writeRaster(hedgesAberdeenRast,
              filename = here("data", "GIS data", "distanceHedgesAberdeen.tif"), overwrite = TRUE)

  writeRaster(hedgesWessexRast,
              filename = here("data", "GIS data", "distanceHedgesWessex.tif"), overwrite = TRUE)


  landuseList <- list(
    "Aberdeen" = list(landuse = here("data", "GIS data", "landuseAberdeen.tif"),
                      distanceWoodland = here("data", "GIS data", "distanceWoodlandAberdeen.tif"),
                      distanceHedges = here("data", "GIS data", "distanceHedgesAberdeen.tif"),
                      roads = roadsAberdeenCrop),
    "Wessex" = list(landuse = here("data", "GIS data", "landuseWessex.tif"),
                    distanceWoodland = here("data", "GIS data", "distanceWoodlandWessex.tif"),
                    distanceHedges = here("data", "GIS data", "distanceHedgesWessex.tif"),
                    roads = roadsWessexCrop)
  )

  return(landuseList)

}
