#' Read land use and road data
#'
#' @name read_deer_data
#' @description Reads in the UKCEH land use data for the two areas and also OS road data.
#' @return A list of Aberdeen and Wessex land data.
#'
#' @export
read_landuse_data <- function(deerData){

  # targets::tar_load("tar_deerData")
  # deerData <- tar_deerData

  sfDeer <- st_as_sf(deerData, coords = c("x","y"),
                     crs = 27700)

  landRastAberdeen <- terra::rast(here("data", "GIS data", "UKCEH_Landcover",
                                       "UKCEH_Landcover2023_ab",
                                       "data", "LCM.tif"))

  landAberdeen <- terra::crop(landRastAberdeen, st_bbox(sfDeer %>%
                                                   filter(region == "Aberdeenshire")) +
                         c(-2000, -2000, 2000, 2000)
  )

  roadsAberdeen_NO <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_NO.gml"),
                              layer = "RoadLink")
  roadsAberdeen_NJ <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_NJ.gml"),
                              layer = "RoadLink")
  roadsAberdeen <- rbind(roadsAberdeen_NO, roadsAberdeen_NJ)
  roadsAberdeenCrop <- sf::st_crop(roadsAberdeen, st_bbox(sfDeer %>%
                                                            filter(region == "Aberdeenshire")))
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


  landRastNewForest <- terra::rast(here("data", "GIS data", "UKCEH_Landcover",
                                        "UKCEH_Landcover2023_nf",
                                        "data", "LCM.tif"))

  landNewForest <- terra::crop(landRastNewForest, st_bbox(sfDeer %>%
                                                     filter(region == "New Forest")) +
                          c(-2000, -2000, 2000, 2000)
  )

  landNewForest <- landNewForest %>%
    mutate(LCM_1_cat = paste0("LCM_", LCM_1))

  roadsNewForest_SY <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_SU.gml"),
                               layer = "RoadLink")
  roadsNewForestCrop <- sf::st_crop(roadsNewForest_SY, st_bbox(sfDeer %>%
                                                                 filter(region == "New Forest")))

  roadsNewForestCrop <- roadsNewForestCrop %>%
    mutate(roadSize = case_when(
      roadFunction == "A Road" ~ "A roads",
      roadFunction == "B Road" ~ "B roads",
      roadFunction %in% c("Local Access Road", "Restricted Local Access Road",
                          "Local Road", "Minor Road", "Secondary Access Road") ~ "C roads",
      TRUE ~ "Other"
    )) %>%
    mutate(roadSize = factor(roadSize,
                             levels = c("A roads", "B roads", "C roads", "Other")))

  writeRaster(landAberdeen, filename = here("data", "GIS data", "landuseAberdeen.tif"), overwrite = TRUE)
  writeRaster(landNewForest, filename = here("data", "GIS data", "landuseWessex.tif"), overwrite = TRUE)

  landuseList <- list(
    "Aberdeen" = list(landuse = here("data", "GIS data", "landuseAberdeen.tif"),
                      roads = roadsAberdeenCrop),
    "Wessex" = list(landuse = here("data", "GIS data", "landuseWessex.tif"),
                    roads = roadsNewForestCrop)
  )

  return(landuseList)

}
