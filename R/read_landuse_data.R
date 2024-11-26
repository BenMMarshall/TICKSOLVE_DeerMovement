#' Read land use and road data
#'
#' @name read_deer_data
#' @description Reads in the UKCEH land use data for the two areas and also OS road data.
#' @return A list of Aberdeen and Wessex land data.
#'
#' @export
read_landuse_data <- function(deerData){

  sfDeer <- st_as_sf(deerData, coords = c("x","y"),
                     crs = 27700)

  landRastAberdeen <- terra::rast(here("data", "GIS data", "UKCEH_Landcover",
                                       "UKCEH_Landcover2023_ab",
                                       "data", "LCM.tif"))

  landAberdeen <- crop(landRastAberdeen, st_bbox(sfDeer %>%
                                                   filter(region == "Aberdeenshire")))

  roadsAberdeen_NO <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_NO.gml"),
                              layer = "RoadLink")
  roadsAberdeen_NJ <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_NJ.gml"),
                              layer = "RoadLink")
  roadsAberdeen <- rbind(roadsAberdeen_NO, roadsAberdeen_NJ)
  roadsAberdeenCrop <- sf::st_crop(roadsAberdeen, st_bbox(sfDeer %>%
                                                            filter(region == "Aberdeenshire")))



  landRastNewForest <- terra::rast(here("data", "GIS data", "UKCEH_Landcover",
                                        "UKCEH_Landcover2023_nf",
                                        "data", "LCM.tif"))

  landNewForest <- crop(landRastNewForest, st_bbox(sfDeer %>%
                                                     filter(region == "New Forest")))

  landNewForest <- landNewForest %>%
    mutate(LCM_1_cat = paste0("LCM_", LCM_1))

  roadsNewForest_SY <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_SU.gml"),
                               layer = "RoadLink")
  roadsNewForestCrop <- sf::st_crop(roadsNewForest_SY, st_bbox(sfDeer %>%
                                                                 filter(region == "New Forest")))

  return(list(
    "aber" = list(landAberdeen = landAberdeen,
                  roadsAberdeen = roadsAberdeenCrop),
    "wess" = list(landNewForest = landNewForest,
                  roadsNewForest = roadsNewForestCrop)
  )
  )

}
