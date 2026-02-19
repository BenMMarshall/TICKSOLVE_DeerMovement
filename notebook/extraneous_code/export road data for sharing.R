
library(here)
library(sf)
library(dplyr)

# targets::tar_load("tar_patchList")
# patchList <- tar_patchList

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

st_write(roadsAberdeenCrop, here("data", "GIS data", "roadsAberdeen.geoJSON"))

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

st_write(roadsWessexCrop, here("data", "GIS data", "roadsWessex.geoJSON"))
