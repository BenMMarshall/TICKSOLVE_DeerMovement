library(here)
library(sf)
library(dplyr)
library(terra)
library(tidyterra)

allPoints <- st_read(here("data", "GIS data", "WRENLandscape", "all_points.shp"))
plot(allPoints)
allPoints$site.id

allPatches <- st_read(here("data", "GIS data", "WRENLandscape", "all_patches_polygon.shp"))
plot(allPatches)
allPatches <- st_transform(allPatches, crs = st_crs(allPoints))

write_sf(obj = allPatches, dsn = here("data", "GIS data", "patchesWREN.geoJSON"))

overallLU <- rast(here("data", "GIS data", "UKCEH_Landcover", "UKCEH_Landcover2023_wr_25res", "data", "LCM.tif"))
plot(overallLU$LCM_1)

landuseWREN <- terra::crop(overallLU, vect(allPatches))

landuseWREN <- landuseWREN %>%
  mutate(LCM_1_cat = paste0("LCM_", LCM_1))

plot(wrenLU)


# Road data ---------------------------------------------------------------

roadsAberdeen_NN <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_NN.gml"),
                            layer = "RoadLink")
roadsAberdeen_NO <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_NO.gml"),
                            layer = "RoadLink")
roadsAberdeen_NS <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_NS.gml"),
                            layer = "RoadLink")
roadsAberdeen_NT <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_NT.gml"),
                            layer = "RoadLink")

roadsOverall <- do.call(bind_rows, list(roadsAberdeen_NN,
                                        roadsAberdeen_NO,
                                        roadsAberdeen_NS,
                                        roadsAberdeen_NT))

roadsWREN <- sf::st_crop(roadsOverall, st_bbox(allPatches))
roadsWREN <- roadsWREN %>%
  mutate(roadSize = case_when(
    roadFunction == "A Road" ~ "A roads",
    roadFunction == "B Road" ~ "B roads",
    roadFunction %in% c("Local Access Road", "Restricted Local Access Road",
                        "Local Road", "Minor Road", "Secondary Access Road") ~ "C roads",
    TRUE ~ "Other"
  )) %>%
  mutate(roadSize = factor(roadSize,
                           levels = c("A roads", "B roads", "C roads", "Other")))

# Woodland distance -------------------------------------------------------

distanceWoodlandWREN <- wrenLU %>%
  select(LCM_1) %>%
  filter(LCM_1 %in% 1:2) %>%
  mutate(LCM_1 = case_when(
    LCM_1 %in% 1:2 ~ 1,
    TRUE ~ NA)) %>%
  terra::distance() %>%
  rename(distanceWoodland = LCM_1)

# Hedgerow distance -------------------------------------------------------

############################################################
############################################################
prelimAggFact <- NA
############################################################
############################################################

hedgerowData <- vect(here("data", "GIS data", "UKCEH_Hedgerow", "GB_WLF_V1_0.gdb"))

hedgesWREN <- terra::crop(hedgerowData, landuseWREN)
hedgesWRENRast <- rast(landuseWREN)

hedgesWRENRast <- terra::distance(hedgesWRENRast, hedgesWREN)


if(is.na(prelimAggFact)){
  hedgesWRENRast <- terra::rasterize(st_buffer(hedgesWREN, 0+2), hedgesWRENRast,
                                         fun = "max", background = NA, touches = TRUE) %>%
    terra::distance() %>%
    rename(distanceHedges = layer)
} else {
  hedgesWRENRast <- terra::rasterize(st_buffer(hedgesWREN, prelimAggFact+2), hedgesWRENRast,
                                         fun = "max", background = NA, touches = TRUE) %>%
    terra::distance() %>%
    rename(distanceHedges = layer)
}

plot(hedgesWRENRast)

# Land use recat ----------------------------------------------------------

landuseWREN <- landuseWREN %>%
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

writeRaster(landuseWREN,
            filename = here("data", "GIS data", "landuseWREN.tif"), overwrite = TRUE)

writeRaster(distanceWoodlandWREN,
            filename = here("data", "GIS data", "distanceWoodlandWREN.tif"), overwrite = TRUE)

writeRaster(hedgesWRENRast,
            filename = here("data", "GIS data", "distanceHedgesWREN.tif"), overwrite = TRUE)
