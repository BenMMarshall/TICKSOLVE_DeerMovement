# Patches -----------------------------------------------------------------

library(here)
library(sf)
library(terra)
library(dplyr)

# remotes::install_github("BAAQMD/geotools")
aberdeenPatches <- read_sf(here("data", "GIS data", "patches",
                                "patchesAberdeen_2025-01-29", "Aberdeenshire_all_patches_polygon.shp"))
aberdeenPatchesSelected <- read_sf(here("data", "GIS data", "patches",
                                        "Abdnshire", "Abdnshire", "Abdn_final_patches.shp"))

# aberdeenPatches %>%
#   ggplot() +
#   geom_sf(aes(fill = ar_km_s))

# wessexPatches <- read_sf(here("data", "GIS data", "patches",
#                               "shapefiles", "all_patches_wessex.shp"))
wessexPatchesSelected <- read_sf(here("data", "GIS data", "patches",
                              "Wessex", "wessex_patches.shp"))

# wessexPatches %>%
#   ggplot() +
#   geom_sf(aes(fill = ar_km_s))

wessexPatches <- wessexPatches %>%
  filter(!duplicated(Ptch_ID)) %>%
  mutate(id = row_number()) %>%
  select(Ptch_ID, id)

aberdeenPatches <- aberdeenPatches %>%
  rename(Ptch_ID = lyr.1) %>%
  filter(!duplicated(Ptch_ID)) %>%
  mutate(id = row_number()) %>%
  select(Ptch_ID, id)

aberdeenPatchesSelected <- aberdeenPatchesSelected %>%
  rename(Ptch_ID = lyr.1)

st_crs(aberdeenPatches) <- st_crs(27700)
st_crs(aberdeenPatchesSelected) <- st_crs(27700)

class_raster <- terra::rast(here("data", "GIS data", "patches", "Woodlands_wilt.tif"))

source(here("notebook", "extraneous_code", "patchRaster.R"))

wessPatchesRast <- patchRaster(class_raster = here("data", "GIS data", "patches", "Woodlands_wilt.tif"),
                           rclmat = matrix(c(0,1,2,0,1,1), nrow = 3),
                           sizethresh = 5000)

wessexPatches <- terra::as.polygons(wessPatchesRast$Binary_ras)
wessexPatches <- sf::st_as_sf(wessexPatches, group = FALSE) %>%
  filter(lyr.1 == 1) %>%
  st_cast("POLYGON") %>%
  mutate(Ptch_ID = row_number())

st_crs(wessexPatches) <- st_crs(27700)
# wessexPatches <- sf::st_crop(wessexPatches,
#             st_bbox(c(xmin = 395550, xmax = 445575, ymax = 94950, ymin = 137550), crs = st_crs(27700)))

wessexPatches %>%
  mutate(area_m2 = st_area(.)) %>%
  filter(area_m2 > units::set_units(5000, "m2"))

st_write(wessexPatches, here("data", "GIS data", "patchesWessex.geoJSON"),
         driver = "geoJSON", append = FALSE)

st_area(st_as_sfc(st_bbox(patchesAberdeen), crs=27700))
st_area(st_as_sfc(st_bbox(wessexPatches), crs=27700))

st_write(wessexPatchesSelected, here("data", "GIS data", "patchesWessex_selected.geoJSON"),
         driver = "geoJSON", append = FALSE)
# st_write(wessexPatches, here("data", "GIS data", "patchesWessex.geoJSON"),
#          driver = "geoJSON", append = FALSE)
st_write(aberdeenPatches, here("data", "GIS data", "patchesAberdeen.geoJSON"),
         driver = "geoJSON", append = FALSE)
st_write(aberdeenPatchesSelected, here("data", "GIS data", "patchesAberdeen_selected.geoJSON"),
         driver = "geoJSON", append = FALSE)

patchesWessex <- read_sf(here("data", "GIS data", "patchesWessex.geoJSON"))
patchesAberdeen <- read_sf(here("data", "GIS data", "patchesAberdeen.geoJSON"))

st_crs(patchesWessex)

patchesWessex %>%
  ggplot() +
  geom_sf(aes(fill = iso_men)) +
  # geom_point(data = deerData %>%
  #              filter(Latitude < 54),
  #            aes(x = Longitude, y = Latitude, colour = Name)) +
  geom_sf(data = sfDeer %>%
            filter(region == "New Forest"),
          aes(colour = Name)) +
  coord_sf(ylim = st_bbox(sfDeer[sfDeer$region == "New Forest",])[c(2,4)],
           xlim = st_bbox(sfDeer[sfDeer$region == "New Forest",])[c(1,3)])

patchesAberdeen %>%
  ggplot() +
  geom_sf(aes(fill = iso_men)) +
  # geom_point(data = deerData %>%
  #              filter(Latitude < 54),
  #            aes(x = Longitude, y = Latitude, colour = Name)) +
  geom_sf(data = sfDeer %>%
            filter(region == "Aberdeenshire"),
          aes(colour = Name)) +
  coord_sf(ylim = st_bbox(sfDeer[sfDeer$region == "Aberdeen",])[c(2,4)],
           xlim = st_bbox(sfDeer[sfDeer$region == "Aberdeen",])[c(1,3)])


