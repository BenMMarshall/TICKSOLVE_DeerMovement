# Patches -----------------------------------------------------------------

library(here)
library(sf)
library(dplyr)

# remotes::install_github("BAAQMD/geotools")
aberdeenPatches <- read_sf(here("data", "GIS data", "patches",
                                "shapefiles", "all_patches_abdn.shp"))
aberdeenPatchesSelected <- read_sf(here("data", "GIS data", "patches",
                                        "Abdnshire", "Abdnshire", "Abdn_final_patches.shp"))

# aberdeenPatches %>%
#   ggplot() +
#   geom_sf(aes(fill = ar_km_s))

wessexPatches <- read_sf(here("data", "GIS data", "patches",
                              "shapefiles", "all_patches_wessex.shp"))

# wessexPatches %>%
#   ggplot() +
#   geom_sf(aes(fill = ar_km_s))

wessexPatches <- wessexPatches %>%
  filter(!duplicated(Ptch_ID)) %>%
  mutate(id = row_number()) %>%
  select(Ptch_ID, id)

aberdeenPatches <- aberdeenPatches %>%
  filter(!duplicated(Ptch_ID)) %>%
  mutate(id = row_number()) %>%
  select(Ptch_ID, id)

aberdeenPatchesSelected <- aberdeenPatchesSelected %>%
  rename(Ptch_ID = lyr.1)

st_crs(aberdeenPatchesSelected) <- st_crs(27700)

st_write(wessexPatches, here("data", "GIS data", "patchesWessex.geoJSON"),
         driver = "geoJSON", append = FALSE)
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
