
library(here)
library(dplyr)
library(sf)
library(terra)
library(tidyterra)
library(ggplot2)
library(rnaturalearth)
library(stringr)
library(ggrepel)

targets::tar_load("tar_deerData")
targets::tar_source()

deerData <- tar_deerData

paletteList <- load_deer_palette()

regionLocations <- deerData %>%
  group_by(region) %>%
  summarise(meanLon = mean(Longitude),
            meanLat = mean(Latitude)) %>%
  st_as_sf(coords = c("meanLon", "meanLat"), crs = 4326) %>%
  st_transform(crs = "+proj=moll")

worldData <- ne_countries(scale = "medium", type = "map_units",
                          returnclass = "sf")

geodata::gadm(country = "GB",
              path = here("data", "GIS data"),
              level = 2,
              version = "latest")
gbGADM <- readRDS(here("data", "GIS data", "gadm", "gadm41_GBR_2_pk.rds"))
gbGADM <- st_as_sf(gbGADM)
# gbGADM <- st_transform(gbGADM, 27700)


ggplot() +
  geom_sf(data = worldData %>%
            # st_crop(xmin = -50, xmax = 45,
            #         ymin = 10, ymax = 73) %>%
            st_transform(crs = "+proj=moll"),
          aes(fill = sovereignt == "United Kingdom")) +
  geom_sf(data = regionLocations,
          aes(),
          size = 4, colour = paletteList$baseGrey) +
  geom_sf_text(data = regionLocations, aes(label = region),
               size = 4, nudge_y = 75000, hjust = 0, fontface = 4,
               colour = paletteList$baseGrey) +
  annotate("text", x = -600000, y = 6700000, label = "United\nKingdom",
           size = 6, fontface = 2, hjust = 1, lineheight = 0.95) +
  scale_x_continuous(limits = c(-1300000, 3000000)) +
  scale_y_continuous(limits = c(4000000, 7500000)) +
  coord_sf(crs = "+proj=moll") +
  theme_minimal() +
  labs(x = "", y = "") +
  theme(text = element_text(colour = paletteList$baseGrey),
        line = element_line(colour = paletteList$baseGrey),
        legend.position = "none")


# Local maps --------------------------------------------------------------

targets::tar_load("tar_patchList")
patchList <- tar_patchList

targets::tar_load("tar_landuseList")
landuseList <- tar_landuseList

focalRegion <- "Aberdeenshire"
# focalRegion <- "Wessex"

stackLayers <- read_stack_layers()

if(focalRegion == "Aberdeenshire" | focalRegion == "Aberdeen"){
  focalRoads <- landuseList$Aberdeen$roads
  croppedStack <- stackLayers %>%
    crop(patchList[["Aberdeen"]])
} else {
  focalRoads <- landuseList$Wessex$roads
  croppedStack <- stackLayers %>%
    crop(patchList[["Wessex"]])
}

meanLocations <- deerData %>%
  group_by(Animal_ID, region) %>%
  summarise(meanx = mean(x),
            meany = mean(y)) %>%
  filter(region == focalRegion)

ggplot() +
  geom_spatraster(data = croppedStack, aes(fill = Deciduous_Broadleaf_Forest)) +
  geom_spatraster(data = croppedStack, aes(fill = Evergreen_Needleleaf_Forest)) +
  geom_label_repel(data = meanLocations, aes(x = meanx, y = meany, label = Animal_ID),
            colour = paletteList$deerSpeciesPal[1], size = 4, force = 1, force_pull = 0.1,
            box.padding = unit(1, "lines"), label.padding = unit(1.5, "mm"),
            label.size = 0.75, segment.size = 0.95,
            max.overlaps = 20, max.iter = 100000,
            seed = 2025, fontface = 2) +
  geom_point(data = meanLocations, aes(x = meanx, y = meany),
             fill = paletteList$deerSpeciesPal[1], colour = paletteList$baseGrey,
             size = 3, pch = 21) +
  # geom_sf(data = focalRoads,
  #         colour = paletteList$highSigLowSigNoSig[["Not Significant"]], linewidth = 0.25) +
  scale_fill_manual(values = c(
    "0" = "#ffffff",
    "1" = paletteList$corePal[["Woodland"]]
  ), na.value = "grey85") +
  coord_sf(expand = 0) +
  labs(x = "", y = "", fill = "Forested Areas") +
  theme_minimal() +
  theme(text = element_text(colour = paletteList$baseGrey),
        line = element_line(colour = paletteList$baseGrey),
        legend.position = "none")
