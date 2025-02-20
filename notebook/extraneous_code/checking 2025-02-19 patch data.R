library(sf)
library(ggplot2)

list.files(here("data", "GIS data", "patches", "patches_2025-02-19"), pattern = ".shp")

# [1] "Abdn_all_patches.shp"             "Abdn_final_patches_cropped.shp"
# [3] "Wessex_all_patches.shp"           "Wessex_all_patches_edited.shp"
# [5] "Wessex_final_patches_cropped.shp"

Abdn_all_patches.shp <- read_sf(here("data", "GIS data", "patches", "patches_2025-02-19",
                                     "Abdn_all_patches.shp"))

Abdn_final_patches_cropped.shp <- read_sf(here("data", "GIS data", "patches", "patches_2025-02-19",
                                     "Abdn_final_patches_cropped.shp"))


ggplot(Abdn_all_patches.shp) +
  geom_sf() +
  geom_sf(data = Abdn_final_patches_cropped.shp, colour = "red") +
  theme_minimal()

Wessex_all_patches.shp <- read_sf(here("data", "GIS data", "patches", "patches_2025-02-19",
                                     "Wessex_all_patches.shp"))

Wessex_all_patches_edited.shp <- read_sf(here("data", "GIS data", "patches", "patches_2025-02-19",
                                     "Wessex_all_patches_edited.shp"))

Wessex_final_patches_cropped.shp <- read_sf(here("data", "GIS data", "patches", "patches_2025-02-19",
                                     "Wessex_final_patches_cropped.shp"))

ggplot(Wessex_all_patches.shp) +
  geom_sf() +
  geom_sf(data = Wessex_final_patches_cropped.shp, colour = "red") +
  geom_sf(data = Wessex_all_patches_edited.shp, colour = "green") +
  theme_minimal()

