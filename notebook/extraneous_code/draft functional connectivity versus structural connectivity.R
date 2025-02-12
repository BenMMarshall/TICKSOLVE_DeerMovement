
library(terra)
library(tidyterra)
library(sf)
library(here)
library(ggplot2)
library(patchwork)

targets::tar_load("tar_connectStanPois_location_1e.05_3")
targets::tar_load("tar_patchList")

connectTerra <- terra::rast(
  here("data", "GIS data", str_split(tar_connectStanPois_location_1e.05_3, "/")[[1]][6])
)
names(connectTerra) <- "connectivity"
patchList <- tar_patchList
focalPatches <- patchList$AberdeenSelected

b <- 750
currPatches <- st_buffer(focalPatches, b)
patchMeanScore <- terra::extract(connectTerra, currPatches, fun = mean,
                                 bind = TRUE, na.rm = TRUE) %>%
  # dplyr::select(Ptch_ID, connectivity) %>%
  mutate(buffer = b,
         summaryMethod = "mean") %>%
  as.data.frame()

patchMeanScore %>%
  select(Ptch_ID, connectivity, area_km_sq, iso_mean, decid, conif) %>%
  tidyr::pivot_longer(cols = c(area_km_sq, iso_mean, decid, conif),
                      names_to = "patchVariable", values_to = "patchValue") %>%
  ggplot() +
  geom_point(aes(x = connectivity, y = patchValue, colour = as.factor(Ptch_ID))) +
  facet_grid(rows = vars(patchVariable),
             scales = "free") +
  theme_bw() +
  theme(legend.position = "none")
