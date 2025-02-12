
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

values(connectTerra)

library(bestNormalize)

bestNormResults <- bestNormalize(
  x = values(connectTerra),
  standardize = TRUE,
  allow_exp = TRUE,
  # allow_orderNorm = FALSE,
  # out_of_sample = FALSE,
  k = 10,
  r = 5,
)

bestNormResults$chosen_transform

standardise_01 <- function(x){(x-min(x))/(max(x)-min(x))}

ggplot() +
  geom_density(aes(x = bestNormResults$x, colour = "raw")) +
  geom_density(aes(x = bestNormResults$x.t, colour = "bestNormOrderNorm")) +
  geom_density(aes(x = standardise_01(bestNormResults$x.t), colour = "standardOrdNorm")) +
  theme_bw()

### can USE PREDIT function to use the same transformation on new data - good for WESSEX

connectTerra_orderNorm <- connectTerra
values(connectTerra_orderNorm) <- bestNormResults$x.t

connectTerra_orderNormStan <- connectTerra
values(connectTerra_orderNormStan) <- standardise_01(bestNormResults$x.t)

names(connectTerra_orderNorm) <- "orderNorm"
names(connectTerra_orderNormStan) <- "orderNormStan"


rawMap <- ggplot() +
  geom_spatraster(data = connectTerra) +
  geom_sf(data = currPatches, aes(colour = as.factor(Ptch_ID)), fill = NA, size = 1.5) +
  labs(title = "raw") +
  coord_sf(xlim = st_bbox(connectTerra)[c(1,3)], ylim = st_bbox(connectTerra)[c(2,4)]) +
  guides(colour = guide_none())
orderNormMap <- ggplot() +
  geom_spatraster(data = connectTerra_orderNorm) +
  geom_sf(data = currPatches, aes(colour = as.factor(Ptch_ID)), fill = NA, size = 1.5) +
  labs(title = "orderNorm") +
  coord_sf(xlim = st_bbox(connectTerra)[c(1,3)], ylim = st_bbox(connectTerra)[c(2,4)]) +
  guides(colour = guide_none())
orderNormStanMap <- ggplot() +
  geom_spatraster(data = connectTerra_orderNormStan) +
  geom_sf(data = currPatches, aes(colour = as.factor(Ptch_ID)), fill = NA, size = 1.5) +
  labs(title = "orderNormStan") +
  coord_sf(xlim = st_bbox(connectTerra)[c(1,3)], ylim = st_bbox(connectTerra)[c(2,4)]) +
  guides(colour = guide_none())

st_crs(patchList$AberdeenSelected) <- st_crs(27700)
focalPatches <- patchList$AberdeenSelected
b <- 250
currPatches <- st_buffer(focalPatches, b)

patchMeanScore <- terra::extract(connectTerra, currPatches, fun = mean,
                                 bind = TRUE, na.rm = TRUE) %>%
  # dplyr::select(Ptch_ID, connectivity) %>%
  mutate(buffer = b,
         summaryMethod = "mean") %>%
  as.data.frame()

patchMeanScore_orderNorm <- terra::extract(connectTerra_orderNorm, currPatches, fun = mean,
                                 bind = TRUE, na.rm = TRUE) %>%
  # dplyr::select(Ptch_ID, orderNorm) %>%
  mutate(buffer = b,
         summaryMethod = "mean") %>%
  as.data.frame()

patchMeanScore_orderNormStan <- terra::extract(connectTerra_orderNormStan, currPatches, fun = mean,
                                 bind = TRUE, na.rm = TRUE) %>%
  # dplyr::select(Ptch_ID, orderNormStan) %>%
  mutate(buffer = b,
         summaryMethod = "mean") %>%
  as.data.frame()

rawPatch <- ggplot(patchMeanScore) +
  geom_point(aes(x = connectivity, y = as.factor(Ptch_ID), colour= as.factor(Ptch_ID))) +
  theme_bw() +
  theme(legend.position = "none")
orderNormPatch <- ggplot(patchMeanScore_orderNorm) +
  geom_point(aes(x = orderNorm, y = as.factor(Ptch_ID), colour = as.factor(Ptch_ID))) +
  theme_bw() +
  theme(legend.position = "none")
orderNormStanPatch <- ggplot(patchMeanScore_orderNormStan) +
  geom_point(aes(x = orderNormStan, y = as.factor(Ptch_ID), colour = as.factor(Ptch_ID))) +
  theme_bw() +
  theme(legend.position = "none")


rawDist <- ggplot() +
  geom_point(data = patchMeanScore, aes(x = connectivity, y = 0, colour = as.factor(Ptch_ID))) +
  geom_density(aes(x = bestNormResults$x))  +
  theme_bw() +
  theme(legend.position = "none")
orderNormDist <- ggplot() +
  geom_point(data = patchMeanScore_orderNorm, aes(x = orderNorm, y = 0, colour = as.factor(Ptch_ID))) +
  geom_density(aes(x = bestNormResults$x.t)) +
  theme_bw() +
  theme(legend.position = "none")
orderNormStanDist <- ggplot() +
  geom_point(data = patchMeanScore_orderNormStan, aes(x = orderNormStan, y = 0, colour = as.factor(Ptch_ID))) +
  geom_density(aes(x = standardise_01(bestNormResults$x.t))) +
  theme_bw() +
  theme(legend.position = "none")

(rawMap + orderNormMap + orderNormStanMap) / (rawDist + orderNormDist + orderNormStanDist) /
  (rawPatch + orderNormPatch + orderNormStanPatch)

ggsave(filename = here("figures", "TEMP_transformationComparison.png"),
       width = 380, height = 300, units = "mm", dpi = 300)


allPatchData <- patchMeanScore %>%
  mutate(connectMeasure = "rawConnectivity") %>%
  rbind(patchMeanScore_orderNorm %>%
          mutate(connectMeasure = "orderNormConnectivity") %>%
          rename(connectivity = orderNorm)) %>%
  rbind(patchMeanScore_orderNormStan %>%
          mutate(connectMeasure = "orderNormStanConnectivity") %>%
          rename(connectivity = orderNormStan)) %>%
  mutate(connectMeasure = factor(connectMeasure,
                                 levels = c("rawConnectivity",
                                            "orderNormConnectivity",
                                            "orderNormStanConnectivity")))


allPatchData <- allPatchData %>%
  select(Ptch_ID, connectivity, connectMeasure, area_km_sq, iso_mean, decid, conif) %>%
  tidyr::pivot_longer(cols = c(area_km_sq, iso_mean, decid, conif),
                      names_to = "patchVariable", values_to = "patchValue")

allPatchData %>%
  ggplot() +
  geom_point(aes(x = connectivity, y = patchValue, colour = as.factor(Ptch_ID))) +
  facet_grid(cols = vars(connectMeasure), rows = vars(patchVariable),
             scales = "free") +
  theme_bw() +
  theme(legend.position = "none")

# patchCompareArea <- allPatchData %>%
#   ggplot() +
#   geom_point(aes(x = area_km_sq, y = connectivity, colour = as.factor(Ptch_ID))) +
#   facet_grid(cols = vars(connectMeasure)) +
#   theme_bw() +
#   theme(legend.position = "none")
#
# patchCompareIsoMean <- allPatchData %>%
#   ggplot() +
#   geom_point(aes(x = iso_mean, y = connectivity, colour = as.factor(Ptch_ID))) +
#   facet_grid(cols = vars(connectMeasure)) +
#   theme_bw() +
#   theme(legend.position = "none")
#
# patchCompareDecid <- allPatchData %>%
#   ggplot() +
#   geom_point(aes(x = decid, y = connectivity, colour = as.factor(Ptch_ID))) +
#   facet_grid(cols = vars(connectMeasure)) +
#   theme_bw() +
#   theme(legend.position = "none")
#
# patchCompareArea / patchCompareIsoMean / patchCompareDecid

ggsave(filename = here("figures", "TEMP_transformationPatchValues.png"),
       width = 380, height = 300, units = "mm", dpi = 300)
