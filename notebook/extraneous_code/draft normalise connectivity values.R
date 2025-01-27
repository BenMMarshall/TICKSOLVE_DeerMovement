
library(terra)
library(tidyterra)

library(patchwork)

targets::tar_load("tar_connectPois_location_1e.05_1")
targets::tar_load("tar_patchList")
terraConnect <- terra::rast(tar_connectPois_location_1e.05_1)
patchList <- tar_patchList

values(terraConnect)

library(bestNormalize)

bestNormResults <- bestNormalize(
  x = values(terraConnect),
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

terraConnect_orderNorm <- terraConnect
values(terraConnect_orderNorm) <- bestNormResults$x.t

terraConnect_orderNormStan <- terraConnect
values(terraConnect_orderNormStan) <- standardise_01(bestNormResults$x.t)

names(terraConnect_orderNorm) <- "orderNorm"
names(terraConnect_orderNormStan) <- "orderNormStan"


rawMap <- ggplot() +
  geom_spatraster(data = terraConnect) +
  labs(title = "raw") +
  coord_sf()
orderNormMap <- ggplot() +
  geom_spatraster(data = terraConnect_orderNorm) +
  labs(title = "orderNorm") +
  coord_sf()
orderNormStanMap <- ggplot() +
  geom_spatraster(data = terraConnect_orderNormStan) +
  labs(title = "orderNormStan") +
  coord_sf()

# st_crs(patchList$AberdeenSelected) <- st_crs(27700)
focalPatches <- patchList$AberdeenSelected
b <- 250
currPatches <- st_buffer(focalPatches, b)

patchMeanScore <- terra::extract(connectTerra, currPatches, fun = mean,
                                 bind = TRUE, na.rm = TRUE) %>%
  dplyr::select(Ptch_ID, connectivity) %>%
  mutate(buffer = b,
         summaryMethod = "mean") %>%
  as.data.frame()

patchMeanScore_orderNorm <- terra::extract(terraConnect_orderNorm, currPatches, fun = mean,
                                 bind = TRUE, na.rm = TRUE) %>%
  dplyr::select(Ptch_ID, orderNorm) %>%
  mutate(buffer = b,
         summaryMethod = "mean") %>%
  as.data.frame()

patchMeanScore_orderNormStan <- terra::extract(terraConnect_orderNormStan, currPatches, fun = mean,
                                 bind = TRUE, na.rm = TRUE) %>%
  dplyr::select(Ptch_ID, orderNormStan) %>%
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
