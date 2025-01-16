
targets::tar_load("tar_landuseList")

hedgerowData <- st_read(here("data", "GIS data", "UKCEH_Hedgerow", "GB_WLF_V1_0.gdb"))

# patchesWessex <- read_sf(here("data", "GIS data", "patchesWessex.geoJSON")) %>%
#   filter(!duplicated(Ptch_ID))
# patchesAberdeen <- read_sf(here("data", "GIS data", "patchesAberdeen.geoJSON")) %>%
#   filter(!duplicated(Ptch_ID))

hedgesWessex <- st_crop(hedgerowData, landuseWessex)
hedgesAberdeen <- st_crop(hedgerowData, landuseAberdeen)

plot(hedgesWessex)
plot(hedgesAberdeen)

focalLanduseTerra <- rast(tar_landuseList$Wessex$landuse)
focalLanduseTerra <- terra::rasterize(st_buffer(hedgesWessex, 20), focalLanduseTerra,
                                      fun = "max", background = 0, touches = TRUE)
plot(focalLanduseTerra)

distanceHedgeWessex <- focalLanduseTerra %>%
  mutate(layer = ifelse(
    layer == 1, layer,
    NA)) %>%
  terra::distance()
plot(distanceHedgeWessex)


hedgesAberdeen

randomHedgePointsAberdeen <- sf::st_sample(hedgesAberdeen, 100) %>%
  st_coordinates() %>% as.data.frame() %>%
  select("x" = X, "y" = Y, -L1)

luHedgePointsAberdeen <- extract(landuseAberdeen, randomHedgePointsAberdeen)
table(luHedgePointsAberdeen$LCM_1_cat)


randomHedgePointsWessex <- sf::st_sample(hedgesWessex, 100) %>%
  st_coordinates() %>% as.data.frame() %>%
  select("x" = X, "y" = Y, -L1)

luHedgePointsWessex <- extract(landuseWessex, randomHedgePointsWessex)
table(luHedgePointsWessex$LCM_1_cat)

ggplot() +
  geom_spatraster(data = landuseWessex, aes(fill = LCM_1)) +
  geom_sf(data = hedgesWessex, color = "red")
