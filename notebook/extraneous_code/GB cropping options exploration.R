
wessLand <- rast(here("data", "GIS data", "landuseWessex.tif"))

occData <- read.csv(here("data", "GBIF data", "fallowUK.csv"),
                    sep = "\t")

occSF <- st_as_sf(occData, coords = c("decimalLongitude", "decimalLatitude"))
st_crs(occSF) <- 4326
occSF <- st_transform(occSF, 27700)

geodata::gadm(country = "GB",
              path = here("data", "GIS data"),
              level = 2,
              version = "latest")

gbGADM <- readRDS(here("data", "GIS data", "gadm", "gadm41_GBR_2_pk.rds"))
gbGADM <- st_as_sf(gbGADM)
gbGADM <- st_transform(gbGADM, 27700)

ggplot(occSF) +
  geom_sf(data = gbGADM, aes(group = NAME_2)) +
  geom_spatraster(data = wessLand, aes(fill = LCM_1_cat),
                  alpha = 0.25) +
  geom_sf(aes(), colour = "black",
             size = 0.5) +
  coord_sf(datum = sf::st_crs(27700),
           xlim = st_bbox(wessLand)[c(1,3)] + c(-50000, 50000),
           ylim = st_bbox(wessLand)[c(2,4)] + c(-50000, 50000),
           expand = 0) +
  theme_minimal() +
  theme(legend.position = "none")

insideWessOcc <- st_crop(occSF, st_bbox(wessLand) +
                           c(-50000, -50000, 50000, 50000))

nrow(insideWessOcc)

