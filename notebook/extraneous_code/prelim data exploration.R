library(here)
library(dplyr)
library(amt)
library(ggplot2)
library(ggridges)
library(lubridate)
library(sf)
library(terra)
library(tidyterra)
library(ctmm)

deerData <- read.csv(here("data", "GPS data", "deer GPScollar ticksolve_cleaned.csv"))
deerMetaData <- read.csv(here("data", "GPS data", "Deer metadata and schedules2.csv"))

deerMetaData <- deerMetaData %>%
  select(Animal_ID, Sex, Collar.ID) %>%
  mutate(Animal_ID = paste0(Animal_ID, "_", Sex))

names(deerData)

deerData <- deerData %>%
  select(-Location, -X, -X.1, -Temperature, -Speed, -fHDOP, -nActivity, -X.2, -nAlt) %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")) %>%
  group_by(Name) %>%
  arrange(datetime) %>%
  mutate(timelag = as.numeric(difftime(datetime, lag(datetime), units = "secs"))) %>%
  left_join(deerMetaData %>%
              select(Animal_ID, "Name" = Collar.ID, Sex)) %>%
  ungroup()


### DEER MISALLOCATED TO NEW FOREST WHEN IN ABERDEEN
deerData %>%
  filter(region == "New Forest", Latitude > 54)
### T3HS-7359
deerData[deerData$Name == "T3HS-7359",]$region <- "Aberdeenshire"


# deerTrack <- make_track(deerData,
#            .t = "datetime", .x = "Longitude", .y = "Latitude",
#            crs = 4326, id = Animal_ID)
#
# deerTrack <- deerTrack %>%
#   nest(data = -"id")
#
# deerTrack <- deerTrack %>%
#   mutate(steps = map(data, ~.x %>%
#                        steps()))
#
# deerTrack <- deerTrack %>%
#   mutate(speed = map(data, ~.x %>%
#                        amt::speed()),
#          sumSpeed = map(data, ~summarize_speed(.x)),
#          sumSl = map(data, ~summarize_sl(.x)))
#
# deerTrack[deerTrack$id == "F7_F",]$sumSpeed
# deerTrack[deerTrack$id == "F7_F",]$sumSl

#### tracking data is in WGS84, patches are in OSGB36
sfDeer <- st_as_sf(deerData, coords = c("Longitude","Latitude"),
                   crs = 4326)
sfDeer <- st_transform(sfDeer, 27700)

sfDeer$x <- sf::st_coordinates(sfDeer)[,1]
sfDeer$y <- sf::st_coordinates(sfDeer)[,2]
sfDeer$epsg <- 27700

sfDeer <- sfDeer %>%
  mutate(
    step = sqrt((x - lag(x))^2 + (y - lag(y))^2),
    speed_ms = (step)/timelag)

write.csv(sfDeer %>%
            st_drop_geometry(), here("data", "deerMovementData.csv"), row.names = FALSE)


# Tracking Regime ---------------------------------------------------------

deerData %>%
  ggplot() +
  geom_point(aes(x = datetime, y = Animal_ID, colour = Sex))

deerData %>%
  mutate(hour = hour(datetime)) %>%
  ggplot() +
  geom_bar(aes(x = hour)) +
  facet_grid(vars(Animal_ID)) +
  theme(strip.text.y = element_text(angle = 0))

deerData %>%
  ggplot() +
  geom_density_ridges(aes(x = timelag, y = Animal_ID, fill = Sex)) +
  scale_x_log10(limits = range(deerData$timelag, na.rm = TRUE))
# T3HS-7357, T3HS-7367, T3HS-7353 are the two on slightly different tracking regimes

# Mapping -----------------------------------------------------------------

deerData %>%
  filter(Latitude > 54) %>%
  ggplot() +
  geom_path(aes(x = Longitude, y = Latitude, colour = Animal_ID)) +
  geom_point(aes(x = Longitude, y = Latitude, colour = Animal_ID),
             size = 0.25) +
  facet_wrap(vars(Animal_ID), scales = "free") +
  coord_quickmap()

deerData %>%
  filter(Latitude < 54) %>%
  ggplot() +
  geom_point(aes(x = Longitude, y = Latitude, colour = Animal_ID)) +
  # facet_wrap(vars(region)) +
  coord_quickmap()

# Patches -----------------------------------------------------------------

# aberdeenPatches <- read_sf(here("data", "GIS data", "patches",
#                             "shapefiles", "all_patches_abdn.shp"))
#
# aberdeenPatches %>%
#   ggplot() +
#   geom_sf(aes(fill = ar_km_s))
#
# wessexPatches <- read_sf(here("data", "GIS data", "patches",
#                             "shapefiles", "all_patches_wessex.shp"))
#
# wessexPatches %>%
#   ggplot() +
#   geom_sf(aes(fill = ar_km_s))
#
# st_write(wessexPatches, here("data", "GIS data", "patchesWessex.geoJSON"),
#          driver = "geoJSON", append = FALSE)
# st_write(aberdeenPatches, here("data", "GIS data", "patchesAberdeen.geoJSON"),
#          driver = "geoJSON", append = FALSE)

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


# Landcover and Roads ---------------------------------------------------------------

UKCEHcolourCodes <- read.csv(here("data", "GIS data", "UKCEHcolourCodes.csv"))
UKCEHcoloursVecColour <- UKCEHcolourCodes$colour
names(UKCEHcoloursVecColour) <- paste0("LCM_", UKCEHcolourCodes$value)
UKCEHcoloursVecNames <- UKCEHcolourCodes$label
names(UKCEHcoloursVecNames) <- paste0("LCM_", UKCEHcolourCodes$value)

st_bbox(sfDeer %>%
          filter(region == "Aberdeenshire"))

landRastAberdeen <- terra::rast(here("data", "GIS data", "UKCEH_Landcover",
                                     "UKCEH_Landcover2023_ab",
                                     "data", "LCM.tif"))

landAberdeen <- crop(landRastAberdeen, st_bbox(sfDeer %>%
                                                 filter(region == "Aberdeenshire")))

landAberdeen <- landAberdeen %>%
  mutate(LCM_1_cat = paste0("LCM_", LCM_1))

# st_layers(here("data", "GIS data", "os_roads", "OSOpenRoads_NO.gml"))
roadsAberdeen_NO <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_NO.gml"),
                            layer = "RoadLink")
roadsAberdeen_NJ <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_NJ.gml"),
                            layer = "RoadLink")
roadsAberdeen <- rbind(roadsAberdeen_NO, roadsAberdeen_NJ)
roadsAberdeenCrop <- sf::st_crop(roadsAberdeen, st_bbox(sfDeer %>%
                                                          filter(region == "Aberdeenshire")))

# table(roadsAberdeenCrop$roadFunction)
roadsAberdeenCrop <- roadsAberdeenCrop %>%
  mutate(roadSize = case_when(
    roadFunction == "A Road" ~ "A roads",
    roadFunction == "B Road" ~ "B roads",
    roadFunction %in% c("Local Access Road", "Restricted Local Access Road",
                        "Local Road", "Minor Road", "Secondary Access Road") ~ "C roads",
    TRUE ~ "Other"
  )) %>%
  mutate(roadSize = factor(roadSize,
                           levels = c("A roads", "B roads", "C roads", "Other")))

ggplot() +
  geom_spatraster(data = landAberdeen, aes(fill = LCM_1_cat)) +
  scale_fill_manual(values = UKCEHcoloursVecColour, labels = UKCEHcolourCodes$label) +
  geom_sf(data = roadsAberdeenCrop, aes(linewidth = roadSize)) +
  scale_linewidth_manual(values = c(1, 0.5, 0.25, 0.1)) +
  geom_sf(data = sfDeer %>%
            filter(region == "Aberdeenshire"),
          aes(), colour = "#ffffff", size = 0.5)

landRastNewForest <- terra::rast(here("data", "GIS data", "UKCEH_Landcover",
                                      "UKCEH_Landcover2023_nf",
                                      "data", "LCM.tif"))

landNewForest <- crop(landRastNewForest, st_bbox(sfDeer %>%
                                                   filter(region == "New Forest")))

landNewForest <- landNewForest %>%
  mutate(LCM_1_cat = paste0("LCM_", LCM_1))

# st_layers(here("data", "GIS data", "os_roads", "OSOpenRoads_NO.gml"))
roadsNewForest_SY <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_SU.gml"),
                             layer = "RoadLink")
roadsNewForestCrop <- sf::st_crop(roadsNewForest_SY, st_bbox(sfDeer %>%
                                                               filter(region == "New Forest")))

# table(roadsNewForestCrop$roadFunction)
roadsNewForestCrop <- roadsNewForestCrop %>%
  mutate(roadSize = case_when(
    roadFunction == "A Road" ~ "A roads",
    roadFunction == "B Road" ~ "B roads",
    roadFunction %in% c("Local Access Road", "Restricted Local Access Road",
                        "Local Road", "Minor Road", "Secondary Access Road") ~ "C roads",
    TRUE ~ "Other"
  )) %>%
  mutate(roadSize = factor(roadSize,
                           levels = c("A roads", "B roads", "C roads", "Other")))


ggplot() +
  geom_spatraster(data = landNewForest, aes(fill = LCM_1_cat)) +
  scale_fill_manual(values = UKCEHcoloursVecColour, labels = UKCEHcolourCodes$label) +
  geom_sf(data = roadsNewForestCrop, aes(linewidth = roadSize)) +
  scale_linewidth_manual(values = c(1, 0.5, 0.25, 0.1)) +
  geom_sf(data = sfDeer %>%
            filter(region == "New Forest"),
          aes(), colour = "#ffffff", size = 0.5)


# Outliers ----------------------------------------------------------------

deerData

deerTele <- as.telemetry(deerData, datetime = "datetime",
                         x = "Longitude", y = "Latitude", id = "Animal_ID")

outlierData <- outlie(deerTele, plot = TRUE, by = "d")

sfDeer <- sfDeer %>%
  group_by(Animal_ID) %>%
  mutate(meanX = mean(x, na.rm = TRUE),
         meanY = mean(y, na.rm = TRUE),
         distCentre = sqrt((x - meanX)^2 + (y - meanY)^2))

sfDeer %>%
  ggplot() +
  geom_path(aes(x = datetime, y = distCentre, colour = speed_ms)) +
  # geom_point(aes(x = datetime, y = distCentre, colour = Sex)) +
  facet_grid(vars(Animal_ID), scales = "free") +
  # scale_colour_gradient(trans = "log") +
  # labs(colour = "log speed (m/s)") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_bw()

# Map speed ---------------------------------------------------------------

sfDeer %>%
  ggplot() +
  geom_path(aes(x = x, y = y, colour = speed_ms),
            alpha = 0.5) +
  coord_quickmap() +
  facet_wrap(vars(Animal_ID), scales = "free") +
  # scale_colour_gradient(trans = "log") +
  # labs(colour = "log speed (m/s)")
  theme_bw()

sfDeer %>%
  filter(speed_ms > 10) %>%
  print(n = 100)

# Site fidelity -----------------------------------------------------------

site_fidelity()




