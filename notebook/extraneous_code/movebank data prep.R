library(here)
library(sf)
library(dplyr)
library(stringr)

deerData <- read.csv(here("data", "GPS data", "deer GPScollar ticksolve_cleaned.csv"))
deerMetaData <- read.csv(here("data", "GPS data", "Deer metadata and schedules2.csv"))

deerMetaData <- deerMetaData %>%
  select(Animal_ID, Sex, Collar.ID) %>%
  mutate(Animal_ID = paste0(Animal_ID, "_", Sex))

# deerData %>%
#   filter(Name == "T3HS-7351") %>%
#   mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
#   filter(Date > as.Date("2023-03-30"), Date < as.Date("2023-04-02"))

deerData <- deerData %>%
  select(-Location, -ID, -X, -X.1, -Temperature, -Speed, -fHDOP, -nActivity, -X.2, -nAlt,
         -Patch.ID) %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
  group_by(Name) %>%
  arrange(datetime) %>%
  left_join(deerMetaData %>%
              select(Animal_ID, "Name" = Collar.ID, Sex)) %>%
  ungroup() %>%
  mutate(region = case_when(
    region == "New Forest" ~ "Wessex",
    TRUE ~ region
  ),
  Species = case_when(spp == "roe" ~ "Capreolus capreolus",
                      spp == "fallow" ~ "Dama dama")) %>%
  select(-spp)

deerData$Animal_ID <- gsub("^R", "Roe", deerData$Animal_ID)
deerData$Animal_ID <- gsub("^F", "Fallow", deerData$Animal_ID)
deerData$Animal_ID <- str_replace(deerData$Animal_ID, "[:digit:]{1,2}",
                                  str_pad(str_extract(deerData$Animal_ID, "[:digit:]{1,2}"), width = 2, side = "left", pad = "0"))

##########################################
### DEER MISALLOCATED TO NEW FOREST WHEN IN ABERDEEN
##########################################
deerData %>%
  filter(region == "Wessex", Latitude > 54)
### T3HS-7359
deerData[deerData$Name == "T3HS-7359",]$region <- "Aberdeenshire"

##########################################
##########################################

#### tracking data is in WGS84, patches are in OSGB36
sfDeer <- st_as_sf(deerData, coords = c("Longitude","Latitude"), remove = FALSE,
                   crs = 4326)
sfDeer <- st_transform(sfDeer, 27700)

sfDeer$x <- sf::st_coordinates(sfDeer)[,1]
sfDeer$y <- sf::st_coordinates(sfDeer)[,2]
sfDeer$epsg <- 27700

deerOUT <- sfDeer %>%
  st_drop_geometry()

sfDeer %>%
  group_by(region, Species) %>%
  summarise(meanlon = mean(Longitude),
            meanlat = mean(Latitude))

write.csv(deerOUT %>%
            filter(region == "Wessex", Species == "Capreolus capreolus"),
          file = here::here("data", "GPS data", "MOVEBANK Roe Deer, Wessex.csv"),
          row.names = FALSE)

write.csv(deerOUT %>%
            filter(region == "Wessex", Species == "Dama dama"),
          file = here::here("data", "GPS data", "MOVEBANK Fallow Deer, Wessex.csv"),
          row.names = FALSE)


Roe Deer (Capreolus capreolus) - University of Glasgow - England, UK - GPS
Fallow Deer (Dama dama) - University of Glasgow - England, UK - GPS

Citation
https://ticksolve.ceh.ac.uk/

Acknowledgements
We thank the University of Glasgow, University of Liverpool, and UK Centre for Ecology and Hydrology for making this research possible.
This research forms part of the TickSolve project (https://ticksolve.ceh.ac.uk/) and was funded by UK Research and Innovation through the NERC grant NE/W003171/1 and NE/W003244/1.
For permission to conduct the work, we thank NatureScot, ForestryEngland and NaturalEngland.
We are indebted to the hundreds of volunteers who assisted in the deployment of deer GPS collars.
We especially thank Mark Hewison, Jochen Langbein, Tim Dansie, Andy Page, Sandy Shore and Andy Shore for generous advice in the field and training in capture methods.

Grants used
NERC Grants (NE/W003260/1, NE/W003171/1 and NE/W003244/1)

License Type	Custom

License Terms	not set

Study Summary
TickSolve is a large multi-institution NERC Highlight Topic project focused on environmental solutions to reduce the risk of current and future tick-borne zoonotic pathogens in the UK, running from 2021-2025.

Wessex        Capreolus capreolus   -1.67    50.9
Wessex        Dama dama             -1.66    51.0
                                                                                                                                        3 Wessex        Dama dama             -1.66
