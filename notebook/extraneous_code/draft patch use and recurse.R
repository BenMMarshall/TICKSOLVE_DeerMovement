
targets::tar_load("tar_deerData")

sfDeer <- tar_deerData %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE, crs = 27700)

patchesWessex <- read_sf(here("data", "GIS data", "patchesWessex.geoJSON"))
patchesAberdeen <- read_sf(here("data", "GIS data", "patchesAberdeen.geoJSON"))

patchesAll <- rbind(patchesWessex, patchesAberdeen)

patchIntersection <- sf::st_intersection(sfDeer, patchesAll)

patchIntersection %>%
  group_by(Animal_ID) %>%
  summarise(patches = n_distinct(Ptch_ID)) %>%
  st_drop_geometry() %>%
  ggplot() +
  geom_histogram(aes(x = patches)) +
  theme_bw()

library(recurse)

# focalDeer <- deerData %>%
#   filter(Animal_ID == "Roe13_F") %>%
#   dplyr::select(x, y, datetime, "id" = Animal_ID)

id <- "Roe13_F"
focalDeer <- deerData %>%
  filter(Animal_ID == id) %>%
  select(x, y, "t" = datetime, "id" = Animal_ID)
  # st_as_sf(coords = c("x", "y"), remove = FALSE, crs = 27700) %>%
  # make_track(.x = x, .y = y, .t = datetime, crs = 27700, id = Animal_ID)

focalsf <- focalDeer %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE, crs = 27700)

inPatch <- st_intersection(focalsf, patchesAll)

focalDeer %>%
  left_join(inPatch %>%
              select(t, id, Ptch_ID) %>%
              filter(!duplicated(t))) %>%
  ggplot() +
  geom_point(aes(x = x, y = y, colour = Ptch_ID))

# find points within polygons
st_join(focalsf, patchesAll, join = st_within)

#### SEEMS LIKE RECURSE WILL NOT WORK AS THE POLYGON SHAPES (CONCAVE + HOLES)
# ARE LIKELY CAUSING AN ISSUE ####
#### TRAJ CAN ENTER AND EXIT DURING ONE MOVE, LEAST THAT'S WHAT I SUSPECT IS CASUING ISSUES ####
intersectingPoly <- st_intersection(focalsf, patchesAll)

focalPatches <- patchesAll %>%
  # filter(Ptch_ID %in% unique(intersectingPoly$Ptch_ID)[1]) %>%
  filter(Ptch_ID %in% unique(intersectingPoly$Ptch_ID)) %>%
  filter(!duplicated(Ptch_ID)) %>%
  st_geometry()
focalPatches <- focalPatches[3] %>% st_union() %>% st_cast("POLYGON")
plot(focalPatches)

focalTest <- as.data.frame(focalDeer) %>%
  mutate(t = as.POSIXct(t),
         id = factor(id))

recurse::getRecursionsInPolygon(trajectory = focalTest,
                                polygon = focalPatches,
                                threshold = 4, verbose = TRUE)

