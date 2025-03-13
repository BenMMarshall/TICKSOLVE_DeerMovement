
library(ggmap)
library(ggplot2)
library(here)
library(dplyr)
library(sf)
library(terra)
library(tidyterra)
library(lubridate)

register_google(key = key)

targets::tar_load("tar_deerData")
deerData <- tar_deerData

deerSF <- st_as_sf(deerData, coords = c("x", "y"), crs = 27700, remove = FALSE)
deerLL <- st_transform(deerSF, crs = 4326)

deerLL <- deerLL %>%
  mutate(month = month(as.POSIXct(datetime), abbr = TRUE, label = TRUE)) %>%
  mutate(lon = st_coordinates(deerLL)[,1],
         lat = st_coordinates(deerLL)[,2],
         Sex = ifelse(Sex == "M", "Male", "Female"))

focalDeer <- focalDeer %>%
  filter(Animal_ID == "Roe07_F")

centreLoc <- focalDeer %>%
  st_coordinates() %>%
  as.data.frame() %>%
  summarise(x = mean(X),
            y = mean(Y),
            xMin = min(X),
            xMax = max(X),
            yMin = min(Y),
            yMax = max(Y))

map <- get_googlemap(c(centreLoc[[1]],
                       centreLoc[[2]]), zoom = 15,
                     maptype = "satellite")

backgroundImage <- terra::rast(map)

scaleLocation <- data.frame(x = c(focalDeer[focalDeer$x == min(focalDeer$x),]$x,
                                  focalDeer[focalDeer$x == min(focalDeer$x),]$x + 500),
                            y = c(focalDeer[focalDeer$y == max(focalDeer$y),]$y,
                                  focalDeer[focalDeer$y == max(focalDeer$y),]$y))

scaleLocation <- scaleLocation %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE, crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

scaleLocationLL <- data.frame(
  x = scaleLocation$lon[1],
  xend = scaleLocation$lon[2],
  y = scaleLocation$lat[1],
  yend = scaleLocation$lat[2]
)

windowsFonts()

paletteMonths <- viridis::magma(n = 12)
names(paletteMonths) <- lubridate::month(seq(1:12), abbr = TRUE, label = TRUE)

ggplot() +
  geom_spatraster_rgb(data = backgroundImage) +
  geom_segment(data = scaleLocationLL, aes(x = x - 0.001, y = y + 0.001,
                                           xend = xend - 0.001, yend = yend + 0.001),
               colour = "white", size = 1.5) +
  annotate("text", x = scaleLocationLL$x[1] - 0.001, y = scaleLocationLL$y[1] + 0.001,
           label = "500m", colour = "white", size = 3, hjust = 0, vjust = 1.3,
           fontface = 2) +
  geom_path(data = focalDeer, aes(x = lon, y = lat, colour = month),
            linewidth = 0.8, alpha = 0.75) +
  geom_point(data = focalDeer, aes(x = lon, y = lat, fill = month), pch = 21,
             colour = "white", size = 1.5) +
  coord_sf(xlim = c(centreLoc[[3]] - 0.001, centreLoc[[4]] + 0.001),
           ylim = c(centreLoc[[5]] - 0.001, centreLoc[[6]] + 0.001)
           ) +
  scale_x_continuous(breaks = pretty(seq(centreLoc[[3]], centreLoc[[4]], length.out = 4),
                                     n = 4)) +
  scale_y_continuous(breaks = pretty(seq(centreLoc[[5]], centreLoc[[6]], length.out = 4),
                                     n = 4)) +
  scale_fill_manual(values = paletteMonths) +
  scale_colour_manual(values = paletteMonths) +
  # scale_fill_viridis_d(option = "C") +
  # scale_colour_viridis_d(option = "C") +
  labs(title = focalDeer$Animal_ID[1],
       x = "Longitude", y = "Latitude", colour = "Month", fill = "Month") +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25", family = "TT Arial"),
    line = element_line(colour = "grey25"),
    plot.background = element_blank(),
    axis.title = element_text(size = 12),
    axis.ticks = element_line(colour = "grey25"),
    axis.text = element_text(angle = 45, hjust = 1, vjust = 0.5)
  )

centreLoc_aber <- deerLL %>%
  filter(region == "Aberdeenshire") %>%
  st_coordinates() %>%
  as.data.frame() %>%
  summarise(x = mean(X),
            y = mean(Y),
            xMin = min(X),
            xMax = max(X),
            yMin = min(Y),
            yMax = max(Y))

map_aber <- get_googlemap(c(centreLoc_aber[[1]],
                       centreLoc_aber[[2]]), zoom = 9,
                     maptype = "satellite")

backgroundImage_aber <- terra::rast(map_aber)

ggplot() +
  geom_spatraster_rgb(data = backgroundImage_aber) +
  geom_segment(data = scaleLocationLL, aes(x = x - 0.001, y = y + 0.001,
                                           xend = xend - 0.001, yend = yend + 0.001),
               colour = "white", size = 1.5) +
  annotate("text", x = scaleLocationLL$x[1] - 0.001, y = scaleLocationLL$y[1] + 0.001,
           label = "500m", colour = "white", size = 3, hjust = 0, vjust = 1.3,
           fontface = 2) +
  # geom_path(data = deerLL %>%
  #             filter(region == "Aberdeenshire"), aes(x = lon, y = lat, colour = month,
  #                                                    group = Animal_ID),
  #           linewidth = 0.8, alpha = 0.75) +
  geom_point(data = deerLL  %>%
               filter(region == "Aberdeenshire"), aes(x = lon, y = lat, fill = Sex, group = Animal_ID),
             pch = 21,
             colour = "white", size = 1.5) +
  coord_sf(xlim = c(centreLoc_aber[[3]] - 0.001, centreLoc_aber[[4]] + 0.001),
           ylim = c(centreLoc_aber[[5]] - 0.001, centreLoc_aber[[6]] + 0.001)) +
  scale_fill_viridis_d(option = "C") +
  scale_colour_viridis_d(option = "C") +
  labs(title = "Aberdeenshire Overview",
       x = "Longitude", y = "Latitude", colour = "Month", fill = "Month") +
  theme_bw()

