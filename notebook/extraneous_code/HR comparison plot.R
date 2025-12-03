
library(here)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(ggdist)
library(patchwork)

homRangeData <- read.csv(here("data", "Home Range Database", "HomeRangeData_2023_09_06.csv"))

akdeSummary <- read.csv(here::here("tables", "allAKDESummary.csv"))

paletteList

currentStudyData <- akdeSummary %>%
  filter(level == 0.95) %>%
  select(est, Sex, Animal_ID) %>%
  mutate(Species = "Capreolus capreolus",
         HR_Level = "Individual",
         Tracking_Method = "GPS",
         HR_Method_Simple = "AKDE\n(This Study)",
         Home_Range_km2 = est/100) %>%
  mutate(Latitude = ifelse(str_detect(Animal_ID, "03|07"), 50.9594, 57.13307),
         Longitude = ifelse(str_detect(Animal_ID, "03|07"), -1.661488, -2.503074)) %>%
  select(-Animal_ID)

aberMean <- readRDS(here::here("modelOutput", "aberdeenMeanAKDE.rds"))

currentStudyMean <- data.frame(Species = "Capreolus capreolus",
                               HR_Level = "Individual",
                               Sex = "B",
                               Tracking_Method = "GPS",
                               HR_Method_Simple = "AKDE\n(This Study)",
                               Home_Range_km2 = aberMean$CI[1,2]/100

)

roeData <- homRangeData[homRangeData$Species %in% c("Capreolus capreolus"),] %>%
  filter(Context == "Wild")

roeData %>%
  filter(HR_Level == "Individual") %>%
  filter(Tracking_Method %in% c("Radio tracking",
                                "Radio tracking, direct observations",
                                "Radio tracking, live trapping",
                                "Satellite tracking (GPS), radio tracking")) %>%
  group_by(HR_Level, Species, HR_Method_Simple, Tracking_Method) %>%
  summarise(n = n())

indiHRData <- roeData %>%
  filter(HR_Level == "Individual",
         HR_Span == "Annual") %>%
  filter(Tracking_Method %in% c("Radio tracking",
                                "Radio tracking, direct observations",
                                "Radio tracking, live trapping",
                                "Satellite tracking (GPS), radio tracking")) %>%
  plyr::rbind.fill(currentStudyData) %>%
  mutate(Sex = case_when(Sex == "M" ~ "Male",
                         Sex == "F" ~ "Female",
                         TRUE ~ "Both/Unknown")) %>%
  mutate(Sex = factor(Sex, levels = c("Female", "Male", "Both/Unknown")))

popHRData <- roeData %>%
  filter(!HR_Level == "Individual",
         HR_Span == "Annual") %>%
  filter(Tracking_Method %in% c("Radio tracking",
                                "Radio tracking, direct observations",
                                "Radio tracking, live trapping",
                                "Satellite tracking (GPS), radio tracking")) %>%
  plyr::rbind.fill(currentStudyMean) %>%
  mutate(Sex = case_when(Sex == "B" ~ "Both/Unknown",
                         Sex == "M" ~ "Male",
                         Sex == "F" ~ "Female")) %>%
  mutate(Sex = factor(Sex, levels = c("Female", "Male", "Both/Unknown")))


(hrCompPlot <- indiHRData %>%
    ggplot() +
    geom_density_ridges(aes(x = Home_Range_km2*100, y = HR_Method_Simple, fill = Sex, point_colour = Sex,
                            point_shape = Sex),
                        jittered_points = TRUE,
                        position = position_points_jitter(height = 0.1, yoffset = -0.2),
                        alpha = 0.7, scale = 0.9, point_alpha = 0.65, point_size = 2) +
    geom_point(data = popHRData,
               aes(x = Home_Range_km2*100, y = HR_Method_Simple, colour = Sex),
               size = 4, position = position_nudge(y = -0.05), alpha = 0.85) +
    scale_x_log10(labels = scales::comma) +
    scale_colour_manual(values = c("#B54D17", "#85AB7A", "#9F7E93")) +
    scale_shape_manual(aesthetics = "point_shape", values = c(17, 18, 15)) +
    scale_colour_manual(aesthetics = "point_colour", values = c("#B54D17", "#85AB7A", "#9F7E93")) +
    scale_fill_manual(values = c("#B54D17", "#85AB7A", "#9F7E93")) +
    # scale_y_discrete(position = "right") +
    # scale_x_continuous(limits = c(0, 1200)) +
    labs(x = "Home Range (ha)", y = "Home Range Estimation Method") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      axis.title = element_text(face = 2),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y.right = element_blank(),
      # legend.position = "none",
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
    )
  )


mapData <- map_data(map = "world") %>%
  filter(!long < -15) %>%
  filter(!lat > 64)

labLong <- expand.grid(
  long = seq(-10, 20, 10),
  lat = seq(35, 60, 10)) %>%
  filter(lat == 35 & long > -10)
labLat <- expand.grid(
  long = seq(-10, 20, 10),
  lat = seq(35, 60, 10)) %>%
  filter(long == -10 & lat > 35)
origin <- data.frame(
  lat = 35, long = -10, lab = "35N\n-10E"
)

(hrCompMap <- mapData %>%
    ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group),
                 fill = "grey45", colour = "grey25", linewidth = 0.5) +
    geom_point(data = indiHRData,
               aes(x = Longitude, y = Latitude),
               size = 4, colour = "grey25", fill = "#9F7E93", shape = 21) +
    geom_point(data = labLong, aes(x = long, y = lat),
              colour = "white", size = 5) +
    geom_text(data = labLong, aes(x = long, y = lat, label = long),
              fontface = 4, colour = "grey25",
              size = 2) +
    geom_point(data = labLat, aes(x = long, y = lat),
              colour = "white", size = 5) +
    geom_text(data = labLat, aes(x = long, y = lat, label = lat),
              fontface = 4, colour = "grey25",
              size = 2) +
    geom_point(data = origin, aes(x = long, y = lat),
              colour = "white", size = 10) +
    geom_text(data = origin, aes(x = long, y = lat, label = lab),
              fontface = 4, colour = "grey25", lineheight = 0.8,
              size = 2) +
  expand_limits(x = mapData$long, y = mapData$lat) +
    coord_map("moll", xlim = c(-10, 20), ylim = c(35, 61)) +
    scale_x_continuous(breaks = seq(-10,20,10)) +
    scale_y_continuous(breaks = seq(35, 60, 10)) +
    theme_minimal() +
    labs(x = "", y = "") +
    theme(text = element_text(colour = "grey25"),
          line = element_line(colour = "grey25"),
          plot.background = element_blank(),
          panel.background = element_rect(fill = "grey85", colour = NA),
          panel.border = element_blank(),
          legend.position = "none",
          axis.text = element_blank()))


# Basic use
hrCompPlot + inset_element(hrCompMap,
                           # left = 0,
                           # bottom = 0.65,
                           # right = 0.275,
                           # top = 1,
                           left = 0.68,
                           bottom = 0.63,
                           right = 1,
                           top = 1,
                           align_to = "full")

ggsave(here("figures", "hrStudyComparison.png"),
       width = 240, height = 200, dpi = 300, units = "mm")
ggsave(here("figures", "hrStudyComparison.pdf"),
       width = 240, height = 200, units = "mm")
