
targets::tar_load("tar_akdeLists")
targets::tar_load("tar_deerData")

akdeLists <- tar_akdeLists
deerData <- tar_deerData

library(here)
library(dplyr)
library(ggplot2)
library(sf)
library(ctmm)
library(stringr)

# mean(list(listAKDE$Roe15_F, listAKDE$Roe12_F))
# mean(tar_akdeLists$akde)

allAreas <- do.call(rbind, akdeLists$area) %>%
  left_join(deerData %>%
              group_by(Animal_ID, region, Sex) %>%
              slice_head(n = 1) %>%
              select(Animal_ID, region, Sex))


allAreas %>%
  filter(!str_detect(Animal_ID, "Fallow")) %>%
  ggplot() +
  geom_errorbar(aes(x = Animal_ID, ymin = low, ymax = high,
                     colour = level, group = level),
                position = position_dodge(width = 0.2), width = 0.25) +
  geom_point(aes(x = Animal_ID, y = est,
                     colour = level, group = level, shape = Sex), position = position_dodge(width = 0.2)) +
  geom_hline(data = allAreas %>%
               filter(!str_detect(Animal_ID, "Fallow")) %>%
               group_by(level) %>%
               summarise(mean = mean(est)),
             aes(yintercept = mean, colour = level), linetype = 2) +
  coord_flip() +
  facet_grid(rows = vars(region), scales = "free_y", space = "free_y", switch = "y",
             axes = "all_y") +
  labs(x = "", y = "Area (hectares)") +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    axis.title = element_text(face = 2),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    # legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
  )



allSF <- do.call(rbind, akdeLists$sf) %>%
  mutate(Animal_ID = str_extract(name, ".+?(?=\\s)"),
         level = str_extract(name, "..\\%")) %>%
  left_join(deerData %>%
              group_by(Animal_ID, region, Sex) %>%
              slice_head(n = 1) %>%
              select(Animal_ID, region, Sex))

deerData %>%
  filter(timeLag < 60*60)
