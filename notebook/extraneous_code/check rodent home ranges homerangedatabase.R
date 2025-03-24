# install the HomeRange R package
# install.packages("https://github.com/SHoeks/HomeRange/raw/main/HomeRange_1.07.tar.gz",
#                  repos=NULL,
#                  method="libcurl")
# alternatively, install the HomeRange R package using install_github:
# remotes::install_github("SHoeks/HomeRange", subdir='pkg')
# or using the pak package manager
# pak::pkg_install("SHoeks/HomeRange/pkg")

# load package into R
library('HomeRange') # prints version
#> HomeRange pkg version: 1.07
#> HomeRange database version: 2024_07_09

# get data with the references attached
HomeRangeDataWithRefs <- GetHomeRangeData(IncludeReferences = TRUE)

library(stringr)
library(dplyr)
rodentData <- HomeRangeDataWithRefs[HomeRangeDataWithRefs$Species %in% c("Apodemus flavicollis",
                                                           "Myodes glareolus",
                                                           "Clethrionomys glareolus",
                                                           "Apodemus sylvaticus",
                                                           "Sciurus carolinensis",
                                                           "Sciurus vulgaris"),] %>%
  filter(Context == "Wild")

fallowData <- HomeRangeDataWithRefs[HomeRangeDataWithRefs$Species %in% c("Dama dama"),] %>%
  filter(Context == "Wild")

# "Clethrionomys glareolus" - is alt name for bank vole
names(rodentData)
table(rodentData$Tracking_Method)
table(rodentData$HR_Method_Simple)

rodentData %>%
  # filter(HR_Level == "Individual") %>%
  filter(Tracking_Method %in% c("Radio tracking",
                                "Radio tracking, direct observations",
                                "Radio tracking, live trapping",
                                "Satellite tracking (GPS), radio tracking")) %>%
  group_by(HR_Level, Species, HR_Method_Simple, Tracking_Method) %>%
  summarise(n = n()) %>%
  print(n = 500)

library(ggplot2)
library(ggridges)
library(ggdist)

rodentData %>%
  # filter(HR_Level == "Individual") %>%
  filter(Tracking_Method %in% c("Radio tracking",
                                "Radio tracking, direct observations",
                                "Radio tracking, live trapping",
                                "Satellite tracking (GPS), radio tracking")) %>%
  ggplot() +
  geom_density_ridges(aes(x = Home_Range_km2, y = HR_Method_Simple, fill = Species)) +
  scale_x_log10(labels = scales::comma) +
  facet_wrap(facet = vars(Species), scales = "free")

rodentData %>%
  # filter(HR_Level == "Individual") %>%
  filter(Tracking_Method %in% c("Radio tracking",
                                "Radio tracking, direct observations",
                                "Radio tracking, live trapping",
                                "Satellite tracking (GPS), radio tracking")) %>%
  group_by(Species) %>%
  mean_hdci(Home_Range_km2, prob = 0.95) %>%
  arrange(desc(Home_Range_km2))

fallowData %>%
  filter(HR_Level == "Individual") %>%
  filter(Tracking_Method %in% c("Radio tracking",
                                "Radio tracking, direct observations",
                                "Radio tracking, live trapping",
                                "Satellite tracking (GPS), radio tracking")) %>%
  ggplot() +
  geom_density_ridges(aes(x = Home_Range_km2, y = HR_Method_Simple, fill = Species)) +
  scale_x_log10(labels = scales::comma)

fallowData %>%
  filter(HR_Level == "Individual") %>%
  filter(Tracking_Method %in% c("Radio tracking",
                                "Radio tracking, direct observations",
                                "Radio tracking, live trapping",
                                "Satellite tracking (GPS), radio tracking")) %>%
  pull(Home_Range_km2) %>% max()
