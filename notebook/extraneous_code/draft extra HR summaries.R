
library(dplyr)

targets::tar_load("tar_deerData")
deerData <- tar_deerData
akdeLists <- tar_akdeLists

allAreas <- do.call(rbind, akdeLists$area) %>%
  left_join(deerData %>%
              group_by(Animal_ID, region, Sex) %>%
              slice_head(n = 1) %>%
              dplyr::select(Animal_ID, region, Sex))

allAreas

allAreas %>%
  filter(level == 0.95) %>%
  filter(region == "Aberdeenshire")

plot(akdeLists$vario$Fallow02_F)

lapply(names(akdeLists$vario), function(x){
  png(filename = here::here("modelOutput", paste0("variogram_", x, ".png")))
  plot(akdeLists$vario[[x]])
  title(x)
  dev.off()
})
