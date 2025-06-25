
targets::tar_load("tar_akdeLists")
library(ctmm)

plot(tar_akdeLists$akde$Roe01_F)

writeRaster(tar_akdeLists$akde$Roe01_F,
            filename = here::here("modelOutput", "ctmmUD", "ctmmUD_Roe01.tif"),
            format = "GTiff", DF = "CDF", overwrite = TRUE)

library(terra)
test <- terra::rast(here::here("modelOutput", "ctmmUD", "ctmmUD_Roe01.tif"))
plot(test)
