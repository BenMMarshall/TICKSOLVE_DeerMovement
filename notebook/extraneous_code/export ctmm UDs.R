
targets::tar_load("tar_akdeLists")
library(ctmm)

plot(tar_akdeLists$akde$Roe01_F)

for(indi in names(tar_akdeLists$akde)){
  plot(tar_akdeLists$akde[[indi]])
  writeRaster(tar_akdeLists$akde[[indi]],
              filename = here::here("modelOutput", "ctmmUD", paste0("ctmmUD_", indi, ".tif")),
              format = "GTiff", DF = "CDF", overwrite = TRUE)
}


library(terra)
test <- terra::rast(here::here("modelOutput", "ctmmUD", "ctmmUD_Roe01.tif"))
plot(test)
