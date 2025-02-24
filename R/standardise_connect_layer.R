#' Set connect layers between 0 and 1
#'
#' @name standardise_connect_layer
#' @description abc
#' @return abc
#'
#' @export
standardise_connect_layer <- function(connectRasterLoc,
                                      REGION,
                                      THETA = NULL, MSEdf = NULL){

  # library(terra)
  # library(tidyterra)
  # library(sf)
  # library(here)
  # library(ggplot2)
  # library(patchwork)
  # library(bestNormalize)
  #
  # targets::tar_load("tar_connectPois_location_1e.05_1")
  # connectTerra <- terra::rast(tar_connectPois_location_1e.05_1)
  # connectTerraPois_Aberdeen_theta_1e-04
  connectTerra <- terra::rast(connectRasterLoc)
  names(connectTerra) <- "connectivity"
  standardise_01 <- function(x){(x-min(x))/(max(x)-min(x))}

  if(!is.null(MSEdf)){
    meanMSE <- MSEdf %>%
      group_by(theta) %>%
      summarise(meanMSE = mean(mse))
    THETA <- meanMSE[meanMSE$meanMSE == min(meanMSE$meanMSE),]$theta
  }

  # bestNormResults <- bestNormalize(
  #   x = values(connectTerra),
  #   standardize = TRUE,
  #   allow_exp = TRUE,
  #   # allow_orderNorm = FALSE,
  #   # out_of_sample = FALSE,
  #   k = 10,
  #   r = 5,
  # )
  #
  # saveRDS(bestNormResults, here("data", "modelOutput",
  #                               paste0("bestNormResults_", REGION, "_theta_", THETA, ".rds")))
  #
  # values(connectTerra) <- standardise_01(bestNormResults$x.t)

  values(connectTerra) <- standardise_01(values(connectTerra))

  connectRasterStanLoc <- here("data", "GIS data",
                           paste0("connectTerraStandard", str_extract(connectRasterLoc, pattern = "SSF|Pois"),
                                  "_", sub("shire", "", REGION), "_theta_", THETA, ".tif"))

  terra::writeRaster(connectTerra,
                     filename = connectRasterStanLoc,
                     overwrite = TRUE)

  return(connectRasterStanLoc)

}
