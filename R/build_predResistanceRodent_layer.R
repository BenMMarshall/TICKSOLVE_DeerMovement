#' Create raster based on the mean SSF coefs
#'
#' @name build_predResistanceRodent_layer
#' @description abc
#' @return abc
#'
#' @export
build_predResistanceRodent_layer <- function(projLayer,
                                             prelimAggFact){

  projTerra <- terra::rast(projLayer)
  ## To avoid problems with 0's in RSP, we set them to a very small value
  values(projTerra) <- ifelse(values(projTerra) == 0, 0.0000000000001, values(projTerra))

  landuseGB <- terra::rast(here("data", "GIS data", "UKCEH_Landcover",
                                "UKCEH_Landcover2023_GB_25res",
                                "data", "gblcm2023_25m.tif"))
  names(landuseGB) <- c("LCM_1", "LCM_2", "LCM_3")
  landuseGB <- landuseGB %>%
    dplyr::select(LCM_1)
  # landuseGB <- terra::crop(landuseGB, st_bbox(patchList$GB))
  # pull out OTHER to trim the other layers, remove sea etc
  seaNAcut <- classify(landuseGB,
                       rcl = matrix(c(1, 21, 1), nrow = 1, ncol = 3))

  if(prelimAggFact >= 1 & !is.null(prelimAggFact) & !is.na(prelimAggFact)){
    seaNAcut <- terra::aggregate(seaNAcut, fact = prelimAggFact,
                                 fun = "max")
  }
  print("Sea NA complete")

  values(projTerra) <- 1 - values(projTerra)

  names(projTerra) <- "resistance"

  predRasterLoc <- here("data", "GIS data", paste0("predictionTerraRodent_", str_extract(projLayer, "wessex|aberdeen"), ".tif"))

  terra::writeRaster(projTerra,
                     filename = predRasterLoc,
                     overwrite = TRUE)

  return(predRasterLoc)

}
