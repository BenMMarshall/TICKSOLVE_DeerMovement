#' Save the EMmeanbyTSS projection
#'
#' @name save_proj_layer
#' @description abc
#' @return abc
#'
#' @export
save_proj_layer <- function(forecast){

  # targets::tar_load("tar_biomodForecast_fallow")
  # here::here("Dama.dama", "models", "ALLModels", tar_biomodForecast@models.projected[1])
  # plot(rast(here::here("Dama.dama", "proj_CurrentEM", "proj_CurrentEM_Dama.dama_ensemble.tif")))
  # list.files(here::here("Dama.dama", "models", "ALLModels"))
  # forecast <- tar_biomodForecast_fallow

  forecastTerra <- rast(here::here(forecast@sp.name,
                                   paste0("proj_", forecast@proj.name),
                                   paste0("proj_", forecast@proj.name, "_", forecast@sp.name, "_ensemble.tif")))

  projLayer <- forecastTerra %>%
    dplyr::select(contains("EMwmeanByTSS"))

  # convert the 0-1000 to 0-1
  values(projLayer) <- values(projLayer)/1000

  projLayerLoc <- here("data", "GIS data",
                      paste0("projTerra_", sub("CurrentEM_", "", forecast@proj.name), ".tif"))

  terra::writeRaster(projLayer,
                     filename = projLayerLoc,
                     overwrite = TRUE)

  return(projLayerLoc)
}
