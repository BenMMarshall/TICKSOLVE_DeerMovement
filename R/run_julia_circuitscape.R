#' Julia call wrapper to run circuitscape
#'
#' @name run_julia_circuitscape
#' @description abc
#' @return abc
#'
#' @export
run_julia_circuitscape <- function(model, circuitScapeData){

  # library(JuliaCall)
  # julia_install_package("Circuitscape")
  JuliaCall::julia_setup()
  JuliaCall::julia_install_package_if_needed("Circuitscape")
  JuliaCall::julia_library("Circuitscape")

  if(model == "pois"){
    iniFile <- here::here("data", "GIS data", "circuitscape", "circuitscapeSettings_pois.ini")
  } else  if(model == "ssf"){
    iniFile <- here::here("data", "GIS data", "circuitscape", "circuitscapeSettings_ssf.ini")
  }

  julia_command("using Circuitscape")
  JuliaCall::julia_call('compute', normalizePath(iniFile))

  circuitscapeLocations <- list.files(here("data", "GIS data", "circuitscape"), pattern = "pois.*?.asc$",
                                      full.names = TRUE)

  return(circuitscapeLocations)

}
