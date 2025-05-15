#' Read forest patche spatial data
#'
#' @name read_patches_data_WREN
#' @description Read in the previously created patch information.
#' @return A two object list, one object for each study location.
#'
#' @export
read_patches_data_WREN <- function(){

  patchesWREN <- read_sf(here("data", "GIS data", "patchesWREN.geoJSON"))

  patchList <- list("WREN" = patchesWREN)

  return(patchList)

}
