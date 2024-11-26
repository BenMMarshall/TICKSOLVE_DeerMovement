#' Read forest patche spatial data
#'
#' @name read_patches_data
#' @description Read in the previously created patch information.
#' @return A two object list, one object for each study location.
#'
#' @export
read_patches_data <- function(){
  patchesWessex <- read_sf(here("data", "GIS data", "patchesWessex.geoJSON"))
  patchesAberdeen <- read_sf(here("data", "GIS data", "patchesAberdeen.geoJSON"))

  return(list("aber" = patchesAberdeen,
              "wess" = patchesWessex))
}
