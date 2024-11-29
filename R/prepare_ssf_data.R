#' Extract data from landscape layers for SSF
#'
#' @name prepare_ssf_data
#' @description Extract data from landscape ready for SSF models.
#' @return A list of available and used dataframes.
#'
#' @export
prepare_ssf_data <- function(deerData, landuseList, patchList,
                             nAvail = 10){

  ssfDataList <- vector("list", legnth = length(unique(deerData$Animal_ID)))
  names(ssfDataList) <- unique(deerData$Animal_ID)
  for(id in unique(deerData$Animal_ID)){

    focalDeer <- deerData %>%
      filter(Animal_ID == id) %>%
      st_as_sf(coords = c("x", "y"), remove = FALSE, crs = 27700)

    focalHR <- akdeLists$sf[[id]] %>%
      filter(level = conAvail)

    if(focalDeer$region[1] == "Aberdeenshire"){
      focalDistancePatch <- terra::rast(patchList$distanceAberdeen)
    } else {
      focalDistancePatch <- terra::rast(patchList$distanceWessex)
    }

    if(focalDeer$region[1] == "Aberdeenshire"){
      focalLand <- terra::rast(landuseList$Aberdeen$landuse)
    } else {
      focalLand <- terra::rast(landuseList$Wessex$landuse)
    }
    focalLand <- focalLand %>%
      mutate(LCM_1_cat = paste0("LCM_", LCM_1))


    availPoints <- sf::st_sample(, size = nrow(focalDeer) * nAvail,
                                 type = typeAvial)

    # could also be defined as an area surrounding the points
    availPoints <- terra::extract(focalDistance, availPoints, bind = TRUE)
    focalDeer <- terra::extract(focalDistance, focalDeer, bind = TRUE)

    availPoints <- terra::extract(focalLand, availPoints, bind = TRUE)
    focalDeer <- terra::extract(focalLand, focalDeer, bind = TRUE)

    availPoints$case_ <- 0
    focalDeer$case_ <- 1

    rsfDataList[[id]] <- list(
      availPoints = availPoints,
      usedDeer = focalDeer
    )

  }

  return(rsfDataList)

}
