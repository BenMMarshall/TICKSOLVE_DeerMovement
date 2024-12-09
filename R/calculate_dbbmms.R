#' Create dbbmms to test against distance maps
#'
#' @name calculate_dbbmms
#' @description abc
#' @return abc
#'
#' @export
calculate_dbbmms <- function(deerData, landuseList, window, margin, locationError){
  # window <- windowSize
  # margin <- marginSize

  crs27700 <- sp::CRS(SRS_string = "EPSG:27700")

  deerIDs <- unique(deerData$Animal_ID)
  dbbmmList <- vector("list", length = length(deerIDs))
  names(dbbmmList) <- deerIDs
  for(id in deerIDs){
    # id <- deerIDs[2]
    print(id)
    focalDeer <- deerData %>%
      dplyr::filter(Animal_ID == id)

    focalRegion <- deerData[deerData$Animal_ID == id,]$region[1]

    if(focalRegion == "Aberdeenshire"){
      focalLand <- raster::raster(landuseList$Aberdeen$landuse)
    } else {
      focalLand <- raster::raster(landuseList$Wessex$landuse)
    }

    moveObj <- move(x = focalDeer$x, y = focalDeer$y,
                    time = focalDeer$datetime,
                    proj = crs27700@projargs)

    # set_grid.ext <- 4
    # set_dimsize <- 640
    dbbmm <- brownian.bridge.dyn(object = moveObj,
                                 raster = res(focalLand)[1],
                                 location.error = locationError,
                                 ext = 2,
                                 # dimSize = set_dimsize,
                                 window.size = window,
                                 margin = margin)

    values(dbbmm) <- values(dbbmm)/sum(values(dbbmm), na.rm = TRUE)

    dbbmmList[[id]] <- dbbmm

  }
  return(dbbmmList)

}
