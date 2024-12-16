#' Compare connectivity layer with all dBBMMs
#'
#' @name calculate_dbbmm_mse
#' @description abc
#' @return abc
#'
#' @export
calculate_dbbmm_mse <- function(deerData, dbbmmList, connectRasterLoc, REGION, THETA){

  # targets::tar_load("tar_dbbmmList")
  # REGION
  # connectRasterLoc
  # dbbmmList <- tar_dbbmmList

  connectRaster <- raster::raster(connectRasterLoc)

  focalDeer <- deerData %>%
    filter(region == REGION)

  focalDeerIDs <- unique(focalDeer$Animal_ID)

  MSEList <- vector("list", length = length(focalDeerIDs))
  names(MSEList) <- focalDeerIDs
  for(id in focalDeerIDs){
    print(id)
    dbbmm <- dbbmmList[[id]]
    resampledDBBMM <- resample(dbbmm, connectRaster)
    values(connectRaster) <- ifelse(is.na(values(connectRaster)), 0, values(connectRaster))
    values(connectRaster) <- values(connectRaster)/sum(values(connectRaster), na.rm = TRUE) # rescale pas model
    sum(values(connectRaster)) #1

    values(resampledDBBMM) <- ifelse(is.na(values(resampledDBBMM)), 0, values(resampledDBBMM))
    plot(resampledDBBMM)

    MSE <- mean((values(resampledDBBMM)-values(connectRaster))^2) ## for each pixel, calculate the Mean Square Error MSE. Compute the results for different models and compare

    MSEList[[id]] <- MSE
  }
  MSEdf <- as.data.frame(do.call(rbind, MSEList))
  MSEdf$Animal_ID <- row.names(MSEdf)
  MSEdf$theta <- THETA
  return(MSEdf)

}
