#' Create rasters suitable for the circuitscape
#'
#' @name prepare_circuitscape_data
#' @description abc
#' @return abc
#'
#' @export
prepare_circuitscape_data <- function(predRasterLoc, patchList, REGION, prelimAggFact = 10, patchDistance){

  predictionTerra <- terra::rast(predRasterLoc)

  focalPatches <- patchList[[sub("shire", "", REGION)]]

  focalPatches <- focalPatches %>%
    filter(!duplicated(Ptch_ID)) %>%
    mutate(NumID = row_number())

  patchDistanceMatrix <- sf::st_distance(focalPatches)
  minDist <- patchDistance
  units(minDist) <- units::as_units("m")

  rowInclusionList <- vector("list", nrow(patchDistanceMatrix))
  # for(r in 1:nrow(patchDistanceMatrix)){
  for(r in 1:1){
    # r <- 1
    rowInclusionList[[r]] <- data.frame(mode = r,
                                        include = focalPatches$NumID[patchDistanceMatrix[r,] < minDist]) %>%
      filter(!include == r)
  }
  pairsInclusion <- do.call(rbind, rowInclusionList)

  # pairsInclusion <- expand.grid(mode = unique(values(binaryRaster)),
  #             include = unique(values(binaryRaster))) %>%
  #   filter(!is.na(mode)) %>%
  #   filter(!is.na(include))

  write.table(pairsInclusion, file = here::here("data", "GIS data", "circuitscape", "pairInclusion.txt"),
              row.names = FALSE, sep = '\t', quote = FALSE)

  # extent_m <- terra::ext(focalPatches)
  # # will result in a grid that has a 1 m x 1 m res
  # xRes <- abs(extent_m[1] - extent_m[2])
  # yRes <- abs(extent_m[3] - extent_m[4])

  template <- terra::rast(predictionTerra)

  binaryRaster <- terra::rasterize(vect(focalPatches),
                                   template, field = "NumID")

  # binaryRaster <- binaryRaster %>%
  #   dplyr::mutate(NumID = as.numeric(NumID))

  patchCircuitTerraLoc <- here::here("data", "GIS data", "nodesCircuitTerra_Aberdeen.tif")
  predictionCircuitTerraLoc <- here::here("data", "GIS data", "predictionCircuitTerra_Aberdeen.tif")

  predictionTerra <- raster::aggregate(predictionTerra, fact = 10, fun = max)
  binaryRaster <- raster::aggregate(binaryRaster, fact = 10, fun = max)

  terra::writeRaster(binaryRaster, patchCircuitTerraLoc,
                     overwrite = TRUE)
  terra::writeRaster(predictionTerra, predictionCircuitTerraLoc,
                     overwrite = TRUE)

  fileList <- list(
    predictionCircuitTerra = predictionCircuitTerraLoc,
    nodesCircuitTerra = patchCircuitTerraLoc
  )

  return(fileList)
}
