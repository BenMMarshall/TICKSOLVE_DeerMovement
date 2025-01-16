#' Create rasters suitable for the circuitscape
#'
#' @name prepare_circuitscape_data
#' @description abc
#' @return abc
#'
#' @export
prepare_circuitscape_data <- function(predRasterLoc, patchList, REGION, prelimAggFact = 10, patchDistance){

  # targets::tar_load("tar_predPoisResist_location")
  # targets::tar_load("tar_patchList")
  # REGION = "Aberdeenshire"
  # prelimAggFact = 10
  # patchDistance = 1000
  # predRasterLoc <- tar_predPoisResist_location
  # patchList <- tar_patchList

  predictionTerra <- terra::rast(predRasterLoc)

  focalPatches <- patchList[[sub("shire", "", REGION)]]

  template <- terra::rast(predictionTerra)

  binaryRaster <- terra::rasterize(vect(focalPatches %>% mutate(Ptch_ID = as.numeric(Ptch_ID))),
                                   template, field = "Ptch_ID")

  unique(as.character(values(binaryRaster$Ptch_ID)))

  focalPatches <- focalPatches %>%
    filter(Ptch_ID %in% unique(as.character(values(binaryRaster$Ptch_ID)))) %>%
    mutate(NumID = row_number())

  patchDistanceMatrix <- sf::st_distance(focalPatches)
  minDist <- patchDistance
  units(minDist) <- units::as_units("m")

  ### TO TURN ON ABOVE EDIT THE ini model file.

  # matrix distance methods -------------------------------------------------
  # also not working, just returns zeroes

  patchDistanceMatrix <- apply(patchDistanceMatrix, 2, as.numeric)
  patchDistanceMatrix <- apply(patchDistanceMatrix, 2, round)
  # patchDistanceMatrix <- apply(patchDistanceMatrix, 2, as.integer)

  write.table(patchDistanceMatrix, file = here::here("data", "GIS data", "circuitscape", "pairwiseDistances.txt"),
              row.names = FALSE, col.names = FALSE, sep = '\t', quote = FALSE)

  minMaxText <- paste("min 1\nmax", patchDistance)
  pairwiseDistancesTxt <- readLines(here::here("data", "GIS data", "circuitscape", "pairwiseDistances.txt"))
  pairwiseDistancesTxt <- c(minMaxText, pairwiseDistancesTxt)
  writeLines(pairwiseDistancesTxt, here::here("data", "GIS data", "circuitscape", "pairwiseDistances.txt"))


  # exclusions based on distance --------------------------------------------
  # circuitscape julia this doesn't work: https://github.com/Circuitscape/Circuitscape.jl/issues/341
  # known issue
  rowInclusionList <- vector("list", nrow(patchDistanceMatrix))
  for(r in unique(focalPatches$Ptch_ID)){
    # r <- unique(focalPatches$Ptch_ID)[1]
    rowInclusionList[[r]] <- data.frame(mode = r,
                                        include = focalPatches$Ptch_ID[patchDistanceMatrix[which(unique(focalPatches$Ptch_ID) == r),] < minDist]) %>%
      filter(!include == r)
  }
  pairsInclusion <- do.call(rbind, rowInclusionList)

  write.table(pairsInclusion, file = here::here("data", "GIS data", "circuitscape", "pairInclusion.txt"),
              row.names = FALSE, sep = '\t', quote = FALSE)

  patchCircuitTerraLoc <- here::here("data", "GIS data", "nodesCircuitTerra_Aberdeen.tif")
  predictionCircuitTerraLoc <- here::here("data", "GIS data", "predictionCircuitTerra_Aberdeen.tif")

  # predictionTerra <- raster::aggregate(predictionTerra, fact = 10, fun = max)
  # binaryRaster <- raster::aggregate(binaryRaster, fact = 10, fun = max)

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
