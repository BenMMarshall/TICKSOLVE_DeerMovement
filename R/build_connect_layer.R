#' Create raster using passage and resistance layer
#'
#' @name build_connect_layer
#' @description abc
#' @return abc
#'
#' @export
build_connect_layer <- function(predRasterLoc, patchList, akdeSummary, REGION, prelimAggFact = NA,
                                seed = 2025, THETA = NULL, repeatsPerPair, MSEdf = NULL){

  # targets::tar_load("tar_predPoisResist_location")
  # targets::tar_load("tar_patchList")
  # predRasterLoc <- tar_predPoisResist_location
  # focalPatches <- tar_patchList
  # REGION = "Aberdeenshire"
  # prelimAggFact = 20
  # seed = 2025
  # THETA = 0.01
  # repeatsPerPair = 1
  # patchDistance = 500

  patchDistance <- mean(as.numeric(akdeSummary$longestAxisSummary$longestAxisRange_m))

  if(!is.null(MSEdf)){
    meanMSE <- MSEdf %>%
      group_by(theta) %>%
      summarise(meanMSE = mean(mse))

    THETA <- meanMSE[meanMSE$meanMSE == min(meanMSE$meanMSE),]$theta
  }

  focalPatches <- patchList[[sub("shire", "", REGION)]]

  predictionTerra <- raster::raster(predRasterLoc)

  if(!is.na(prelimAggFact)){
    ## Agregate pixels to speed up the process during preliminary analyses.
    predictionTerra <- raster::aggregate(predictionTerra, fact = prelimAggFact, fun = mean)
  }

  ## Make a transition map (format needed by gdistance to represent the conductance map, i.e. 1/friction)
  transitionLayer <- gdistance::transition(predictionTerra, transitionFunction = min, directions = 8) # conductance between pixels = 1/friction map
  # tr #class TransitionLayer
  # plot(raster(tr))

  transitionLayer <- gdistance::geoCorrection(transitionLayer, multpl = FALSE, scl = TRUE)

  # Create the random points (here, 5 random points for each polygon)
  set.seed(seed)

  focalPatches <- focalPatches %>%
    filter(!duplicated(Ptch_ID))

  patchDistanceMatrix <- sf::st_distance(focalPatches)
  minDist <- patchDistance
  units(minDist) <- units::as_units("m")
  closePolys <- which(patchDistanceMatrix < minDist, arr.ind = TRUE)
  # remove the start ends that are the same patch
  closePolys <- closePolys[!apply(closePolys, 1, function(x) if(x[1] == x[2]) return(TRUE) else return(FALSE)),]
  closePolys <- closePolys[rep(seq_len(nrow(closePolys)), each = repeatsPerPair),]
  # closePolys[2,1]
  # closePolys[2,2]
  # startPoints <- st_sample(focalPatches[closePolys[2,1],], size = c(1,1), type = "random")
  # endPoints <- st_sample(focalPatches[closePolys[2,2],], size = c(1,1), type = "random")
  # focalPatches %>%
  #   dplyr::select(Ptch_ID) %>%
  #   ggplot() +
  #   geom_sf() +
  #   geom_sf(data = focalPatches[closePolys[2,1],], colour = "lightgreen") +
  #   geom_sf(data = focalPatches[closePolys[2,2],], colour = "pink") +
  #   geom_sf(data = startPoints, colour = "green") +
  #   geom_sf(data = endPoints, colour = "red")

  nPatchesClose <- nrow(closePolys)

  pas <- NULL
  set.seed(0)
  sttim <- Sys.time()
  for (i in c(1:nPatchesClose)){
    # i <- 4
    print(paste(i, "/", nPatchesClose))
    # pt_win <- sample(rndpts$id, size=1, prob = (rndpts$val_win)) #draw random points (random pairs)
    # pt_cal <- sample(rndpts$id, size=1, prob = (rndpts$val_cal))
    # pt_win <- xyFromCell(kud_win, pt_win) #get xy for each point
    # pt_cal <- xyFromCell(kud_cal, pt_cal)
    stPoint <- as(st_sample(focalPatches[closePolys[i,1],], size = c(1,1), type = "random"), "Spatial")
    edPoint <- as(st_sample(focalPatches[closePolys[i,2],], size = c(1,1), type = "random"), "Spatial")
    # make map
    pasT <- try(gdistance::passage(transitionLayer, stPoint, edPoint, theta = THETA,
                                   totalNet = "net"), silent = FALSE)
    # plot(pasT)
    # NB: to obtain the different models described in the paper, change theta values as described in the
    # paper. For simplicity here we produce only one model
    if (class(pasT)!="try-error"){
      if (mean(values(pasT)<Inf, na.rm=T)>0.1){
        if (is.null(pas)) pas <- pasT
        if (!is.null(pas)) pas <- pas + pasT
      }}
  }
  (endtim <- Sys.time()-sttim)
  # par(mfrow=c(1,2))
  # plot(raster(tr), col=grey(c(100:0)/100))
  # plot(pas, col=grey(c(100:0)/100))

  pas_terra <- as(pas, "SpatRaster")

  ggplot() +
    geom_spatraster(data = pas_terra, aes(fill = layer)) +
    # geom_sf(data = focalPatches, alpha = 0.1) +
    # geom_sf(data = focalRoads, colour = "#000000", alpha = 0.25) +
    coord_sf(xlim = c(max(c(ext(focalPatches)[1],
                            ext(pas_terra)[1])),
                      min(c(ext(focalPatches)[2],
                            ext(pas_terra)[2]))),
             ylim = c(max(c(ext(focalPatches)[3],
                            ext(pas_terra)[3])),
                      min(c(ext(focalPatches)[4],
                            ext(pas_terra)[4]))),
             expand = 0)

  connectRasterLoc <- here("data", "GIS data",
                           paste0("connectTerra", str_extract(predRasterLoc, pattern = "SSF|Pois"),
                                  "_", sub("shire", "", REGION), "_theta_", THETA, ".tif"))

  terra::writeRaster(pas,
                     filename = connectRasterLoc,
                     overwrite = TRUE)

  return(connectRasterLoc)

}
