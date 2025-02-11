#' Create raster using passage and resistance layer
#'
#' @name build_connect_layer
#' @description abc
#' @return abc
#'
#' @export
build_connect_layer <- function(predRasterLoc, patchList, akdeSummary, REGION, prelimAggFact = NA,
                                seed = 2025, THETA = NULL, repeatsPerPair, MSEdf = NULL,
                                MINPATCHSIZE, cropArea = 750, cores = 4){

  # library(here)
  # library(dplyr)
  # library(sf)
  # library(terra)
  # library(stringr)
  # library(doParallel)
  # library(foreach)
  # targets::tar_load("tar_predPoisResist_location")
  # targets::tar_load("tar_predPoisResist_locationWessex")
  # targets::tar_load("tar_patchList")
  # targets::tar_load("tar_akdeSummary")
  # predRasterLoc <- tar_predPoisResist_locationWessex
  # predRasterLoc <- tar_predPoisResist_location
  # akdeSummary <- tar_akdeSummary
  # patchList <- tar_patchList
  # REGION = "Wessex"
  # REGION = "Aberdeenshire"
  # prelimAggFact = 2
  # prelimAggFact = NA
  # seed = 2025
  # THETA = 1e-05
  # repeatsPerPair = 1
  # patchDistance = 750
  # MINPATCHSIZE <- 1700000
  # MINPATCHSIZE <- 5000
  # cropArea <- 750
  # MSEdf <- NULL
  # cores = 8

  connectRasterLoc <- here("data", "GIS data",
                           paste0("connectTerra", str_extract(predRasterLoc, pattern = "SSF|Pois"),
                                  "_", sub("shire", "", REGION), "_theta_", THETA, ".tif"))

  currFiles <- list.files(here("data", "GIS data"),
                          pattern = paste0("connectTerra", str_extract(predRasterLoc, pattern = "SSF|Pois"),
                                           "_", sub("shire", "", REGION), "_theta_", THETA, ".tif"), full.names = TRUE)

  if(connectRasterLoc %in% currFiles){

    return(connectRasterLoc)

  } else {

    patchDistance <- mean(as.numeric(akdeSummary$longestAxisSummary$longestAxisRange_m))/2

    if(!is.null(MSEdf)){
      meanMSE <- MSEdf %>%
        group_by(theta) %>%
        summarise(meanMSE = mean(mse))

      THETA <- meanMSE[meanMSE$meanMSE == min(meanMSE$meanMSE),]$theta
    }

    focalPatches <- patchList[[sub("shire", "", REGION)]]

    focalPatches <- focalPatches %>%
      mutate(area_m = as.numeric(st_area(.))) %>%
      filter(area_m > MINPATCHSIZE)

    predictionTerra <- raster::raster(predRasterLoc)

    if(!is.na(prelimAggFact)){
      ## Agregate pixels to speed up the process during preliminary analyses.
      predictionTerra <- raster::aggregate(predictionTerra, fact = prelimAggFact, fun = mean)
    }

    ## Make a transition map (format needed by gdistance to represent the conductance map, i.e. 1/friction)
    # transitionLayer <- gdistance::transition(predictionTerra, transitionFunction = min, directions = 8) # conductance between pixels = 1/friction map
    # tr #class TransitionLayer
    # plot(raster(tr))
    # rm(predictionTerra)
    # transitionLayer <- gdistance::geoCorrection(transitionLayer, multpl = FALSE, scl = TRUE)

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
    rm(patchDistanceMatrix)
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
    pas_terra <- predictionTerra
    pas_terra[] <- 0
    pas_terra <- rast(pas_terra)

    #setup parallel backend to use many processors
    # cores <- detectCores()
    cl <- makeCluster(cores)
    # cl <- makeCluster(cores[1]-4) #not to overload your computer
    registerDoParallel(cl)

    sttim <- Sys.time()
    # passageList <- list()
    # for(i in 1:nPatchesClose){
    # passageList <- foreach(i = 1:100, .errorhandling = "pass") %dopar% {
    passageList <- foreach(i = 1:nPatchesClose, .errorhandling = "pass") %dopar% {
      # i <- 1
      pointDist <- units::set_units(patchDistance*20, "m")
      print(paste(i, "/", nPatchesClose))
      # check distance between locations
      while(pointDist > units::set_units(patchDistance*8, "m")){
        stPoint <- sf::st_sample(focalPatches[closePolys[i,1],], size = c(1,1), type = "random")
        edPoint <- sf::st_sample(focalPatches[closePolys[i,2],], size = c(1,1), type = "random")
        pointDist <- sf::st_distance(stPoint, edPoint)
        print(pointDist[,1])
      }

      stPoint <- as(stPoint, "Spatial")
      edPoint <- as(edPoint, "Spatial")
      spBBox <- sp::bbox(rbind(stPoint, edPoint))

      # if distance is greater than crop area addition we need to make sure the box is
      # big enough to diffuse paths, this can be problematic when points are distant
      # (cos of big patches) and with very little variation in x or y. We can instead
      # expand the bbox crop box by half the distance between points to allow the more
      # diffuse paths, at the cost of computation (so only do this IF needed, otherwise use cropArea).
      if(pointDist > units::set_units(cropArea, "m")){
        shorterAxis <- ifelse((spBBox[3]-spBBox[1]) < (spBBox[4]-spBBox[2]), "x", "y")

        if(shorterAxis == "x"){
          spBBox <- spBBox + c(-as.numeric(units::set_units(pointDist, "m"))/2,
                               -cropArea,
                               as.numeric(units::set_units(pointDist, "m"))/2,
                               cropArea)
        } else {
          spBBox <- spBBox + c(-cropArea,
                               -as.numeric(units::set_units(pointDist, "m"))/2,
                               cropArea,
                               as.numeric(units::set_units(pointDist, "m"))/2)
        }
      } else {
        spBBox <- spBBox + c(-cropArea, -cropArea, cropArea, cropArea)
      }

      # quick clean up to make sure the max and min extents remain within the overall patch space
      patchBB <- sf::st_bbox(focalPatches)
      if(spBBox[1] < patchBB[1]){spBBox[1] <- patchBB[1]}
      if(spBBox[2] < patchBB[2]){spBBox[2] <- patchBB[2]}
      if(spBBox[3] > patchBB[3]){spBBox[3] <- patchBB[3]}
      if(spBBox[4] > patchBB[4]){spBBox[4] <- patchBB[4]}

      cropPred <- raster::crop(predictionTerra,
                               raster::extent(spBBox[1],
                                              spBBox[3],
                                              spBBox[2],
                                              spBBox[4]))
      transitionLayer <- gdistance::transition(cropPred, transitionFunction = min, directions = 8) # conductance between pixels = 1/friction map
      transitionLayer <- gdistance::geoCorrection(transitionLayer, multpl = FALSE, scl = TRUE)

      # make map
      # sttim2 <- Sys.time()
      pasT <- try(gdistance::passage(transitionLayer, stPoint, edPoint, theta = THETA,
                                     totalNet = "net"), silent = FALSE)
      # (endtim2 <- Sys.time()-sttim2)
      # print(endtim2)

      # passageList[[i]] <- pasT
    }
    stopCluster(cl)
    (endtim <- Sys.time()-sttim)

    # Number of iterations
    imax<-c(length(passageList))
    pb <- txtProgressBar(min = 0, max = imax, style = 3)
    i <- 0
    for(pass in passageList){
      i <- i+1
      setTxtProgressBar(pb, i)
      passCurr <- terra::rast(pass)
      crs(passCurr) <- crs(pas_terra)
      pas_terra <- pas_terra + terra::extend(passCurr, pas_terra,
                                             fill = 0)
    }
    # plot(pas_terra)

    # ggplot() +
    #   geom_spatraster(data = pas_terra, aes(fill = pred)) +
      # geom_spatraster(data = rast(predictionTerra), aes(fill = pred)) +
      # geom_sf(data = focalPatches, alpha = 0.1) +
    #   geom_sf(data = sf::st_as_sf(rbind(stPoint, edPoint)), colour = "red", size = 2) +
    #   # geom_sf(data = focalRoads, colour = "#000000", alpha = 0.25) +
    #   coord_sf(xlim = c(max(c(ext(focalPatches)[1],
    #                           ext(pas_terra)[1])),
    #                     min(c(ext(focalPatches)[2],
    #                           ext(pas_terra)[2]))),
    #            ylim = c(max(c(ext(focalPatches)[3],
    #                           ext(pas_terra)[3])),
    #                     min(c(ext(focalPatches)[4],
    #                           ext(pas_terra)[4]))),
    #            expand = 0, datum = sf::st_crs(27700))

    terra::writeRaster(pas_terra,
                       filename = connectRasterLoc,
                       overwrite = TRUE)

    return(connectRasterLoc)

  }

}
