# install.packages(c("foreach", "doParallel"))

library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(8)
# cl <- makeCluster(cores[1]-4) #not to overload your computer
registerDoParallel(cl)

cropArea <- 750

# .combine = "+"
pas_terra <- predictionTerra
pas_terra[] <- 0
pas_terra <- rast(pas_terra)

sttim <- Sys.time()
passageList <- foreach(i = 2000:7000) %dopar% {
  print(paste(i, "/", nPatchesClose))
  stPoint <- sf::st_sample(focalPatches[closePolys[i,1],], size = c(1,1), type = "random")
  edPoint <- sf::st_sample(focalPatches[closePolys[i,2],], size = c(1,1), type = "random")
  pointDist <- sf::st_distance(stPoint, edPoint)

  stPoint <- as(stPoint, "Spatial")
  edPoint <- as(edPoint, "Spatial")
  spBBox <- sp::bbox(rbind(stPoint, edPoint))

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
                           spBBox)
  transitionLayer <- gdistance::transition(cropPred, transitionFunction = min, directions = 8) # conductance between pixels = 1/friction map
  transitionLayer <- gdistance::geoCorrection(transitionLayer, multpl = FALSE, scl = TRUE)

  # make map
  pasT <- try(gdistance::passage(transitionLayer, stPoint, edPoint, theta = THETA,
                                 totalNet = "net"), silent = FALSE)
}
stopCluster(cl)
(endtim <- Sys.time()-sttim)

# lapply(passageList[1:50], plot)

# Number of iterations
imax<-c(length(passageList))
pb <- txtProgressBar(min = 0, max = imax, style = 3)
i <- 0
for(pass in passageList){
  i <- i+1
  setTxtProgressBar(pb, i)
  passCurr <- rast(pass)
  crs(passCurr) <- crs(pas_terra)
  pas_terra <- pas_terra + terra::extend(passCurr, pas_terra,
                                         fill = 0)
}
plot(pas_terra)


# plot(terra::extend(pass1, emptyRaster))
# plot(pass1)
# plot(emptyRaster + terra::extend(pass1, emptyRaster))

# plot(pasT)
if (class(pasT)!="try-error"){
  if (mean(values(pasT)<Inf, na.rm=T)>0.1){
    if (is.null(pas)) pas <- pasT
    if (!is.null(pas)) pas <- pas + pasT
  }}

#stop cluster

# 5 passage at agg 20 takes 14.12 - with 15 threads
14.12/5 * 17838 /60/60
# 100 passage at agg 20 takes 1.531827 mins - with 15 threads
1.531827/100 * 17838 /60
# 100 passage at agg 10 takes 1.086671 mins - with 15 threads
1.086671/100 * 17838 /60
# 5 passage at agg 2 takes 14.21714 mins - 2 threads - each using ~8gb
14.21714/5 * 17838 /60
# 5 passage at agg 2 takes 3.401 seconds - 2 threads - using the cropped area
3.401/5 * 17838 /60/60
# 5 passage at agg 0 takes 4.01076 seconds - 2 threads - using the cropped area
4.01076/5 * 17838 /60/60
# 100 passage at agg 0 takes 11.04296 seconds - 8 threads - using the cropped area
11.04296/100 * 17838 /60/60
# 100 passage at agg 0 takes 12.74564 seconds - 12 threads - using the cropped area 1000
12.74564/100 * 17838 /60/60
# 2000 passage at agg 0 takes 3.73784 minutes - 12 threads - using the cropped area 1000
3.73784/100 * 17838 /60
# 2000 passage at agg 0 takes 3.662112  minutes - 12 threads - using the cropped area 1000
3.662112/100 * 17838 /60
# 2000 passage at agg 0 takes 2.894549 minutes - 12 threads - using the cropped area 500
2.894549/100 * 17838 /60
# 50 passage at agg 0 takes 12.24223 seconds - 8 threads - using the cropped area 750
12.24223/100 * 17838 /60/60
# 1000 passage at agg 0 takes 9.512232 mins - 8 threads - using the cropped area 750, with expand if needed
9.512232/1000 * 17838 /60
# 5000 passage at agg 0 takes 9.100612 mins - 8 threads - using the cropped area 750, with expand if needed
9.100612/5000 * 17838 /60
# 1000 passage at agg 0 takes 2.967935 mins - 8 threads - using the cropped area
# 750, with expand if needed, distance limited to 8 times half HR
2.967935/1000 * 17838 /60
## slowest passage calcs come as a result of the long distances between start
## and end points - e.g 20km ~ 5 mins whereas 5 km ~ 5 seconds

# 5 passage at agg 10 takes 14.12 - with 15 threads
14.12/5 * 17838 /60/60

# 1 passage at agg 20 takes 4.8 seconds
4.8 * 17838 /60/60
# 5 passage at agg 20 takes 22.24
22.24/5 * 17838 /60/60



#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

finalMatrix <- foreach(i=1:150000, .combine=cbind) %dopar% {
  tempMatrix = functionThatDoesSomething() #calling a function
  #do other things if you want

  tempMatrix #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
}
#stop cluster
stopCluster(cl)
