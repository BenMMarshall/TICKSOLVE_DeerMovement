# install.packages(c("foreach", "doParallel"))

library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(1)
# cl <- makeCluster(cores[1]-4) #not to overload your computer
registerDoParallel(cl)

# .combine = "+"

sttim <- Sys.time()
passageList <- foreach(i = 1:5) %dopar% {
  print(paste(i, "/", nPatchesClose))
  stPoint <- as(sf::st_sample(focalPatches[closePolys[i,1],], size = c(1,1), type = "random"), "Spatial")
  edPoint <- as(sf::st_sample(focalPatches[closePolys[i,2],], size = c(1,1), type = "random"), "Spatial")
  # make map
  pasT <- try(gdistance::passage(transitionLayer, stPoint, edPoint, theta = THETA,
                                 totalNet = "net"), silent = FALSE)
}
stopCluster(cl)
(endtim <- Sys.time()-sttim)


# plot(pasT)
if (class(pasT)!="try-error"){
  if (mean(values(pasT)<Inf, na.rm=T)>0.1){
    if (is.null(pas)) pas <- pasT
    if (!is.null(pas)) pas <- pas + pasT
  }}

#stop cluster

# 5 passage at agg 20 takes 14.12 - with 15 threads
14.12/5 * 17838 /60/60
# 100 passage at agg 20 takes 1.531827 mins
1.531827/100 * 17838 /60
# 100 passage at agg 10 takes 1.086671 mins
1.086671/100 * 17838 /60
# 5 passage at agg 0 takes ___ seconds - 12 threads
___/5 * 17838 /60/60

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
