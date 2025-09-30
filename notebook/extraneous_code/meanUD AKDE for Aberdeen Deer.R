
library(ctmm)
library(stringr)
library(dplyr)
library(sf)
library(raster)

targets::tar_load("tar_deerData")

deerData <- tar_deerData
unique(deerData$Animal_ID)

deerData <- deerData %>%
  mutate(
    site = case_when(
      str_detect(Animal_ID, "e02|e04|e06|e08|e13|e15") ~ "Dunecht",
      str_detect(Animal_ID, "e05|e10") ~ "Dinnet",
      str_detect(Animal_ID, "e09") ~ "Glendye",
      str_detect(Animal_ID, "e11|e12|e14") ~ "Wellhouse",
      TRUE ~ "Other"
    ))

deerData <- as.data.frame(deerData)

expansion <- 500
resolution <- 5

sites <- unique(deerData$site)
outList <- vector("list", length = length(sites))
names(outList) <- sites
for(s in sites){
  # s <- "Wellhouse"
  if(s == "Other"){
    {next}
  }

  siteData <- deerData[which(deerData$site == s),]

  teleObj <- ctmm::as.telemetry(siteData,
                                timeformat = "%Y-%m-%d %H:%M:%S",
                                timezone = "UTC")

  if(s == "Glendye"){
    teleObj <- list("Roe09_M" = teleObj)
  }

  print("teleObj")

  listVarioData <- lapply(teleObj, function(x){
    out <- ctmm::variogram(x, fast = FALSE, CI = "Gauss")
    return(out)
  })

  print("vario")

  listGuess <- lapply(teleObj, function(x){
    out <- ctmm::ctmm.guess(x, interactive = FALSE)
    return(out)
  })

  print("guess")

  listFits <- lapply(names(teleObj), function(x){
    # listFits <- lapply(names(teleObj)[1:2], function(x){
    print(x)
    out <- ctmm::ctmm.select(teleObj[[x]], listGuess[[x]], verbose = FALSE,
                             cores = 6, method = "pHREML")
    return(out)
  })
  names(listFits) <- names(teleObj)

  print("fit")

  movementExtent <- st_bbox(st_as_sf(siteData, coords = c("x", "y"), remove = FALSE,
                                     crs = st_crs("EPSG:27700")))

  blankRaster <- raster::raster(xmn = movementExtent[1] - expansion,
                                xmx = movementExtent[3] + expansion,
                                ymn = movementExtent[2] - expansion,
                                ymx = movementExtent[4] + expansion,
                                resolution = resolution,
                                crs = sp::CRS(SRS_string = 27700))
  blankRaster[] <- 0
  blankRaster <- raster::projectExtent(blankRaster, crs = teleObj[[1]]@info$projection)

  listAKDE <- lapply(names(teleObj), function(x){
    # listAKDE <- lapply(names(teleObj)[1:2], function(x){
    print(x)
    out <- ctmm::akde(teleObj[[x]], listFits[[x]],
                      weights = TRUE,
                      grid = blankRaster
    )
    return(out)
  })
  names(listAKDE) <- names(teleObj)

  meanUD <- mean(listAKDE)
  outList[[s]] <- meanUD
  # plot(meanUD)
  writeRaster(meanUD,
              filename = here::here("modelOutput", "ctmmUD", paste0(s, "_meanUD.tif")),
              format = "GTiff", overwrite = TRUE)

}


plot(outList$Glendye)
