#' Create wrsf models and wrsf adjusted AKDEs
#'
#' @name run_wrsf_models
#' @description abc
#' @return abc
#'
#' @export
run_wrsf_models <- function(deerData,
                            akdeLists,
                            landuseList,
                            REGION,
                            error = 0.001){

  # library(here)
  # library(dplyr)
  # library(raster)
  # library(terra)
  # library(tidyterra)
  # library(ctmm)

  # targets::tar_load("tar_akdeLists")
  # targets::tar_load("tar_landuseList")
  # targets::tar_load("tar_deerData")

  # akdeLists <- tar_akdeLists
  # landuseList <- tar_landuseList
  # deerData <- tar_deerData
  # REGION = "Aberdeenshire"

  if(REGION == "Aberdeenshire"){
    focalDistanceWoodland <- terra::rast(landuseList$Aberdeen$distanceWoodland)
    focalDistanceHedges <- terra::rast(landuseList$Aberdeen$distanceHedges)
    focalLand <- terra::rast(landuseList$Aberdeen$landuse)
  }
  focalLand <- focalLand %>%
    tidyterra::select(landuse)
  focalDistanceWoodland <- project(focalDistanceWoodland, y = crs("epsg:4326"))
  focalDistanceHedges <- project(focalDistanceHedges, y = crs("epsg:4326"))
  focalLand <- project(focalLand, y = crs("epsg:4326"))

  focalLand <- focalLand %>%
    mutate(
      Deciduous_Broadleaf_Forest = ifelse(landuse == "Deciduous Broadleaf Forest", 1, 0),
      Evergreen_Needleleaf_Forest = ifelse(landuse == "Evergreen Needleleaf Forest", 1, 0),
      Cropland = ifelse(landuse == "Cropland", 1, 0),
      Tall_Grassland = ifelse(landuse == "Tall Grassland", 1, 0),
      Short_Grassland = ifelse(landuse == "Short Grassland", 1, 0),
      Open_Shrubland = ifelse(landuse == "Open Shrubland", 1, 0),
      Barren = ifelse(landuse == "Barren", 1, 0),
      Permanent_Wetland = ifelse(landuse == "Permanent Wetland", 1, 0),
      Human_Settlements = ifelse(landuse == "Human Settlements", 1, 0),
      Other = ifelse(landuse == "Other", 1, 0)
    ) %>%
    tidyterra::select(-landuse)

  binaryList <- terra::split(focalLand, names(focalLand))
  names(binaryList) <- names(focalLand)

  rasterList <- list(
    distanceWoodland = raster::raster(focalDistanceWoodland),
    # distanceHedges = raster::raster(focalDistanceHedges),
    # Deciduous_Broadleaf_Forest = raster::raster(binaryList$Deciduous_Broadleaf_Forest),
    # Evergreen_Needleleaf_Forest = raster::raster(binaryList$Evergreen_Needleleaf_Forest),
    # Cropland = raster::raster(binaryList$Cropland),
    Tall_Grassland = raster::raster(terra::project(binaryList$Tall_Grassland, terra::crs(focalDistanceWoodland))),
    # Short_Grassland = raster::raster(binaryList$Short_Grassland),
    Open_Shrubland = raster::raster(terra::project(binaryList$Open_Shrubland, terra::crs(focalDistanceWoodland)))#,
    # Barren = raster::raster(binaryList$Barren),
    # Permanent_Wetland = raster::raster(binaryList$Permanent_Wetland),
    # Human_Settlements = raster::raster(binaryList$Human_Settlements)
  )

  focalDeer <- deerData %>%
    filter(region == REGION)
  focalDeerIDs <- unique(focalDeer$Animal_ID)
  # x <- focalDeerIDs[1]

  # integrator="MonteCarlo"
  # Time difference of -21.48579 mins
  # integrator="Riemann"

  listWRSF <- lapply(names(akdeLists$tele)[names(akdeLists$tele) %in% focalDeerIDs], function(x){
    print(x)
    st <- Sys.time()
    wrsfOUT <- rsf.fit(data = akdeLists$tele[[x]],
                       UD = akdeLists$akde[[x]],
                       R = rasterList,
                       debias = FALSE,
                       error = error,
                       integrator = "Riemann",
                       max.mem = "20 Gb")
    ed <- Sys.time()

    return(wrsfOUT)
  })
  names(listWRSF) <- focalDeerIDs

  wAKDE <- ctmm::akde(data = akdeLists$tele[names(akdeLists$tele) %in% focalDeerIDs],
                      CTMM = listWRSF,
                      R = rasterList,
                      weights = TRUE,
                      error = error)

  listArea <- lapply(names(wAKDE), function(x){
    # listArea <- lapply(names(listAKDE)[1:2], function(x){
    print(x)

    kdeSummaryList <- lapply(c(0.9, 0.95, 0.99), function(y){
      # y <- 0.95
      kdeSum <- summary(wAKDE[[x]], level.UD = y)
      kdeSummary <- as.data.frame(kdeSum$CI)
      kdeSummary$level <- y
      kdeSummary <- kdeSummary[str_detect(rownames(kdeSummary), "area"),]
      kdeSummary$unit <- rownames(kdeSummary)
      print(kdeSummary$unit)
      if(str_detect(kdeSummary$unit, "square kilo")){
        kdeSummary$low <- kdeSummary$low *100
        kdeSummary$est <- kdeSummary$est *100
        kdeSummary$high <- kdeSummary$high *100
        kdeSummary$unit <- "area (hectares)"
      }
      return(kdeSummary)
    })
    kdeSummaryAll <- do.call(rbind, kdeSummaryList)
    kdeSummaryAll$Animal_ID <- x
    return(kdeSummaryAll)
  })
  names(listArea) <- names(wAKDE)

  listOUT <- list(wrsfModel = listWRSF,
                  wrsfADKE = wAKDE,
                  wrsfAKDEareas = listArea)

  return(listOUT)

}
