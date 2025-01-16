#' Extract data from landscape layers for SSF
#'
#' @name prepare_ssf_data
#' @description Extract data from landscape ready for SSF models.
#' @return A list of available and used dataframes.
#'
#' @export
prepare_ssf_data <- function(deerData, landuseList, patchList,
                             nAvail = 10, slDist = "gamma", taDist = "vonmises"){

  # targets::tar_load("tar_deerData")
  # targets::tar_load("tar_landuseList")
  # # targets::tar_load("tar_patchList")
  # deerData <- tar_deerData
  # landuseList <- tar_landuseList
  # # patchList <- tar_patchList

  ssfDataList <- vector("list", length = length(unique(deerData$Animal_ID)))
  names(ssfDataList) <- unique(deerData$Animal_ID)
  for(id in unique(deerData$Animal_ID)){
    # id <- unique(deerData$Animal_ID)[1]
    print(id)
    focalRegion <- deerData[deerData$Animal_ID == id,]$region[1]

    focalDeer <- deerData %>%
      filter(Animal_ID == id) %>%
      # st_as_sf(coords = c("x", "y"), remove = FALSE, crs = 27700) %>%
      make_track(.x = x, .y = y, .t = datetime, crs = 27700, id = Animal_ID)

    # if(focalDeer$region[1] == "Aberdeenshire"){
    #   focalDistancePatch <- terra::rast(patchList$distanceAberdeen)
    # } else {
    #   focalDistancePatch <- terra::rast(patchList$distanceWessex)
    # }
    if(focalRegion == "Aberdeenshire"){
      focalDistanceWoodland <- terra::rast(landuseList$Aberdeen$distanceWoodland)
      focalDistanceHedges <- terra::rast(landuseList$Aberdeen$distanceHedges)
      focalLand <- terra::rast(landuseList$Aberdeen$landuse)
    } else {
      focalDistanceWoodland <- terra::rast(landuseList$Wessex$distanceWoodland)
      focalDistanceHedges <- terra::rast(landuseList$Wessex$distanceHedges)
      focalLand <- terra::rast(landuseList$Wessex$landuse)
    }

    focalLand <- focalLand %>%
      mutate(LCM_1_cat = paste0("LCM_", LCM_1))

    focalSteps <- focalDeer %>%
      steps(lonlat = FALSE, keep_cols = "end")

    # focalDistancePatch <- terra::rast(here("data", "GIS data", "distanceAberdeen.tif"))

    focalAllSteps <- focalSteps %>%
      amt::random_steps(n_control = 10,
                        sl_distr = amt::fit_distr(focalSteps$sl_, slDist),
                        ta_distr = amt::fit_distr(focalSteps$ta_, taDist)) %>%
      amt::extract_covariates(covariates = focalDistanceWoodland, where = "end") %>%
      amt::extract_covariates(covariates = focalDistanceHedges, where = "end") %>%
      amt::extract_covariates(covariates = focalLand, where = "end") %>%
      mutate(index = row_number()) #%>%
      # mutate(landuse = factor(case_when(
      #   LCM_1 %in% 1 ~ "Deciduous Broadleaf Forest",
      #   LCM_1 %in% 2 ~ "Evergreen Needleleaf Forest",
      #   LCM_1 %in% 3 ~ "Cropland",
      #   LCM_1 %in% 4 ~ "Tall Grassland",
      #   LCM_1 %in% 5:7 ~ "Short Grassland",
      #   LCM_1 %in% 9:10 ~ "Open Shrubland",
      #   LCM_1 %in% c(12,15,16,17,18) ~ "Barren",
      #   LCM_1 %in% c(8,11,19) ~ "Permanent Wetland",
      #   LCM_1 %in% 20:21 ~ "Human Settlements",
      #   TRUE ~ "Other"
      # ), levels = c(
      #   "Deciduous Broadleaf Forest",
      #   "Evergreen Needleleaf Forest",
      #   "Cropland",
      #   "Tall Grassland",
      #   "Short Grassland",
      #   "Open Shrubland",
      #   "Barren",
      #   "Permanent Wetland",
      #   "Human Settlements",
      #   "Other"
      # )))

    # extract instances of paths crossing roads -------------------------------

    if(focalRegion == "Aberdeenshire"){
      focalRoads <- landuseList$Aberdeen$roads
    } else {
      focalRoads <- landuseList$Wessex$roads
    }

    focalRoadCrossingsList <- vector("list", length = length(unique(focalAllSteps$step_id_)))
    names(focalRoadCrossingsList) <- unique(focalAllSteps$step_id_)
    for(s in unique(focalAllSteps$step_id_)){
      print(paste0("Step ", s))
      # s <- unique(focalAllSteps$step_id_)[4]
      focalStep <- focalAllSteps %>%
        filter(step_id_ == s)
      startLoc <- focalStep[1,c("x1_", "y1_")]

      stepLinesList <- lapply(1:nrow(focalStep), function(x){
        # x <- 4
        row <- focalStep[x,]
        pointMatrix <- rbind(matrix(as.numeric(startLoc), nrow = 1),
                             matrix(as.numeric(row[c("x2_", "y2_")]), nrow = 1))
        sfLine <- sf::st_linestring(pointMatrix)
        sfLine <- st_sfc(sfLine)
        st_crs(sfLine) <- 27700
        # st_crs(sfLine) <- sf::st_set_crs(sfLine, 27700)
        return(sfLine)
      })
      # stepLinesUnion <- do.call(st_union, stepLinesList)

      roadCrossings <- unlist(lapply(stepLinesList, function(x){
        # x <- stepLinesList[[1]]
        crossings <- st_crosses(x, focalRoads, sparse = FALSE)
        # print(any(crossings))
        return(any(crossings))
      }))
      print(paste0(sum(roadCrossings), " / ", length(roadCrossings)))

      roadCrossingsDF <- data.frame(
        step_id_ = s,
        Animal_ID = id,
        roadCrossings = roadCrossings,
        index = focalStep$index)

      focalRoadCrossingsList[[s]] <- roadCrossingsDF

    }
    focalRoadCrossings <- do.call(rbind, focalRoadCrossingsList)

    focalAllSteps <- focalAllSteps %>%
      left_join(focalRoadCrossings, by = c("step_id_", "index"))

    # rbind(focalAllSteps[1:2,c("x1_", "y1_")] %>%
    #         as.matrix(),
    #       focalAllSteps[1:2,c("x2_", "y2_")] %>%
    #         as.matrix())
    # focalLine <- sf::st_linestring(rbind(focalAllSteps[1,c("x1_", "y1_")] %>%
    #                                        as.matrix(),
    #                                      focalAllSteps[1,c("x2_", "y2_")] %>%
    #                                        as.matrix()))
    # testRoad1 <- sf::st_linestring(rbind(c(376702,805700), c(376860,805890)))
    # testRoad2 <- sf::st_linestring(rbind(c(376752,805700), c(376760,805890)))
    # ggplot() +
    #   geom_sf(data = focalLine, colour = "red") +
    #   geom_sf(data = testRoad1, colour = "grey50") +
    #   geom_sf(data = testRoad2)
    # st_crosses(focalLine, testRoad, sparse = FALSE)
    # testRoadMulti <- st_union(testRoad1, testRoad2)
    # st_crosses(focalLine, testRoadMulti, sparse = FALSE)
    # ggplot() +
    #   geom_sf(data = focalLine, colour = "red") +
    #   geom_sf(data = testRoadMulti, colour = "grey50")

    ssfDataList[[id]] <- list(
      steps = focalAllSteps
    )

  }

  return(ssfDataList)

}
