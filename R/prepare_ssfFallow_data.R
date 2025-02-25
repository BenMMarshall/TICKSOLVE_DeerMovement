#' Extract data from landscape layers for SSF
#'
#' @name prepare_ssfFallow_data
#' @description Extract data from landscape ready for SSF models.
#' @return A list of available and used dataframes.
#'
#' @export
prepare_ssfFallow_data <- function(deerData, landuseList, patchList, cores = 8,
                             nAvail = 10, slDist = "gamma", taDist = "vonmises"){

  # targets::tar_load("tar_deerData")
  # targets::tar_load("tar_landuseList")
  # # targets::tar_load("tar_patchList")
  # deerData <- tar_deerData
  # landuseList <- tar_landuseList
  # # patchList <- tar_patchList

  landuseWessex <- terra::rast(landuseList$Wessex$landuse)

  # only run models in Roe deer
  deerData <- deerData %>%
    filter(str_detect(Animal_ID, "Fallow"))

  roadsWessex_SU <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_SU.gml"),
                            layer = "RoadLink")
  roadsWessex_ST <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_ST.gml"),
                            layer = "RoadLink")
  roadsWessex_SZ <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_SZ.gml"),
                            layer = "RoadLink")
  roadsWessex_SY <- st_read(here("data", "GIS data", "os_roads", "OSOpenRoads_SY.gml"),
                            layer = "RoadLink")
  roadsWessexCrop_SU <- sf::st_crop(roadsWessex_SU, st_bbox(landuseWessex))
  roadsWessexCrop_ST <- sf::st_crop(roadsWessex_ST, st_bbox(landuseWessex)) %>%
    dplyr::select(-name2)
  roadsWessexCrop_SZ <- sf::st_crop(roadsWessex_SZ, st_bbox(landuseWessex))
  roadsWessexCrop_SY <- sf::st_crop(roadsWessex_SY, st_bbox(landuseWessex))

  roadsWessexCrop <- rbind(roadsWessexCrop_SU,
                           rbind(roadsWessexCrop_ST,
                                 rbind(roadsWessexCrop_SZ, roadsWessexCrop_SY)))

  roadsWessexCrop <- roadsWessexCrop %>%
    mutate(roadSize = case_when(
      roadFunction == "A Road" ~ "A roads",
      roadFunction == "B Road" ~ "B roads",
      roadFunction %in% c("Local Access Road", "Restricted Local Access Road",
                          "Local Road", "Minor Road", "Secondary Access Road") ~ "C roads",
      TRUE ~ "Other"
    )) %>%
    mutate(roadSize = factor(roadSize,
                             levels = c("A roads", "B roads", "C roads", "Other")))


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

    focalSteps <- focalDeer %>%
      steps(lonlat = FALSE, keep_cols = "end")

    # focalDistancePatch <- terra::rast(here("data", "GIS data", "distanceAberdeen.tif"))

    focalAllSteps <- focalSteps %>%
      amt::random_steps(n_control = nAvail,
                        sl_distr = amt::fit_distr(focalSteps$sl_, slDist),
                        ta_distr = amt::fit_distr(focalSteps$ta_, taDist)) %>%
      mutate(index = row_number())

    # extract instances of paths crossing roads -------------------------------

    #setup parallel backend to use many processors
    # cores <- detectCores()
    cl <- makeCluster(cores)
    # cl <- makeCluster(cores[1]-4) #not to overload your computer
    registerDoParallel(cl)

    focalRoadCrossingsList <- vector("list", length = length(unique(focalAllSteps$step_id_)))
    names(focalRoadCrossingsList) <- unique(focalAllSteps$step_id_)
    # for(s in unique(focalAllSteps$step_id_)){
    focalRoadCrossingsList <- foreach(i = 1:length(unique(focalAllSteps$step_id_))) %dopar% {

      s <- unique(focalAllSteps$step_id_)[i]
      require(dplyr)
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
        sfLine <- sf::st_sfc(sfLine)
        sf::st_crs(sfLine) <- 27700
        # st_crs(sfLine) <- sf::st_set_crs(sfLine, 27700)
        return(sfLine)
      })
      # stepLinesUnion <- do.call(st_union, stepLinesList)

      roadCrossings <- unlist(lapply(stepLinesList, function(x){
        # x <- stepLinesList[[1]]
        crossings <- sf::st_crosses(x, roadsWessexCrop, sparse = FALSE)
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
    stopCluster(cl)
    focalRoadCrossings <- do.call(rbind, focalRoadCrossingsList)

    focalAllSteps <- focalAllSteps %>%
      left_join(focalRoadCrossings, by = c("step_id_", "index"))

    ssfDataList[[id]] <- list(
      steps = focalAllSteps
    )

  }

  return(ssfDataList)

}
