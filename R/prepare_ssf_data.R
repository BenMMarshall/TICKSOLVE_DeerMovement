#' Extract data from landscape layers for SSF
#'
#' @name prepare_ssf_data
#' @description Extract data from landscape ready for SSF models.
#' @return A list of available and used dataframes.
#'
#' @export
prepare_ssf_data <- function(deerData, landuseList, patchList,
                             nAvail = 10, slDist = "gamma", taDist = "vonmises"){

  ssfDataList <- vector("list", legnth = length(unique(deerData$Animal_ID)))
  names(ssfDataList) <- unique(deerData$Animal_ID)
  for(id in unique(deerData$Animal_ID)){
    # id <- unique(deerData$Animal_ID)[1]

    focalDeer <- deerData %>%
      filter(Animal_ID == id) %>%
      # st_as_sf(coords = c("x", "y"), remove = FALSE, crs = 27700) %>%
      make_track(.x = x, .y = y, .t = datetime, crs = 27700, id = Animal_ID)

    if(focalDeer$region[1] == "Aberdeenshire"){
      focalDistancePatch <- terra::rast(patchList$distanceAberdeen)
    } else {
      focalDistancePatch <- terra::rast(patchList$distanceWessex)
    }

    if(focalDeer$region[1] == "Aberdeenshire"){
      focalLand <- terra::rast(landuseList$Aberdeen$landuse)
    } else {
      focalLand <- terra::rast(landuseList$Wessex$landuse)
    }
    focalLand <- focalLand %>%
      mutate(LCM_1_cat = paste0("LCM_", LCM_1))

    focalSteps <- focalDeer %>%
      steps(lonlat = FALSE, keep_cols = "end")

    # focalDistancePatch <- terra::rast(here("data", "GIS data", "distanceAberdeen.tif"))

    focalAllSteps <- focalSteps %>%
      amt::random_steps(n_control = 10,
                        sl_distr = amt::fit_distr(focalSteps$sl_, "gamma"),
                        ta_distr = amt::fit_distr(focalSteps$ta_, "vonmises")) %>%
      amt::extract_covariates(covariates = focalDistancePatch, where = "end") # %>%
    # amt::extract_covariates(covariates = focalLand, where = "end")

    ssfDataList[[id]] <- list(
      steps = focalAllSteps
    )

  }

  return(ssfDataList)

}
