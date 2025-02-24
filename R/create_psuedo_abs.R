#' Build a biased pseudo-absence dataset
#'
#' @name create_psuedo_abs
#' @description Using human footprint
#' @return A bm_PseudoAbsences object
#'
#' @export
create_psuedo_abs <- function(occData, hfBiasLayer = here("data", "Human Footprint", "hfp2022.tif"),
                              nPointMultiplier = 3, nReps = 3){
  # occData <- tar_occData
  hfData <- terra::rast(hfBiasLayer)
  UKbbox <- st_bbox(c(xmin = -900000, xmax = 200000, ymin = 5500000, ymax = 7000000))
  hfDataCrop <- terra::crop(hfData, UKbbox)
  hfDataBNG <- terra::project(hfDataCrop, crs("epsg:27700"))
  # plot(hfDataBNG)
  rescale <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}
  hfDataBNG <- hfDataBNG %>%
    mutate(hfp2022 = rescale(hfp2022))

  occDataSimple <- occData %>%
    mutate(x = st_coordinates(occData)[,1],
           y = st_coordinates(occData)[,2]) %>%
    select(x, y, resp)

  # advice from biomod2’s team:
  # - random selection of PA when high specificity is valued over high sensitivity
  # - number of PA = 3 times the number of presences
  # - 10 repetitions

  pseudoWeighted <- spatSample(hfDataBNG, nrow(occData)*3*3, method = "weights",
                               na.rm = TRUE, as.df = TRUE, values = TRUE,
                               xy = TRUE)

  pseudoWeighted <- st_as_sf(pseudoWeighted, coords = c("x", "y"), crs = 27700)
  pseudoWeighted <- pseudoWeighted %>%
    mutate(resp = 0) %>%
    mutate(x = st_coordinates(pseudoWeighted)[,1],
           y = st_coordinates(pseudoWeighted)[,2]) %>%
    select(x, y, resp)

  fullRespData <- rbind(occDataSimple, pseudoWeighted)

  PAtable <- data.frame(PA1 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
                        PA2 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
                        PA3 = ifelse(fullRespData$resp == 1, TRUE, FALSE)#,
                        # PA4 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
                        # PA5 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
                        # PA6 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
                        # PA7 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
                        # PA8 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
                        # PA9 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
                        # PA10 = ifelse(fullRespData$resp == 1, TRUE, FALSE)
                        )

  for (i in 1:ncol(PAtable)) PAtable[sample(which(PAtable[, i] == FALSE), nrow(occData)*3), i] = TRUE

  envLayers <- read_stack_layers(layerLoc = here("data", "GIS data", "SDM Layers"),
                    tar_sdm_layers)

  fullRespData <- as_spatvector(fullRespData, geom = c("x", "y")) %>%
    select(resp)

  PA.u <- bm_PseudoAbsences(resp.var = fullRespData,
                            expl.var = envLayers,
                            strategy = "user.defined",
                            user.table = PAtable)

  return(PA.u)
}
