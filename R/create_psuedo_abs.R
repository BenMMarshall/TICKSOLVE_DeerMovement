#' Build a biased pseudo-absence dataset
#'
#' @name create_psuedo_abs
#' @description Using human footprint
#' @return A bm_PseudoAbsences object
#'
#' @export
create_psuedo_abs <- function(occData, hfBiasLayer = here::here("data", "Human Footprint", "hfp2022.tif"),
                              nPointMultiplier = 3, nReps = 3){

  envLayers <- read_stack_layers(layerLoc = here("data", "GIS data", "SDM Layers"))

  # targets::tar_load("tar_occData_rodent")
  targets::tar_source()
  # occData <- tar_occData_rodent
  hfData <- terra::rast(hfBiasLayer)

  gbGADM <- readRDS(here("data", "GIS data", "gadm", "gadm41_GBR_2_pk.rds"))
  gbGADM <- st_as_sf(gbGADM)

  # quick crop to speed up reprojection
  UKbbox <- st_bbox(c(xmin = -800000, xmax = 200000, ymin = 5800000, ymax = 6830000))
  hfDataCrop <- terra::crop(hfData, UKbbox)
  # then crop to just the GB area
  hfDataCrop <- terra::project(hfDataCrop, crs(envLayers))
  hfDataCrop <- terra::crop(hfDataCrop, st_bbox(envLayers))

  hfDataBNG <- terra::project(hfDataCrop, crs("epsg:27700"))
  # plot(hfDataBNG)
  rescale <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}
  hfDataBNG <- hfDataBNG %>%
    mutate(hfp2022 = rescale(hfp2022))

  # hfData25m <- terra::disagg(hfDataBNG, fact = res(hfDataBNG)[1] / 25)

  print("- hf read and disaggregated")

  occDataSimple <- occData %>%
    mutate(x = st_coordinates(occData)[,1],
           y = st_coordinates(occData)[,2]) %>%
    select(x, y, resp) %>%
    st_drop_geometry()

  weightedRandom <- spatSample(# x = hfData25m,
                                 x = hfDataBNG,
                                 # size = 10,
                                 size = nrow(occData)*nPointMultiplier*nReps,
                                 method = "weights",
                                 na.rm = TRUE, as.df = FALSE, values = FALSE,
                                 xy = TRUE)

  print("- weigthed sampling complete")

  weightedRandom <- weightedRandom %>%
    mutate(resp = NA) %>%
    select(x, y, resp)

  fullRespData <- rbind(occDataSimple, weightedRandom)

  # advice from biomod2’s team:
  # - random selection of PA when high specificity is valued over high sensitivity
  # - number of PA = 3 times the number of presences
  # - 10 repetitions

  # pseudoWeighted <- spatSample(hfDataBNG, nrow(occData)*3*3, method = "weights",
  #                              na.rm = TRUE, as.df = TRUE, values = TRUE,
  #                              xy = TRUE)

  # pseudoWeighted <- st_as_sf(pseudoWeighted, coords = c("x", "y"), crs = 27700)
  # pseudoWeighted <- pseudoWeighted %>%
  #   mutate(resp = 0) %>%
  #   mutate(x = st_coordinates(pseudoWeighted)[,1],
  #          y = st_coordinates(pseudoWeighted)[,2]) %>%
  #   select(x, y, resp)

  # fullRespData <- rbind(occDataSimple, pseudoWeighted)

  PAtable <- as.data.frame(matrix(FALSE, nrow = nrow(fullRespData), ncol = nReps))
  names(PAtable) <- paste0("PA", 1:nReps)
  # known locations always included
  PAtable[which(fullRespData$resp == TRUE),] <- TRUE

  # PAtable <- data.frame(PA1 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
  #                       PA2 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
  #                       PA3 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
  #                       PA4 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
  #                       PA5 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
  #                       PA6 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
  #                       PA7 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
  #                       PA8 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
  #                       PA9 = ifelse(fullRespData$resp == 1, TRUE, FALSE),
  #                       PA10 = ifelse(fullRespData$resp == 1, TRUE, FALSE)
  #                       )

  for (i in 1:ncol(PAtable)) PAtable[sample(which(PAtable[, i] == FALSE),
                                            nrow(occData)*nPointMultiplier), i] = TRUE

  fullRespData <- as_spatvector(fullRespData, geom = c("x", "y")) %>%
    select(resp)

  print("- bm_PseudoAbsences start")

  PA.u <- bm_PseudoAbsences(resp.var = fullRespData,
                            expl.var = envLayers,
                            strategy = "user.defined",
                            user.table = PAtable)

  print("- bm_PseudoAbsences complete")

  return(PA.u)
}
