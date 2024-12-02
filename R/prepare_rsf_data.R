#' Extract data from landscape layers for RSF
#'
#' @name prepare_rsf_data
#' @description Extract data from landscape ready for RSF models.
#' @return A list of available and used dataframes.
#'
#' @export
prepare_rsf_data <- function(deerData, akdeLists, landuseList, patchList,
                             nAvail = 10, typeAvial = "random", conAvail = "99%"){

  # targets::tar_load("tar_deerData")
  # targets::tar_load("tar_akdeLists")
  # targets::tar_load("tar_landuseList")
  # targets::tar_load("tar_patchList")
  # deerData <- tar_deerData
  # akdeLists <- tar_akdeLists
  # landuseList <- tar_landuseList
  # patchList <- tar_patchList

  rsfDataList <- vector("list", length = length(names(akdeLists$sf)))
  names(rsfDataList) <- names(akdeLists$sf)
  for(id in names(akdeLists$sf)){
    # id <- names(akdeLists$sf)[1]
    print(id)
    focalDeer <- deerData %>%
      filter(Animal_ID == id) %>%
      st_as_sf(coords = c("x", "y"), remove = FALSE, crs = 27700)

    focalHR <- akdeLists$sf[[id]]
    print(focalHR)
    focalHR <- focalHR %>%
      filter(level == conAvail, ci == "est")
    print(focalHR)

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

    availPoints <- sf::st_sample(focalHR, size = nrow(focalDeer) * nAvail,
                                 type = typeAvial)
    availPoints <- st_transform(availPoints, crs = 27700) %>%
      st_coordinates()
    # could also be defined as an area surrounding the points
    availDistancePatch <- terra::extract(focalDistancePatch, availPoints)
    availLanduse <- terra::extract(focalLand, availPoints)

    availPoints <- as.data.frame(availPoints) %>%
      rename("x" = X, "y" = Y) %>%
      mutate(availDistancePatch) %>%
      rename("distancePatch" = layer) %>%
      mutate(availLanduse)

    focalDeerDistance <- terra::extract(focalDistancePatch, focalDeer)
    focalDeer <- focalDeer %>%
      mutate(distancePatch = focalDeerDistance$layer)

    focalDeerLand <- terra::extract(focalLand, focalDeer)
    focalDeer <- focalDeer %>%
      cbind(focalDeerLand %>% select(-ID))

    availPoints$case_ <- 0
    focalDeer$case_ <- 1

    availPoints$Animal_ID <- id

    availPoints <- availPoints %>%
      mutate(landuse = factor(case_when(
        LCM_1 %in% 1:2 ~ "Woodland",
        LCM_1 %in% 3 ~ "Arable",
        LCM_1 %in% 4:7 ~ "Grasslands",
        LCM_1 %in% 9:10 ~ "Heathland",
        LCM_1 %in% c(11,14) ~ "Aquatic",
        LCM_1 %in% 20:21 ~ "Human Settlements",
        TRUE ~ "Other"
      ), levels = c(
        "Woodland",
        "Grasslands",
        "Heathland",
        "Aquatic",
        "Arable",
        "Human Settlements",
        "Other"
      )))
    focalDeer <- focalDeer %>%
      mutate(landuse = factor(case_when(
        LCM_1 %in% 1:2 ~ "Woodland",
        LCM_1 %in% 3 ~ "Arable",
        LCM_1 %in% 4:7 ~ "Grasslands",
        LCM_1 %in% 9:10 ~ "Heathland",
        LCM_1 %in% c(11,14) ~ "Aquatic",
        LCM_1 %in% 20:21 ~ "Human Settlements",
        TRUE ~ "Other"
      ), levels = c(
        "Woodland",
        "Grasslands",
        "Heathland",
        "Aquatic",
        "Arable",
        "Human Settlements",
        "Other"
      )))

    rsfDataList[[id]] <- list(
      availPoints = availPoints,
      usedDeer = focalDeer
    )

  }

  return(rsfDataList)

}
