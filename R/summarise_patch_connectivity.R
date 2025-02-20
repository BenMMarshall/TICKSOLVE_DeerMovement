#' Summarise Patch Connectivity
#'
#' @name summarise_patch_connectivity
#' @description abc
#' @return abc
#'
#' @export
summarise_patch_connectivity <- function(MSEdf, connectRasterLocations, patchList, selectedPatchList, REGION,
                                         buffers){

  # library(dplyr)
  # library(here)
  # library(stringr)
  # library(terra)
  # library(tidyterra)
  # library(sf)
  # library(ggplot2)
  #
  # targets::tar_load("tar_connectPois_list")
  # targets::tar_load("tar_msePois_df")
  # targets::tar_load("tar_patchList")
  # THETA <- 0.1
  # patchList <- tar_patchList
  # MSEdf <- tar_msePois_df
  # connectRasterLocations <- tar_connectPois_list
  # connectTerra <- terra::rast(tar_connectPois_list[[3]])
  # targets::tar_source()
  # REGION <- "Aberdeenshire"

  SELECTEDPATCHES <- selectedPatchList$AberdeenSelected$Ptch_ID

  paletteList <- load_deer_palette()

  meanMSE <- MSEdf %>%
    group_by(theta) %>%
    summarise(meanMSE = mean(mse))

  bestTheta <- meanMSE[meanMSE$meanMSE == min(meanMSE$meanMSE),]$theta

  connectTerraAddr <- connectRasterLocations[str_detect(names(connectRasterLocations),
                                                        sub("e-", "e.", as.character(bestTheta)))]
  connectTerra <- terra::rast(connectTerraAddr[[1]])

  paletteListpaletteListpaletteList <- load_deer_palette()

  focalPatches <- patchList[[sub("shire", "", REGION)]] %>%
    filter(!duplicated(Ptch_ID))

  method <- str_extract(connectTerraAddr, "SSF|Pois")

  names(connectTerra) <- "connectivity"

  patchesBufferedList <- vector("list", length(buffers))
  names(patchesBufferedList) <- paste0("buffer_", buffers)
  for(b in buffers){
    # b <- buffers[3]
    currPatches <- st_buffer(focalPatches, b)

    if(b == 0){
      currPatches <- focalPatches
    }

    patchMeanScore <- terra::extract(connectTerra, currPatches, fun = mean,
                                     bind = TRUE, na.rm = TRUE) %>%
      dplyr::select(Ptch_ID, connectivity) %>%
      mutate(buffer = b,
             summaryMethod = "mean") %>%
      as.data.frame()

    patchMaxScore <- terra::extract(connectTerra, currPatches, fun = max,
                                    bind = TRUE, na.rm = TRUE) %>%
      dplyr::select(Ptch_ID, connectivity) %>%
      mutate(buffer = b,
             summaryMethod = "max") %>%
      as.data.frame()

    patchMedScore <- terra::extract(connectTerra, currPatches, fun = median,
                                    bind = TRUE, na.rm = TRUE) %>%
      dplyr::select(Ptch_ID, connectivity) %>%
      mutate(buffer = b,
             summaryMethod = "median") %>%
      as.data.frame()

    patchesBufferedList[[paste0("buffer_", b)]] <- patchMeanScore %>%
      rbind(patchMaxScore) %>%
      rbind(patchMedScore)

  }

  bufferSummaries <- do.call(rbind, patchesBufferedList)

  # st_crs(patchList$AberdeenSelected) <- st_crs(27700)

  patchAreas <- data.frame(Ptch_ID = as.character(patchList$AberdeenSelected$Ptch_ID),
                           area_ha = as.numeric(units::set_units(st_area(patchList$AberdeenSelected), "ha")))

  bufferSummaries <- bufferSummaries %>%
    mutate(Ptch_ID = as.character(Ptch_ID)) %>%
    left_join(patchList$AberdeenSelected %>%
                st_drop_geometry() %>%
                mutate(Ptch_ID = as.character(Ptch_ID))) %>%
    left_join(patchAreas)

  bufferSummaries <- bufferSummaries %>%
    mutate(selected = factor(ifelse(Ptch_ID %in% as.character(patchList$AberdeenSelected$Ptch_ID), "Selected", "Not selected"),
                             levels = c("Not selected", "Selected")))

  return(bufferSummaries)

}
