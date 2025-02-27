#' Summarise Patch Connectivity
#'
#' @name extract_patch_connectivity
#' @description abc
#' @return abc
#'
#' @export
extract_patch_connectivity <- function(MSEdf, connectRasterLocations, patchList, selectedPatchList, REGION,
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
  # targets::tar_load("tar_selectedPatchList")
  # selectedPatchList <- tar_selectedPatchList
  # THETA <- 0.1
  # patchList <- tar_patchList
  # MSEdf <- tar_msePois_df
  # connectRasterLocations <- tar_connectPois_list
  # connectTerra <- terra::rast(tar_connectPois_list[[3]])
  # targets::tar_source()
  # REGION <- "Aberdeenshire"
  # buffers <- c(0, 750)

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
  selectedFocalPatches <- selectedPatchList[str_detect(names(selectedPatchList), sub("shire", "", REGION))][[1]]

  method <- str_extract(connectTerraAddr, "SSF|Pois")

  names(connectTerra) <- "connectivity"

  patchesBufferedList <- vector("list", length(buffers))
  names(patchesBufferedList) <- paste0("buffer_", buffers)
  for(b in buffers){
    # b <- buffers[2]

    if(b == 0){
      currPatches <- focalPatches
    }

    currPatches <- st_buffer(focalPatches, b)

    patchMeanScore <- terra::extract(connectTerra, currPatches, fun = mean,
                                     bind = TRUE, na.rm = TRUE) %>%
      dplyr::select(Ptch_ID, connectivity) %>%
      mutate(buffer = b,
             summaryMethod = "mean") %>%
      as.data.frame()

    # patchMaxScore <- terra::extract(connectTerra, currPatches, fun = max,
    #                                 bind = TRUE, na.rm = TRUE) %>%
    #   dplyr::select(Ptch_ID, connectivity) %>%
    #   mutate(buffer = b,
    #          summaryMethod = "max") %>%
    #   as.data.frame()

    # patchMedScore <- terra::extract(connectTerra, currPatches, fun = median,
    #                                 bind = TRUE, na.rm = TRUE) %>%
    #   dplyr::select(Ptch_ID, connectivity) %>%
    #   mutate(buffer = b,
    #          summaryMethod = "median") %>%
    #   as.data.frame()

    patchesBufferedList[[paste0("buffer_", b)]] <- patchMeanScore # %>%
      # rbind(patchMaxScore) %>%
      # rbind(patchMedScore)
  }
  bufferSummaries <- do.call(rbind, patchesBufferedList)

  selectedPatchesBufferedList <- vector("list", length(buffers))
  names(selectedPatchesBufferedList) <- paste0("buffer_", buffers)
  for(b in buffers){

    if(b == 0){
      currPatches <- selectedFocalPatches
    }

    currPatches <- st_buffer(selectedFocalPatches, b)

    patchMeanScore <- terra::extract(connectTerra, currPatches, fun = mean,
                                     bind = TRUE, na.rm = TRUE) %>%
      # dplyr::select(Ptch_ID, connectivity) %>%
      mutate(buffer = b,
             summaryMethod = "mean") %>%
      as.data.frame()

    selectedPatchesBufferedList[[paste0("buffer_", b)]] <- patchMeanScore
  }
  selectedBufferSummaries <- do.call(rbind, selectedPatchesBufferedList)

  # st_crs(patchList$AberdeenSelected) <- st_crs(27700)
  bufferSummaries <- bufferSummaries %>%
    mutate(selected = factor(ifelse(Ptch_ID %in% as.character(selectedPatchList$AberdeenSelected$Ptch_ID), "Selected", "Not selected"),
                             levels = c("Not selected", "Selected")))

  bufferSummariesAll <- bind_rows(selectedBufferSummaries %>%
                                    mutate(selected = "Selected"),
                                  bufferSummaries %>%
                                    filter(selected == "Not selected"))
  row.names(bufferSummariesAll) <- NULL

  # patchAreas <- data.frame(Ptch_ID = as.character(focalPatches$Ptch_ID),
  #                          area_km2 = as.numeric(units::set_units(st_area(focalPatches), "km2")))

  bufferSummariesAll <- bufferSummariesAll %>%
    mutate(Ptch_ID = as.character(Ptch_ID)) # %>%
    # left_join(selectedPatchList$AberdeenSelected %>%
    #             st_drop_geometry() %>%
    #             mutate(Ptch_ID = as.character(Ptch_ID))) %>%
    # left_join(patchAreas)

  return(bufferSummariesAll)

}
