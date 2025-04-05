#' Summarise Patch Connectivity
#'
#' @name extract_patch_omniConnectivity
#' @description abc
#' @return abc
#'
#' @export
extract_patch_omniConnectivity <- function(omniNormCurr,
                                           selectedPatchList,
                                           buffers){

  # library(dplyr)
  # library(here)
  # library(stringr)
  # library(terra)
  # library(tidyterra)
  # library(sf)
  # library(ggplot2)
  #
  # targets::tar_load("tar_omniLayers_rodent_wessex")
  # targets::tar_load("tar_selectedPatchList")
  # selectedPatchList <- tar_selectedPatchList
  # omniNormCurr <- tar_omniLayers_rodent_wessex
  # buffers <- c(0, 750)
  # # targets::tar_source()

  omniNorm <- terra::rast(omniNormCurr[3])

  if(str_detect(omniNormCurr[3], "aberdeen")){
    focalPatches <- selectedPatchList$AberdeenSelected
  } else {
    focalPatches <- selectedPatchList$WessexSelected
  }

  patchesBufferedList <- vector("list", length(buffers))
  names(patchesBufferedList) <- paste0("buffer_", buffers)
  for(b in buffers){
    # b <- buffers[2]

    if(b == 0){
      currPatches <- focalPatches
    }

    currPatches <- st_buffer(focalPatches, b)

    patchMeanScore <- terra::extract(omniNorm, currPatches, fun = mean,
                                     bind = TRUE, na.rm = TRUE) %>%
      dplyr::select(Ptch_ID, normalized_cum_currmap) %>%
      mutate(buffer = b,
             summaryMethod = "mean") %>%
      as.data.frame()

    patchesBufferedList[[paste0("buffer_", b)]] <- patchMeanScore # %>%
    # rbind(patchMaxScore) %>%
    # rbind(patchMedScore)
  }
  bufferSummaries <- do.call(rbind, patchesBufferedList)

  write.csv(bufferSummaries,
            here("tables", paste0(
              "omniConnectivity_",
              str_extract(omniNormCurr[3], "rodent|fallow"), "_",
              str_extract(omniNormCurr[3], "wessex|aberdeen"),
              "_selectedPatches.csv"
            )),
            row.names = FALSE)

  return(bufferSummaries)

}
