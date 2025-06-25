#' Summarise Patch Connectivity
#'
#' @name extract_patch_connectivity_WREN
#' @description abc
#' @return abc
#'
#' @export
extract_patch_connectivity_WREN <- function(MSEdf,
                                            connectRasterLocations,
                                            patchList,
                                            REGION,
                                            buffers){

  # library(dplyr)
  # library(here)
  # library(stringr)
  # library(terra)
  # library(tidyterra)
  # library(sf)
  # library(ggplot2)
  #
  # targets::tar_load("tar_msePois_df")
  # targets::tar_load("tar_connectStanPois_locationWREN")
  # targets::tar_load("tar_patchList_WREN")
  # patchList <- list(WREN = read_sf(here("data", "GIS data", "patchesWREN.geoJSON")))
  # MSEdf <- tar_msePois_df
  # connectRasterLocations <- tar_connectStanPois_locationWREN
  # connectTerra <- terra::rast(connectRasterLocations[[1]])
  # targets::tar_source()
  # REGION <- "WREN"
  # WRENbuffers <- c(0, 250, 500, 750, 1000, 1500, 2000)
  # buffers <- WRENbuffers

  connectTerra <- terra::rast(connectRasterLocations[1])

  paletteListpaletteListpaletteList <- load_deer_palette()

  focalPatches <- patchList[[sub("shire", "", REGION)]] %>%
    rename(Ptch_ID = id) %>%
    filter(!duplicated(Ptch_ID))
  # selectedFocalPatches <- selectedPatchList[str_detect(names(selectedPatchList), sub("shire", "", REGION))][[1]]

  method <- str_extract(connectRasterLocations[1], "SSF|Pois")

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

  patchMetrics <- read.csv(here("data", "GIS data", "wren_site_metrics.csv")) %>%
    rename(Ptch_ID = patch.id, buffer = buffer_size)

  patchAreas <- data.frame(Ptch_ID = focalPatches$Ptch_ID,
                           area_km2 = as.numeric(units::set_units(st_area(focalPatches), "km2")))

  bufferSummaries <- bufferSummaries %>%
    left_join(patchAreas, by = "Ptch_ID")

  bufferSummariesAll <- patchMetrics %>%
    full_join(bufferSummaries, by = c("Ptch_ID", "buffer"))

  write.csv(bufferSummariesAll,
            here("tables", paste0("connectivityValuesAll_Roe_", REGION, ".csv")),
            row.names = FALSE)


  return(bufferSummaries)

}
