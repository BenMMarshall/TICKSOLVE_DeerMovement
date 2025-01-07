#' Map out theta variation
#'
#' @name map_connectivity_thetas
#' @description abc
#' @return abc
#'
#' @export
map_connectivity_thetas <- function(connectRasterLocations, landuseList, patchList, REGION){

  # library(tidyterra)
  # library(ggplot2)
  # library(stringr)
  #
  # targets::tar_load("tar_connect_location_0.01_300")
  # targets::tar_load("tar_connect_location_0.001_300")
  # targets::tar_load("tar_connect_location_1e.04_300")
  # targets::tar_load("tar_patchList")
  #
  # connectRasterLocations <- c(
  #   tar_connect_location_0.01_300,
  #   tar_connect_location_0.001_300,
  #   tar_connect_location_1e.04_300)
  # names(connectRasterLocations) <- connectRasterLocations

  # targets::tar_load("tar_patchList")
  # targets::tar_load("tar_landuseList")
  # landuseList <- tar_landuseList
  # patchList <- tar_patchList

  connectRaster <- terra::rast(unlist(connectRasterLocations))
  names(connectRaster) <- sub(".tif$", "", str_extract_all(connectRasterLocations, "theta_([\\d\\.\\-e]+)\\.tif$"))

  focalPatches <- patchList[[sub("shire", "", REGION)]]
  focalRoads <- landuseList[[sub("shire", "", REGION)]]$roads

  paletteList <- load_deer_palette()

  (thetaMaps <- ggplot() +
    geom_spatraster(data = connectRaster) +
    # geom_sf(data = focalPatches, fill = NA, colour = "grey75", linewidth = 0.01) +
    # geom_sf(data = focalRoads, colour = "#000000", alpha = 0.25) +
      scale_fill_gradient(high = scales::muted(paletteList$highWhitePal[1]),
                          low = paletteList$highWhitePal[2],
                          na.value = paletteList$baseGrey) +
    facet_wrap(~lyr, ncol = 3) +
    coord_sf(xlim = c(max(c(ext(focalPatches)[1],
                            ext(connectRaster)[1])),
                      min(c(ext(focalPatches)[2],
                            ext(connectRaster)[2]))),
             ylim = c(max(c(ext(focalPatches)[3],
                            ext(connectRaster)[3])),
                      min(c(ext(focalPatches)[4],
                            ext(connectRaster)[4]))),
             expand = 0) +
    labs(fill = "Connectivity") +
    theme_bw() +
    theme(
      text = element_text(colour = paletteList$baseGrey),
      line = element_line(colour = paletteList$baseGrey),
      axis.title = element_text(face = 2),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(angle = 0, face = 4, hjust = 0, vjust = 1)
      # legend.position = "none"
    ))

  ggsave(plot = thetaMaps, filename = here("figures",
                                           paste0("thetaMaps_",
                                                  str_extract(connectRasterLocations, pattern = "SSF|Pois"), ".png")),
         width = 300, height = 200, units = "mm", dpi = 300)
  ggsave(plot = thetaMaps, filename = here("figures",
                                           paste0("thetaMaps_",
                                                  str_extract(connectRasterLocations, pattern = "SSF|Pois"), ".pdf")),
         width = 300, height = 200, units = "mm")

  return(thetaMaps)

}
