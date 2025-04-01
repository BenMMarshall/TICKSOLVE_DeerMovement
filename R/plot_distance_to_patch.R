#' Generate plot to explore distance from patch
#'
#' @name plot_distance_to_patch
#' @description abc
#' @return abc
#'
#' @export
plot_distance_to_patch <- function(deerData, patchList, akdeSummary){

  # library(here)
  # library(dplyr)
  # library(terra)
  # library(tidyterra)
  # library(sf)
  # library(ggplot2)
  # library(stringr)
  #
  # targets::tar_load("tar_deerData")
  # targets::tar_load("tar_patchList")
  # targets::tar_load("tar_akdeSummary")
  # targets::tar_source()
  # akdeSummary <- tar_akdeSummary
  # deerData <- tar_deerData
  # patchList <- tar_patchList

  paletteList <- load_deer_palette()

  akdeHRHalf <- mean(as.numeric(akdeSummary$longestAxisSummary$longestAxisRange_m))/2

  deersf <- deerData %>%
    st_as_sf(coords = c("x", "y"), remove = FALSE, crs = 27700)

  extent_m <- ext(patchList$Aberdeen)
  # will result in a grid that has a 1 m x 1 m res
  xRes <- abs(extent_m[1] - extent_m[2])
  yRes <- abs(extent_m[3] - extent_m[4])

  template <- rast(vect(patchList$Aberdeen), nrows = yRes/10, ncols = xRes/10)

  print("Binary Raster...")
  binaryRaster <- rasterize(vect(patchList$Aberdeen),
                            template)

  print("Distance Raster...")
  distanceRaster <- terra::distance(binaryRaster)

  distance2patch <- terra::extract(distanceRaster, deersf)

  textLabel <- str_wrap(paste0(round(sum(distance2patch$layer < akdeHRHalf, na.rm = TRUE) /
                                       length(distance2patch$layer[!is.na(akdeHRHalf)]) *100,
                                     digits = 0),
                               "% of deer locations when outside a patch are within half the mean longest axis of 99% AKDE home range area (",
                               round(akdeHRHalf, digits = 0), "m)"),
                        width = 80)

  distancePlot <- distance2patch %>%
    filter(!is.na(layer), !is.null(layer), layer > 0) %>%
    ggplot() +
    geom_density(aes(x = layer), fill = paletteList$deerSexPal[[1]],
                 colour = NA, alpha = 0.65) +
    geom_vline(xintercept = akdeHRHalf, linetype = 2, colour = "grey25") +
    # annotate("text", x = akdeHRHalf-20, y = 0.0075,
    #          label = textLabel, hjust = 1, vjust = 0, fontface = 3) +
    labs(y = "Density of deer locations", x = "Distance from patch (m)",
         caption = textLabel) +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      axis.title = element_text(face = 2),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "none",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1),
      strip.placement = "outside"
    )

  ggsave(distancePlot, filename = here("figures", "distanceDistributionPlot.png"),
         width = 210, height = 120, units = "mm", dpi = 300)
  ggsave(distancePlot, filename = here("figures", "distanceDistributionPlot.pdf"),
         width = 210, height = 120, units = "mm")

  return(distancePlot)

}
