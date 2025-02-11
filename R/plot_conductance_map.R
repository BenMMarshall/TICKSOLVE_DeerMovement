#' Map out conductance
#'
#' @name plot_conductance_map
#' @description abc
#' @return abc
#'
#' @export
plot_conductance_map <- function(predResistLocation, patchList, REGION){

  # targets::tar_load("tar_patchList")
  # targets::tar_load("tar_predPoisResist_location")
  # predResistLocation <- tar_predPoisResist_location
  paletteList <- load_deer_palette()

  predTerra <- rast(predResistLocation)
  focalPatches <- patchList[[sub("shire", "", REGION)]]

  resistPlot <- ggplot() +
    geom_spatraster(data = predTerra, aes(fill = pred)) +
    scale_fill_gradient(high = scales::muted(paletteList$highWhitePal[1]),
                        low = paletteList$highWhitePal[2],
                        na.value = paletteList$baseGrey) +
    coord_sf(xlim = c(max(c(ext(focalPatches)[1],
                            ext(predTerra)[1])),
                      min(c(ext(focalPatches)[2],
                            ext(predTerra)[2]))),
             ylim = c(max(c(ext(focalPatches)[3],
                            ext(predTerra)[3])),
                      min(c(ext(focalPatches)[4],
                            ext(predTerra)[4]))),
             expand = 0, datum = st_crs(27700)) +
    labs(fill = "Conductance") +
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
    )

  ggsave(plot = resistPlot, filename = here("figures",
                                           paste0("resistanceConductanceMap_",
                                                  str_extract(predResistLocation[1],
                                                              pattern = "SSF|Pois"), "_", REGION, ".png")),
         width = 220, height = 180, units = "mm", dpi = 300)
  ggsave(plot = resistPlot, filename = here("figures",
                                           paste0("resistanceConductanceMap_",
                                                  str_extract(predResistLocation[1],
                                                              pattern = "SSF|Pois"), "_", REGION, ".pdf")),
         width = 220, height = 180, units = "mm")

  return(resistPlot)

}
