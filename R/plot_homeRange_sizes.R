#' Generate plot showing home range sizes
#'
#' @name plot_homeRange_sizes
#' @description Plot all row deer home range estimates.
#' @return A ggplot object, and saved file.
#'
#' @export
plot_homeRange_sizes <- function(deerData, akdeLists){

  paletteList <- load_deer_palette()

  # targets::tar_load("tar_akdeLists")
  # targets::tar_load("tar_deerData")
  #
  # akdeLists <- tar_akdeLists
  # deerData <- tar_deerData
  # library(here)
  # library(dplyr)
  # library(ggplot2)
  # library(sf)
  # library(ctmm)
  # library(stringr)
  # mean(list(listAKDE$Roe15_F, listAKDE$Roe12_F))
  # mean(tar_akdeLists$akde)

  allAreas <- do.call(rbind, akdeLists$area) %>%
    left_join(deerData %>%
                group_by(Animal_ID, region, Sex) %>%
                slice_head(n = 1) %>%
                dplyr::select(Animal_ID, region, Sex))

  areaPlot <- allAreas %>%
    filter(level == 0.95) %>%
    filter(!str_detect(Animal_ID, "Fallow")) %>%
    ggplot() +
    geom_errorbar(aes(x = Animal_ID, ymin = low, ymax = high,
                      colour = as.factor(level), group = as.factor(level)),
                  position = position_dodge(width = 0.2), width = 0.25) +
    geom_point(aes(x = Animal_ID, y = est,
                   colour = as.factor(level), group = as.factor(level), shape = Sex), position = position_dodge(width = 0.2)) +
    geom_hline(data = allAreas %>%
                 filter(level == 0.95) %>%
                 filter(!str_detect(Animal_ID, "Fallow")) %>%
                 group_by(level) %>%
                 summarise(mean = mean(est)),
               aes(yintercept = mean, colour = as.factor(level)), linetype = 2) +
    scale_colour_manual(values = unname(paletteList$highMidLowPal)) +
    coord_flip() +
    facet_grid(rows = vars(region), scales = "free_y", space = "free_y", switch = "y",
               axes = "all_y") +
    labs(x = "", y = "Area (hectares)") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      axis.title = element_text(face = 2),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # legend.position = "none",
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
    )

  ggsave(areaPlot, filename = here("figures", "homeRangeAreaPlot.png"),
         width = 210, height = 120, units = "mm", dpi = 300)
  ggsave(areaPlot, filename = here("figures", "homeRangeAreaPlot.pdf"),
         width = 210, height = 120, units = "mm")

  return(areaPlot)

}
