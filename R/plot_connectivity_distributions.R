#' Create a plot of connectivity values at different areas
#'
#' @name plot_connectivity_distributions
#' @description abc
#' @return abc
#'
#' @export
plot_connectivity_distributions <- function(validData){

  # library(here)
  # library(dplyr)
  # library(ggplot2)
  # library(ggridges)
  # library(ggtext)
  #
  # targets::tar_load("tar_validation_data")
  # targets::tar_source()
  # validData <- tar_validation_data

  paletteList <- load_deer_palette()

  areaPal <- paletteList$highSigLowSigNoSig
  areaPal <- unname(areaPal)
  areaPalDF <- data.frame(area = c("Known Locations", "Home Range", "Landscape"),
                          colour = areaPal)

  validDataPlottable <- validData %>%
    mutate(area = case_when(
      area == "knownlocations" ~ "Known Locations",
      area == "homerange" ~ "Home Range",
      area == "landscape" ~ "Landscape"
    )) %>%
    left_join(areaPalDF) %>%
    mutate(areaCol = glue::glue("<i style='color:{colour}'>{area}</i>")) %>%
    mutate(areaCol = factor(areaCol,
                            levels = c("<i style='color:#B54D17'>Known Locations</i>",
                                       "<i style='color:#E79559'>Home Range</i>",
                                       "<i style='color:#505050'>Landscape</i>")))

  names(areaPal) <- levels(validDataPlottable$areaCol)

  densityOfConnectivityAber <- validDataPlottable %>%
    filter(region == "Aberdeenshire") %>%
    ggplot() +
    geom_density_ridges(aes(x = connectivityNorm, y = areaCol,
                            fill = areaCol), colour = NA) +
    stat_summary(aes(x = connectivityNorm, y = areaCol, colour = areaCol),
                 shape = "|",
                 size = 0.25,
                 position = position_nudge(y = -0.05),
                 fun = function(x){ggdist::median_hdci(x)[[1]]},
                 fun.min = function(x){ggdist::median_hdci(x)[[2]]},
                 fun.max = function(x){ggdist::median_hdci(x)[[3]]}
    ) +
    # geom_point(aes(x = connectivity, y = area)) +
    scale_colour_manual(values = areaPal) +
    scale_fill_manual(values = areaPal) +
    facet_grid(rows = vars(id), switch = "y") +
    labs(y = "Density by metric and individual", x = "Connectivity") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      axis.title = element_text(face = 2),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
      axis.text = element_markdown(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "none",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1),
      strip.placement = "outside"
    )

  ggsave(densityOfConnectivityAber, filename = here("figures", "densityOfConnectivity_Aberdeen.png"),
         width = 230, height = 210, units = "mm", dpi = 300)
  ggsave(densityOfConnectivityAber, filename = here("figures", "densityOfConnectivity_Aberdeen.pdf"),
         width = 230, height = 210, units = "mm")

  densityOfConnectivityWess <- validDataPlottable %>%
    filter(region == "Wessex") %>%
    ggplot() +
    geom_density_ridges(aes(x = connectivity, y = areaCol,
                            fill = areaCol), colour = NA) +
    stat_summary(aes(x = connectivity, y = areaCol, colour = areaCol),
                 shape = "|",
                 size = 0.25,
                 position = position_nudge(y = -0.05),
                 fun = function(x){ggdist::median_hdci(x)[[1]]},
                 fun.min = function(x){ggdist::median_hdci(x)[[2]]},
                 fun.max = function(x){ggdist::median_hdci(x)[[3]]}
    ) +
    # geom_point(aes(x = connectivity, y = area)) +
    scale_colour_manual(values = areaPal) +
    scale_fill_manual(values = areaPal) +
    facet_grid(rows = vars(id), switch = "y") +
    labs(y = "Density by metric and individual", x = "Connectivity") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      axis.title = element_text(face = 2),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
      axis.text = element_markdown(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "none",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1),
      strip.placement = "outside"
    )

  ggsave(densityOfConnectivityWess, filename = here("figures", "densityOfConnectivity_Wessex.png"),
         width = 230, height = 120, units = "mm", dpi = 300)
  ggsave(densityOfConnectivityWess, filename = here("figures", "densityOfConnectivity_Wessex.pdf"),
         width = 230, height = 120, units = "mm")

  # ggplot() +
  #   geom_spatraster(data = connectTerra) +
  #   # geom_point(data = availPoints, aes(x = x, y = y), colour = "red") +
  #   geom_point(data = availLandscapePoints, aes(x = x, y = y)) +
  #   # geom_sf(data = focalDeer, aes(x = x, y = y)) +
  #   # geom_sf(data = focalHR, alpha = 0.25) +
  #   coord_sf(xlim = range(focalDeer$x), ylim = range(focalDeer$y),
  #            datum = sf::st_crs(27700))

  return(list(densityOfConnectivityAber = densityOfConnectivityAber,
              densityOfConnectivityWess = densityOfConnectivityWess))

}
