#' Generate overview maps per deer
#'
#' @name generate_overview_maps
#' @description AKDE outputs mapped against land use and patches.
#' @return A list of ggplot object, and a saved file per individual.
#'
#' @export
generate_overview_maps <- function(deerData, akdeLists, landuseList, patches){

  # library(here)
  # library(dplyr)
  # library(ggplot2)
  # library(sf)
  # library(ctmm)
  # library(stringr)
  # library(terra)
  # library(tidyterra)
  # library(patchwork)

  # targets::tar_source()
  paletteList <- generate_palettes()
  #
  # targets::tar_load("tar_akdeLists")
  # targets::tar_load("tar_deerData")
  # targets::tar_load("tar_landuse")
  # targets::tar_load("tar_patches")
  #
  # akdeLists <- tar_akdeLists
  # deerData <- tar_deerData
  # landuseList <- tar_landuse
  # patches <- tar_patches

  # landAberdeen <- terra::rast(here("data", "GIS data", "landuseAberdeen.tif"))
  # landNewForest <- terra::rast(here("data", "GIS data", "landuseWessex.tif"))

  # landAberdeen <- terra::rast(landuseList$aber$landuse)
  # landNewForest <- terra::rast(landuseList$wess$landuse)

  # mean(list(listAKDE$Roe15_F, listAKDE$Roe12_F))
  # mean(tar_akdeLists$akde)

  allSF <- do.call(rbind, akdeLists$sf) %>%
    mutate(Animal_ID = str_extract(name, ".+?(?=\\s)"),
           level = str_extract(name, "..\\%"),
           ci = str_extract(name, "low$|est$|high$")) %>%
    left_join(deerData %>%
                group_by(Animal_ID, region, Sex) %>%
                slice_head(n = 1) %>%
                select(Animal_ID, region, Sex))

  # landAberdeen <- landAberdeen %>%
  #   mutate(LCM_1_cat = paste0("LCM_", LCM_1))
  plotList <- vector("list", length = length(names(akdeLists$sf)))
  names(plotList) <- names(akdeLists$sf)
  for(animal in names(akdeLists$sf)){
    # animal <- "Roe03_M"
    deerData <- st_as_sf(deerData, coords = c("x","y"), remove = FALSE,
                         crs = 27700)
    focalDeer <- deerData %>%
      filter(Animal_ID == animal)

    focalSF <- allSF %>%
      filter(Animal_ID == animal)


    if(focalDeer$region[1] == "Aberdeenshire"){
      focalPatches <- patches$aber
    } else {
      focalPatches <- patches$wess
    }

    if(focalDeer$region[1] == "Aberdeenshire"){
      focalRoad <- landuseList$aber$road
    } else {
      focalRoad <- landuseList$wess$road
    }

    if(focalDeer$region[1] == "Aberdeenshire"){
      focalLand <- terra::rast(landuseList$aber$landuse)
    } else {
      focalLand <- terra::rast(landuseList$wess$landuse)
    }
    focalLand <- focalLand %>%
      mutate(LCM_1_cat = paste0("LCM_", LCM_1))

    scaleBar <- data.frame(xStart = round(extent(focalDeer)[2], digits = 1),
                           xEnd = round(extent(focalDeer)[2], digits = 1) - 200,
                           yStart = round(extent(focalDeer)[3], digits = 1),
                           yEnd = round(extent(focalDeer)[3], digits = 1) + 50)

    LCM1Plot  <- focalSF %>%
      ggplot() +
      geom_spatraster(data = focalLand, aes(fill = LCM_1_cat)) +
      scale_fill_manual(values = paletteList$UKCEHcoloursVecColours, labels = paletteList$UKCEHcoloursVecNames,
                        na.value = "red") +
      # geom_spatraster(data = focalLand, aes(fill = LCM_2)) +
      # scale_fill_gradient(low = "#becf98", high = "#5d7c0a", na.value = "red") +
      geom_sf(data = focalPatches, aes(), colour = "#455714", fill = NA,
              linewidth = 0.95) +
      geom_sf(data = focalRoad, aes(linewidth = roadSize), colour = "grey10") +
      scale_linewidth_manual(values = c("A roads" = 1.5, "B roads" = 1, "C roads" = 0.5, "Other" = 0.25)) +
      geom_sf(aes(colour = level, linetype = !ci == "est"), alpha = 0.25,
              fill = NA, linewidth = 0.5) +
      scale_colour_manual(values = c("grey85", "grey65")) +
      geom_sf(data = focalDeer, aes(), colour = "#000000", alpha = 0.5, shape = 4, size = 0.5) +
      geom_path(data = focalDeer, aes(x = x, y = y), colour = "#000000", alpha = 0.1) +
      geom_segment(data = scaleBar, aes(x = xStart, xend = xEnd, y = yStart, yend = yStart),
                   linewidth = 2,
                   colour = "grey15") +
      geom_segment(data = scaleBar, aes(x = xStart, xend = xStart, y = yStart, yend = yEnd),
                   linewidth = 2, arrow = arrow(type = "closed", length = unit(0.2, "cm")),
                   colour = "grey15") +
      geom_point(data = scaleBar, aes(x = xStart, y = yStart), size = 4,
                 colour = "grey15") +
      coord_sf(xlim = extent(focalDeer)[1:2]+c(-100, 100), ylim = extent(focalDeer)[3:4]+c(-100, 100),
               default_crs = sf::st_crs(27700)) +
      labs(colour = "Home Range Contour", linetype = "95% CI", fill = "UKCEH LCM 1",
           title = animal, x = "Longitude", y = "Latitude", linewidth = "Road Size") +
      theme_bw() +
      theme(
        text = element_text(colour = "grey25"),
        line = element_line(colour = "grey25"),
        plot.title = element_text(face = 4),
        axis.title = element_text(face = 2),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        # legend.position = "none",
        legend.title = element_text(face = 2),
        strip.background = element_blank(),
        strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1),
        strip.placement = "outside"
      )

    LCM2Plot <- focalSF %>%
      ggplot() +
      # geom_spatraster(data = focalLand, aes(fill = LCM_1_cat)) +
      # scale_fill_manual(values = paletteList$UKCEHcoloursVecColours, labels = paletteList$UKCEHcoloursVecNames,
      #                   na.value = "red") +
      geom_spatraster(data = focalLand, aes(fill = LCM_2)) +
      scale_fill_gradient(low = "#becf98", high = "#5d7c0a", na.value = "red") +
      geom_sf(data = focalPatches, aes(), colour = "#455714", fill = NA,
              linewidth = 0.95) +
      geom_sf(data = focalRoad, aes(linewidth = roadSize), colour = "grey10") +
      scale_linewidth_manual(values = c("A roads" = 1.5, "B roads" = 1, "C roads" = 0.5, "Other" = 0.25)) +
      geom_sf(aes(colour = level, linetype = !ci == "est"), alpha = 0.25,
              fill = NA, linewidth = 0.5) +
      scale_colour_manual(values = c("grey85", "grey65")) +
      geom_sf(data = focalDeer, aes(), colour = "#000000", alpha = 0.5, shape = 4, size = 0.5) +
      geom_path(data = focalDeer, aes(x = x, y = y), colour = "#000000", alpha = 0.1) +
      geom_segment(data = scaleBar, aes(x = xStart, xend = xEnd, y = yStart, yend = yStart),
                   linewidth = 2,
                   colour = "grey15") +
      geom_segment(data = scaleBar, aes(x = xStart, xend = xStart, y = yStart, yend = yEnd),
                   linewidth = 2, arrow = arrow(type = "closed", length = unit(0.2, "cm")),
                   colour = "grey15") +
      geom_point(data = scaleBar, aes(x = xStart, y = yStart), size = 4,
                 colour = "grey15") +
      coord_sf(xlim = extent(focalDeer)[1:2]+c(-100, 100), ylim = extent(focalDeer)[3:4]+c(-100, 100),
               default_crs = sf::st_crs(27700)) +
      labs(colour = "Home Range Contour", linetype = "95% CI", fill = "UKCEH LCM 1",
           title = animal, x = "Longitude", y = "Latitude", linewidth = "Road Size") +
      theme_bw() +
      theme(
        text = element_text(colour = "grey25"),
        line = element_line(colour = "grey25"),
        plot.title = element_text(face = 4),
        axis.title = element_text(face = 2),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        # legend.position = "none",
        legend.title = element_text(face = 2),
        strip.background = element_blank(),
        strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1),
        strip.placement = "outside"
      )


    comboPlot <- LCM1Plot + LCM2Plot +
      patchwork::plot_layout(guides = "collect")

    ggsave(plot = comboPlot, filename = here("figures", paste0("overviewMaps_", animal, ".png")),
           width = 320, height = 220, units = "mm", dpi = 300)

    plotList[[animal]] <- comboPlot

  }

  return(plotList)

}

