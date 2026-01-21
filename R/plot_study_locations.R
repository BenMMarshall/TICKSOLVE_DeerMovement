#' Build study site location plot
#'
#' @name plot_study_locations
#' @description abc
#' @return abc
#'
#' @export
plot_study_locations <- function(deerData, landuseList, patchList){

  # library(here)
  # library(dplyr)
  # library(sf)
  # library(terra)
  # library(tidyterra)
  # library(ggplot2)
  # library(rnaturalearth)
  # library(stringr)
  # library(ggrepel)
  # library(patchwork)

  # targets::tar_load("tar_deerData")
  # targets::tar_source()
  # deerData <- tar_deerData

  paletteList <- load_deer_palette()

  regionLocations <- deerData %>%
    group_by(region) %>%
    summarise(meanLon = mean(Longitude),
              meanLat = mean(Latitude)) %>%
    st_as_sf(coords = c("meanLon", "meanLat"), crs = 4326) %>%
    st_transform(crs = "+proj=moll")

  worldData <- ne_countries(scale = "medium", type = "map_units",
                            returnclass = "sf")

  geodata::gadm(country = "GB",
                path = here("data", "GIS data"),
                level = 2,
                version = "latest")
  gbGADM <- readRDS(here("data", "GIS data", "gadm", "gadm41_GBR_2_pk.rds"))
  gbGADM <- st_as_sf(gbGADM)
  # gbGADM <- st_transform(gbGADM, 27700)

  # Local maps --------------------------------------------------------------

  # targets::tar_load("tar_patchList")
  # patchList <- tar_patchList
  #
  # targets::tar_load("tar_landuseList")
  # landuseList <- tar_landuseList

  regions <- c("Aberdeenshire", "Wessex")

  stackLayers <- read_stack_layers()

  plotList <- vector("list", length(regions))
  names(plotList) <- regions
  for(focalRegion in regions){

    # focalRegion <- "Aberdeenshire"
    regionLocations_sel <- regionLocations[regionLocations$region %in% focalRegion,]
    (euroMap <- ggplot() +
        geom_sf(data = worldData %>%
                  # st_crop(xmin = -50, xmax = 45,
                  #         ymin = 10, ymax = 73) %>%
                  st_transform(crs = "+proj=moll"),
                aes(fill = sovereignt == "United Kingdom"),
                colour = NA) +
        geom_sf(data = regionLocations_sel,
                aes(),
                fill = paletteList$deerSpeciesPal[1], colour = paletteList$baseGrey,
                size = 5, pch = 21) +
        # geom_sf_label(data = regionLocations_sel, aes(label = region),
        #               size = 4, nudge_y = 75000, hjust = 0, fontface = 4,
        #               colour = paletteList$deerSpeciesPal[1], label.size = 0.75,
        #               box.padding = unit(1, "lines"), label.padding = unit(1.5, "mm")) +
        # annotate("text", x = -600000, y = 6700000, label = "United\nKingdom",
        #          size = 6, fontface = 2, hjust = 1, lineheight = 0.95,
        #          colour = paletteList$corePal[["Woodland"]]) +
        scale_fill_manual(values = c(
          "FALSE" = "#ffffff",
          "TRUE" = paletteList$corePal[["Woodland"]]
        ), na.value = "grey85") +
        scale_x_continuous(limits = c(-600000, 150000), breaks = seq(-15, 20, 5)) +
        scale_y_continuous(limits = c(5800000, 6800000), breaks = seq(50, 60, 5)) +
        coord_sf(crs = "+proj=moll", label_graticule = "NE") +
        theme_minimal() +
        labs(x = "", y = "") +
        theme(text = element_text(colour = paletteList$baseGrey),
              line = element_line(colour = paletteList$baseGrey),
              axis.text = element_blank(),
              panel.grid = element_blank(),
              plot.background = element_blank(),
              panel.background = element_rect(fill = "#ffffff", colour = NA),
              panel.border = element_blank(),
              legend.position = "none")
    )

    if(focalRegion == "Aberdeenshire" | focalRegion == "Aberdeen"){
      focalPatches <- patchList[["Aberdeen"]]
      focalRoads <- landuseList$Aberdeen$roads
      croppedStack <- stackLayers %>%
        crop(focalPatches)
    } else {
      focalPatches <- patchList[["Wessex"]]
      focalRoads <- landuseList$Wessex$roads
      croppedStack <- stackLayers %>%
        crop(focalPatches)
    }

    meanLocations <- deerData %>%
      left_join(tribble(~Animal_ID, ~woodlandName,
                        "Roe01_F", "Gask Wood",
                        "Roe02_F", "Gask Wood",
                        "Roe03_M", "Kings Garn",
                        "Roe04_F", "Gask Wood",
                        "Roe05_F", "Muir of Dinnet",
                        "Roe06_F", "Moss of Air",
                        "Roe07_F", "Holly Hatch",
                        "Roe08_M", "Gask Wood",
                        "Roe09_M", "Black Hillocks",
                        "Roe10_F", "Muir of Dinnet",
                        "Roe11_F", "Well House Wood",
                        "Roe12_F", "Well House Wood",
                        "Roe13_F", "Moss of Air",
                        "Roe14_M", "Well House Wood",
                        "Roe15_F", "Moss of Air")) %>%
      group_by(Animal_ID, region, woodlandName) %>%
      summarise(meanx = mean(x),
                meany = mean(y)) %>%
      filter(region == focalRegion,
             str_detect(Animal_ID, "Roe"))

    woodlandLabels <- meanLocations %>%
      group_by(woodlandName) %>%
      summarise(meanWoodlandx = mean(meanx),
                meanWoodlandy = mean(meany))

    scaleLocation <- data.frame(x = c(st_bbox(focalPatches)[[1]] + 1000,
                                      st_bbox(focalPatches)[[1]] + 11000),
                                y = c(st_bbox(focalPatches)[[4]] - 1000,
                                      st_bbox(focalPatches)[[4]] - 1000))

    scaleLocation <- scaleLocation %>%
      st_as_sf(coords = c("x", "y"), remove = FALSE, crs = 27700)

    scaleLocationXY <- data.frame(
      x = scaleLocation$x[1],
      xend = scaleLocation$x[2],
      y = scaleLocation$y[1],
      yend = scaleLocation$y[2]
    )
    (localMap <- ggplot() +
        geom_spatraster(data = croppedStack, aes(fill = Deciduous_Broadleaf_Forest),
                        inherit.aes = TRUE) +
        geom_spatraster(data = croppedStack, aes(fill = Evergreen_Needleleaf_Forest)) +
        geom_sf(data = focalRoads,
                colour = paletteList$highSigLowSigNoSig[["Not Significant"]], linewidth = 0.15,
                alpha = 0.65) +
        geom_segment(data = scaleLocationXY, aes(x = x, y = y,
                                                 xend = xend, yend = yend),
                     colour = paletteList$baseGrey, linewidth = 0.9) +
        annotate("text", x = scaleLocationXY$x[1], y = scaleLocationXY$y[1],
                 label = "10km", colour = paletteList$baseGrey, size = 5, hjust = 0, vjust = 1.3,
                 fontface = 2) +
        # geom_label_repel(data = meanLocations, aes(x = meanx, y = meany, label = Animal_ID),
        #                  colour = paletteList$deerSpeciesPal[1], size = 3, force = 0.2, force_pull = 1,
        #                  box.padding = unit(0.35, "lines"), label.padding = unit(1.5, "mm"),
        #                  label.size = 0.75, segment.size = 0.95,
        #                  max.overlaps = 20, max.iter = 1000000,
        #                  seed = 2025, fontface = 2) +
        geom_label_repel(data = woodlandLabels, aes(x = meanWoodlandx, y = meanWoodlandy, label = woodlandName),
                         colour = paletteList$deerSpeciesPal[1], size = 3, force = 0.2, force_pull = 1,
                         box.padding = unit(0.35, "lines"), label.padding = unit(1.5, "mm"),
                         label.size = 0.75, segment.size = 0.95,
                         max.overlaps = 20, max.iter = 1000000,
                         seed = 2025, fontface = 2) +
        geom_point(data = meanLocations, aes(x = meanx, y = meany),
                   fill = paletteList$deerSpeciesPal[1], colour = paletteList$baseGrey,
                   size = 3, pch = 21) +
        scale_fill_manual(values = c(
          "0" = "#ffffff",
          "1" = paletteList$corePal[["Woodland"]]
        ), na.value = "grey85") +
        coord_sf(expand = 0) +
        labs(x = "", y = "", fill = "Forested Areas", title = focalRegion) +
        theme_void() +
        theme(text = element_text(colour = paletteList$baseGrey),
              line = element_line(colour = paletteList$baseGrey),
              axis.text = element_blank(),
              axis.line = element_blank(),
              plot.title = element_text(size = 16, face = 4, colour = paletteList$baseGrey),
              legend.position = "none") +
      scale_y_continuous(position = "right"))

    plotList[[focalRegion]] <- localMap +
      inset_element(euroMap, left = 0.65, bottom = 0.65, right = 1.15, top = 1, align_to = "full")

  }

  # fullPlot <- wrap_plots(
  #   euroMap + (plotList[[1]] / plotList[[2]])
  # )

  fullPlot <- plotList[[1]] + plotList[[2]]

  ggsave(plot = fullPlot, filename = here("figures", "studyLocationMapRoe.png"),
         width = 300, height = 130, dpi = 300, units = "mm")

  return(fullPlot)

}
