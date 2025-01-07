#' Plot patch connectivity
#'
#' @name plot_patch_summary
#' @description abc
#' @return abc
#'
#' @export
plot_patch_summary <- function(MSEdf, connectRasterLocations, patchList, REGION, SELECTEDPATCHES){

  # library(dplyr)
  # library(here)
  # library(stringr)
  # library(terra)
  # library(tidyterra)
  # library(sf)
  # library(ggplot2)
  #
  # targets::tar_load("tar_connectSSF_list")
  # targets::tar_load("tar_msePois_df")
  # targets::tar_load("tar_patchList")
  # THETA <- 0.1
  # patchList <- tar_patchList
  # MSEdf <- tar_msePois_df
  # connectRasterLocations <- tar_connectSSF_list
  # connectTerra <- terra::rast(tar_connectSSF_list[[1]])
  # targets::tar_source()
  # REGION <- "Aberdeenshire"

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

  method <- str_extract(connectTerraAddr, "SSF|Pois")

  names(connectTerra) <- "connectivity"

  buffers <- c(0, 50, 100, 200, 500)
  patchesBufferedList <- vector("list", length(buffers))
  names(patchesBufferedList) <- paste0("buffer_", buffers)
  for(b in buffers){

    currPatches <- st_buffer(focalPatches, b)

    if(b == 0){
      currPatches <- focalPatches
    }

    patchMeanScore <- terra::extract(connectTerra, currPatches, fun = mean,
                                     bind = TRUE, na.rm = TRUE) %>%
      dplyr::select(Ptch_ID, connectivity) %>%
      mutate(buffer = b,
             summaryMethod = "mean") %>%
      as.data.frame()

    patchMaxScore <- terra::extract(connectTerra, currPatches, fun = max,
                                    bind = TRUE, na.rm = TRUE) %>%
      dplyr::select(Ptch_ID, connectivity) %>%
      mutate(buffer = b,
             summaryMethod = "max") %>%
      as.data.frame()

    patchMedScore <- terra::extract(connectTerra, currPatches, fun = median,
                                    bind = TRUE, na.rm = TRUE) %>%
      dplyr::select(Ptch_ID, connectivity) %>%
      mutate(buffer = b,
             summaryMethod = "median") %>%
      as.data.frame()

    patchesBufferedList[[paste0("buffer_", b)]] <- patchMeanScore %>%
      rbind(patchMaxScore) %>%
      rbind(patchMedScore)

  }

  bufferSummaries <- do.call(rbind, patchesBufferedList)

  patchAreas <- data.frame(Ptch_ID = focalPatches$Ptch_ID,
                           area_ha = as.numeric(units::set_units(st_area(focalPatches), "ha")))

  bufferSummaries <- bufferSummaries %>%
    left_join(patchAreas)

  selectedPatches <- read.csv(file = here("data", "GIS data", "patches", "Abdnshire", "Abdnshire", "abdn_final_patches.csv"))
  # selectedPatches$Patch_ID

  bufferSummaries <- bufferSummaries %>%
    mutate(selected = factor(ifelse(Ptch_ID %in% selectedPatches$Patch_ID, "Selected", "Not selected"),
                             levels = c("Not selected", "Selected")))

  paletteList

  focalPatches <- focalPatches %>%
    mutate(selected = factor(ifelse(Ptch_ID %in% selectedPatches$Patch_ID, "Selected", "Not selected"),
                             levels = c("Not selected", "Selected")))

  (patchSummaryPlot <- bufferSummaries %>%
    filter(!is.na(connectivity)) %>%
    arrange(selected) %>%
    mutate(Ptch_ID = factor(Ptch_ID, levels = unique(Ptch_ID))) %>%
    ggplot() +
    geom_vline(xintercept = 0, alpha = 0.2) +
    geom_hline(yintercept = 0, alpha = 0.2) +
    geom_point(aes(x = Ptch_ID, y = connectivity, colour = selected), alpha = 0.85, size = 0.75) +
    facet_grid(cols = vars(buffer), rows = vars(summaryMethod)) +
    scale_colour_manual(values = unname(c("#808080",
                                          paletteList$highSigLowSigNoSig[2]))) +
    labs(y = "Connectivity", x = "Patch ID order by selected", colour = "Selected patches") +
    coord_cartesian(clip = "off") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      axis.title = element_text(face = 2),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      # legend.position = "none",
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(size = 10, angle = 0, face = 4, hjust = 0, vjust = 1)))

  ggsave(plot = patchSummaryPlot, filename = here::here("figures", paste0("patchConnectivity_ID_", REGION, ".png")),
         width = 260, height = 180, units = "mm", dpi = 300)
  ggsave(plot = patchSummaryPlot, filename = here::here("figures", paste0("patchConnectivity_ID_", REGION, ".pdf")),
         width = 260, height = 180, units = "mm")

  (areaSummaryPlot <- bufferSummaries %>%
    filter(!is.na(connectivity)) %>%
    ggplot() +
    geom_vline(xintercept = 0, alpha = 0.2) +
    geom_hline(yintercept = 0, alpha = 0.2) +
    geom_point(aes(x = area_ha, y = connectivity, colour = selected), alpha = 0.85, size = 0.75) +
    scale_x_log10() +
    facet_grid(cols = vars(buffer), rows = vars(summaryMethod)) +
    scale_colour_manual(values = unname(c("#808080",
                                          paletteList$highSigLowSigNoSig[2]))) +
    labs(y = "Connectivity", x = "Area (ha)", colour = "Selected patches") +
    coord_cartesian(clip = "off") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      axis.title = element_text(face = 2),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      # legend.position = "none",
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(size = 10, angle = 0, face = 4, hjust = 0, vjust = 1)))

  ggsave(plot = areaSummaryPlot, filename = here::here("figures", paste0("patchConnectivity_area_", REGION, ".png")),
         width = 260, height = 180, units = "mm", dpi = 300)
  ggsave(plot = areaSummaryPlot, filename = here::here("figures", paste0("patchConnectivity_area_", REGION, ".pdf")),
         width = 260, height = 180, units = "mm")

  patchCentroids <- st_centroid(focalPatches) %>%
    filter(Ptch_ID %in% SELECTEDPATCHES)

  (patchMapSelected <- focalPatches %>%
    filter(Ptch_ID %in% SELECTEDPATCHES) %>%
    ggplot() +
    geom_sf(data = focalPatches, alpha = 0.5, aes(colour = selected, fill = selected)) +
    geom_sf_text(data = patchCentroids, aes(label = Ptch_ID, colour = selected), fontface = 2) +
    scale_colour_manual(values = c(NA, "#B54D17")) +
    scale_fill_manual(values = c(NA, "#B54D17"),
                      na.value = paletteList$baseGrey) +
    coord_sf(xlim = c(max(c(ext(focalPatches)[1],
                            ext(connectTerra)[1])),
                      min(c(ext(focalPatches)[2],
                            ext(connectTerra)[2]))),
             ylim = c(max(c(ext(focalPatches)[3],
                            ext(connectTerra)[3])),
                      min(c(ext(focalPatches)[4],
                            ext(connectTerra)[4]))),
             expand = 0, crs = st_crs(27700)) +
    theme_bw() +
    theme(
      text = element_text(colour = paletteList$baseGrey),
      line = element_line(colour = paletteList$baseGrey),
      plot.title = element_text(face = 2),
      axis.title = element_text(face = 2),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(angle = 0, face = 4, hjust = 0, vjust = 1),
      legend.position = "none"
    ))

  (patchOnlySelected <- bufferSummaries %>%
    filter(!is.na(connectivity), buffer == 100, summaryMethod == "mean", selected == "Selected") %>%
    ggplot() +
    geom_vline(xintercept = 0, alpha = 0.2) +
    geom_hline(yintercept = 0, alpha = 0.2) +
    geom_point(aes(x = area_ha, y = connectivity, colour = selected), alpha = 0.85, size = 2) +
    geom_text(aes(x = area_ha, y = connectivity, colour = selected, label = Ptch_ID), alpha = 1,
              hjust = 0.5, vjust = 0, fontface = 2, position = position_nudge(y = 0.05)) +
    scale_x_log10() +
    facet_grid(cols = vars(buffer), rows = vars(summaryMethod)) +
    scale_colour_manual(values = unname(paletteList$highSigLowSigNoSig[2])) +
    labs(y = "Connectivity", x = "Area (ha)", colour = "Selected patches") +
    coord_cartesian(clip = "off") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      axis.title = element_text(face = 2),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "none",
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(size = 10, angle = 0, face = 4, hjust = 0, vjust = 1)))


  ggsave(plot = patchMapSelected + patchOnlySelected,
         filename = here::here("figures", paste0("patchConnectivity_combined.png")),
         width = 280, height = 210, units = "mm", dpi = 300)
  ggsave(plot = patchMapSelected + patchOnlySelected,
         filename = here::here("figures", paste0("patchConnectivity_combined.pdf")),
         width = 280, height = 210, units = "mm")

  plotList <- list("patchSummaryPlot" = patchSummaryPlot,
                   "areaSummaryPlot" = areaSummaryPlot)

  return(plotList)

}
