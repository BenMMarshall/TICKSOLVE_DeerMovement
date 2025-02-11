#' Map out patch summaries
#'
#' @name plot_patch_connectivity
#' @description abc
#' @return abc
#'
#' @export
plot_patch_connectivity <- function(MSEdf, connectRasterLocations, patchList, REGION, bufferSummaries){

  # targets::tar_load("tar_connectPois_list")
  # targets::tar_load("tar_msePois_df")
  # targets::tar_load("tar_patchList")
  # targets::tar_load("tar_patch_summaryPois")
  # bufferSummaries <- tar_patch_summaryPois
  # THETA <- 0.1
  # patchList <- tar_patchList
  # MSEdf <- tar_msePois_df
  # connectRasterLocations <- tar_connectPois_list
  # connectTerra <- terra::rast(tar_connectPois_list[[1]])
  # targets::tar_source()
  # REGION <- "Aberdeenshire"

  SELECTEDPATCHES <- patchList$AberdeenSelected$Ptch_ID

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
    filter(!duplicated(Ptch_ID)) %>%
    mutate(selected = factor(ifelse(Ptch_ID %in% SELECTEDPATCHES, "Selected", "Not selected"),
                             levels = c("Not selected", "Selected")))

  method <- str_extract(connectTerraAddr, "SSF|Pois")

  names(connectTerra) <- "connectivity"

  (connectRasterPlot <- ggplot() +
      geom_spatraster(data = connectTerra, aes()) +
      # geom_sf(data = focalPatches, alpha = 0.1) +
      # geom_sf(data = focalRoads, colour = "#000000", alpha = 0.25) +
      scale_fill_gradient(high = scales::muted("#B54D17"),
                          low = "#ffffff",
                          na.value = paletteList$baseGrey, trans = "sqrt") +
      labs(fill = "Connectivity", title = "By Cell Connectivity") +
      coord_sf(xlim = c(max(c(ext(focalPatches)[1],
                              ext(connectTerra)[1])),
                        min(c(ext(focalPatches)[2],
                              ext(connectTerra)[2]))),
               ylim = c(max(c(ext(focalPatches)[3],
                              ext(connectTerra)[3])),
                        min(c(ext(focalPatches)[4],
                              ext(connectTerra)[4]))),
               expand = 0, datum = st_crs(27700)) +
      scale_x_continuous(breaks = seq(340000, 380000, 20000)) +
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
        strip.text = element_text(angle = 0, face = 4, hjust = 0, vjust = 1)
        # legend.position = "none"
      ))

  patchMeanScore <- terra::extract(connectTerra, focalPatches, fun = mean,
                                   bind = TRUE, na.rm = TRUE) %>%
    rename(meanConnectivity = connectivity)

  (meanConnectPatchPlot <- ggplot() +
      # geom_spatraster(data = connectTerra, aes(), alpha = 0.35) +
      geom_sf(data = patchMeanScore, aes(fill = meanConnectivity, colour = selected, linewidth = selected)) +
      scale_linewidth_manual(values = c(0, 0.25)) +
      scale_colour_manual(values = c(NA, unname(paletteList$highSigLowSigNoSig[2]))) +
      scale_fill_gradient(high = scales::muted("#B54D17"),
                          low = "#ffffff",
                          na.value = paletteList$baseGrey, trans = "sqrt") +
      labs(fill = "Connectivity", title = "Mean Connectivity", colour = "Selected Patches", linewidth = "Selected Patches") +
      # geom_sf(data = focalRoads, colour = "#000000", alpha = 0.25) +
      coord_sf(xlim = c(max(c(ext(focalPatches)[1],
                              ext(connectTerra)[1])),
                        min(c(ext(focalPatches)[2],
                              ext(connectTerra)[2]))),
               ylim = c(max(c(ext(focalPatches)[3],
                              ext(connectTerra)[3])),
                        min(c(ext(focalPatches)[4],
                              ext(connectTerra)[4]))),
               expand = 0, datum = st_crs(27700)) +
      scale_x_continuous(breaks = seq(340000, 380000, 20000)) +
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
        strip.text = element_text(angle = 0, face = 4, hjust = 0, vjust = 1)
        # legend.position = "none"
      )
  )

  patchCentroids <- st_centroid(focalPatches)
  patchCentroidsCoords <- sf::st_coordinates(patchCentroids)
  patchCentroids <- patchCentroids %>%
    mutate(easting = patchCentroidsCoords[,1],
           northing = patchCentroidsCoords[,2],
           Ptch_ID = as.character(Ptch_ID)) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(Ptch_ID, easting, northing) %>%
    as.data.frame()

  (patchSummaryPlot <- bufferSummaries %>%
      filter(!is.na(connectivity), summaryMethod == "mean", buffer == 750) %>%
      left_join(patchCentroids) %>%
      arrange(selected) %>%
      mutate(selected = factor(selected, levels = c("Not selected", "Selected"))) %>%
      ggplot() +
      # geom_vline(xintercept = 0, alpha = 0.2) +
      # geom_hline(yintercept = 0, alpha = 0.2) +
      geom_point(aes(x = easting, y = connectivity, colour = selected,
                     size = selected, shape = selected), alpha = 0.85) +
      # facet_grid(cols = vars(buffer), rows = vars(summaryMethod)) +
      scale_colour_manual(values = unname(c("#808080",
                                            paletteList$highSigLowSigNoSig[2]))) +
      labs(y = "Connectivity", x = "Easting", colour = "Selected patches",
           size = "Selected patches", shape = "Selected patches") +
      coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = seq(340000, 380000, 20000)) +
      scale_y_sqrt() +
      scale_size_manual(values = c(2, 6)) +
      scale_shape_manual(values = c(16, 18)) +
      theme_bw() +
      theme(
        text = element_text(colour = "grey25"),
        line = element_line(colour = "grey25"),
        axis.title = element_text(face = 2),
        # axis.text.x = element_blank(),
        # axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        # legend.position = "none",
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(size = 10, angle = 0, face = 4, hjust = 0, vjust = 1)))


  (comboPlot <- (connectRasterPlot + meanConnectPatchPlot + plot_layout(guides = "collect") +
                   theme(axis.text.y = element_blank(),
                         axis.ticks.y = element_blank())) /
      patchSummaryPlot +
      # plot_layout(guides = "collect") +
      plot_annotation(
        title = paste0(REGION, " landscape connectivity and patches"),
        subtitle = paste0("theta = ", bestTheta, #", max patch distance = ", patchDistance,
                          #", repeats per pair = ", repeatsPerPair,
                          ", model = ", method#,
                          # ", MSE = ", format(min(meanMSE$meanMSE), digits = 4)
                          )) &
      theme(plot.title = element_text(hjust = 0, face = 2),
            plot.subtitle = element_text(hjust = 0, face = 3),
            text = element_text(colour = paletteList$baseGrey))
  )

  ggsave(plot = comboPlot, filename = here::here("figures", paste0("patchConnectivity_", method, "_", REGION, ".png")),
         width = 220, height = 180, units = "mm", dpi = 300)
  ggsave(plot = comboPlot, filename = here::here("figures", paste0("patchConnectivity_", method, "_", REGION, ".pdf")),
         width = 220, height = 180, units = "mm")

  return(comboPlot)

}
