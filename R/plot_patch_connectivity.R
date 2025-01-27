#' Map out patch summaries
#'
#' @name plot_patch_connectivity
#' @description abc
#' @return abc
#'
#' @export
plot_patch_connectivity <- function(MSEdf, connectRasterLocations, patchList, REGION){

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
                          na.value = paletteList$baseGrey) +
      labs(fill = "Connectivity", title = "By Cell Connectivity") +
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
        strip.text = element_text(angle = 0, face = 4, hjust = 0, vjust = 1)
        # legend.position = "none"
      ))

  patchMeanScore <- terra::extract(connectTerra, focalPatches, fun = mean,
                                   bind = TRUE, na.rm = TRUE) %>%
    rename(meanConnectivity = connectivity)

  patchMeanScore <- terra::extract(connectTerra, patchMeanScore, fun = max,
                                   bind = TRUE, na.rm = TRUE) %>%
    rename(maxConnectivity = connectivity)

  patchMeanScore <- terra::extract(connectTerra, patchMeanScore, fun = median,
                                   bind = TRUE, na.rm = TRUE) %>%
    rename(medianConnectivity = connectivity)

  meanConnectPatchPlot <- ggplot() +
    geom_spatraster(data = connectTerra, aes(), alpha = 0.35) +
    geom_sf(data = patchMeanScore, aes(fill = meanConnectivity, colour = selected, linewidth = selected)) +
    scale_linewidth_manual(values = c(0, 0.25)) +
    scale_colour_manual(values = c(NA, "#303030")) +
    scale_fill_gradient(high = scales::muted("#B54D17"),
                        low = "#ffffff",
                        na.value = paletteList$baseGrey) +
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
      strip.text = element_text(angle = 0, face = 4, hjust = 0, vjust = 1)
      # legend.position = "none"
    )

  maxConnectPatchPlot <- ggplot() +
    geom_spatraster(data = connectTerra, aes(), alpha = 0.35) +
    geom_sf(data = patchMeanScore, aes(fill = maxConnectivity, colour = selected, linewidth = selected)) +
    scale_linewidth_manual(values = c(0, 0.25)) +
    scale_colour_manual(values = c(NA, "#303030")) +
    scale_fill_gradient(high = scales::muted("#B54D17"),
                        low = "#ffffff",
                        na.value = paletteList$baseGrey) +
    labs(fill = "Connectivity", title = "Max Connectivity", colour = "Selected Patches", linewidth = "Selected Patches") +
    # geom_sf(data = focalRoads, colour = "#000000", alpha = 0.25) +
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
      strip.text = element_text(angle = 0, face = 4, hjust = 0, vjust = 1)
      # legend.position = "none"
    )

  medianConnectPatchPlot <- ggplot() +
    geom_spatraster(data = connectTerra, aes(), alpha = 0.35) +
    geom_sf(data = patchMeanScore, aes(fill = medianConnectivity, colour = selected, linewidth = selected)) +
    scale_linewidth_manual(values = c(0, 0.25)) +
    scale_colour_manual(values = c(NA, "#303030")) +
    scale_fill_gradient(high = scales::muted("#B54D17"),
                        low = "#ffffff",
                        na.value = paletteList$baseGrey) +
    labs(fill = "Connectivity", title = "Median Connectivity", colour = "Selected Patches", linewidth = "Selected Patches") +
    # geom_sf(data = focalRoads, colour = "#000000", alpha = 0.25) +
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
      strip.text = element_text(angle = 0, face = 4, hjust = 0, vjust = 1)
      # legend.position = "none"
    )

  (comboPlot <- (connectRasterPlot + maxConnectPatchPlot + plot_layout(guides = "collect") +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())) /
    (meanConnectPatchPlot + medianConnectPatchPlot + plot_layout(guides = "collect") +
       theme(axis.text.y = element_blank(),
             axis.ticks.y = element_blank())) +
    # plot_layout(guides = "collect") +
    plot_annotation(
      title = paste0(REGION, " Landscape connectivity and patches"),
      subtitle = paste0("theta = ", bestTheta, #", max patch distance = ", patchDistance,
                        #", repeats per pair = ", repeatsPerPair,
                        ", model = ", method,
                        ", MSE = ", format(min(meanMSE$meanMSE), digits = 4))) &
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
