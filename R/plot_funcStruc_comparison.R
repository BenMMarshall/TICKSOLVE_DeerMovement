#' Plot connectivity values against structural connectivity
#'
#' @name plot_funcStruc_comparison
#' @description abc
#' @return abc
#'
#' @export
plot_funcStruc_comparison <- function(patchData){

  # library(terra)
  # library(tidyterra)
  # library(dplyr)
  # library(sf)
  # library(here)
  # library(ggplot2)
  # library(ggtext)
  # library(patchwork)
  # library(stringr)

  # targets::tar_load("tar_patch_summaryPois")
  # targets::tar_load("tar_msePois_df")
  # targets::tar_load("tar_patchList")
  # targets::tar_source()
  # patchData <- bufferSummariesAll
  # patchData <- tar_patch_summaryPois

  # aberdeenSelectedCovar <- read.csv(here::here("data", "GIS data", "aberdeen_site_metrics.csv"))
  # patchData %>%
  #   left_join(aberdeenSelectedCovar %>%
  #               mutate(Ptch_ID = as.character(patch.id)))

  paletteList <- load_deer_palette()

  # meanMSE <- MSEdf %>%
  #   group_by(theta) %>%
  #   summarise(meanMSE = mean(mse))
  #
  # bestTheta <- meanMSE[meanMSE$meanMSE == min(meanMSE$meanMSE),]$theta
  #
  # connectTerraAddr <- connectRasterLocations[str_detect(names(connectRasterLocations),
  #                                                       sub("e-", "e.", as.character(bestTheta)))]
  # connectTerra <- terra::rast(connectTerraAddr[[1]])
  #
  # focalPatches <- selectedPatchList$AberdeenSelected

  # b <- 750
  # currPatches <- st_buffer(focalPatches, b)
  # patchMeanScore <- terra::extract(connectTerra, currPatches, fun = mean,
  #                                  bind = TRUE, na.rm = TRUE) %>%
  #   # dplyr::select(Ptch_ID, connectivity) %>%
  #   mutate(buffer = b,
  #          summaryMethod = "mean") %>%
  #   as.data.frame()

  selectedPatchData <- patchData %>%
    filter(buffer == 750) %>%
    filter(selected == "Selected")

  conAreaOUT <- lm(connectivity ~ area_km_sq, data = selectedPatchData)
  areaLab <- data.frame(patchVariable = "area_km_sq",
                        modLabel = paste0("Coef: ", signif(summary(conAreaOUT)$coef[2,1], digits = 2),
                                          "<br><i>p-value</i>: ", signif(summary(conAreaOUT)$coef[2,4], digits = 2),
                                          "<br><i>R<sup>2</sup></i>: ", signif(performance::r2(conAreaOUT)[1]$R2, digits = 2)),
                        yPos = 1.5)

  conIsoOUT <- lm(connectivity ~ iso_mean, data = selectedPatchData)
  isoLab <- data.frame(patchVariable = "iso_mean",
                       modLabel = paste0("Coef: ", signif(summary(conIsoOUT)$coef[2,1], digits = 2),
                                         "<br><i>p-value</i>: ", signif(summary(conIsoOUT)$coef[2,4], digits = 2),
                                         "<br><i>R<sup>2</sup></i>: ", signif(performance::r2(conIsoOUT)[1]$R2, digits = 2)),
                       yPos = 175)

  conDecOUT <- lm(connectivity ~ decid, data = selectedPatchData)
  decidLab <- data.frame(patchVariable = "decid",
                         modLabel = paste0("Coef: ", signif(summary(conDecOUT)$coef[2,1], digits = 2),
                                           "<br><i>p-value</i>: ", signif(summary(conDecOUT)$coef[2,4], digits = 2),
                                           "<br><i>R<sup>2</sup></i>: ", signif(performance::r2(conDecOUT)[1]$R2, digits = 2)),
                         yPos = 0.5)

  labelData <- rbind(rbind(areaLab, isoLab), decidLab) %>%
    mutate(patchVariable = case_when(
      patchVariable == "area_km_sq" ~ "Patch Area\n(square km)",
      patchVariable == "iso_mean" ~ "Mean ISO\n(square km)",
      patchVariable == "decid" ~ "Percentage\ndeciduous\nwoodland\n(%)",
      TRUE ~ patchVariable
    ))

  largePatchLabels <- selectedPatchData %>%
    filter(area_km_sq > 2.8) %>%
    select(Ptch_ID, connectivity, area_km_sq) %>%
    tidyr::pivot_longer(cols = c(area_km_sq),
                        names_to = "patchVariable", values_to = "patchValue") %>%
    mutate(patchValue = signif(patchValue, 3)) %>%
    arrange(connectivity) %>%
    mutate(hjust = c(1.2, 0.5, -0.2)) %>%
    mutate(patchVariable = case_when(
      patchVariable == "area_km_sq" ~ "Patch Area\n(square km)",
      patchVariable == "iso_mean" ~ "Mean ISO\n(square km)",
      patchVariable == "decid" ~ "Percentage\ndeciduous\nwoodland\n(%)",
      TRUE ~ patchVariable
    ))

  funcStrucPlot <- selectedPatchData %>%
    mutate(area_km_sq = ifelse(area_km_sq > 2.8, 2.8, area_km_sq)) %>%
    tidyr::pivot_longer(cols = c(area_km_sq, iso_mean, decid),
                        names_to = "patchVariable", values_to = "patchValue") %>%
    mutate(patchVariable = case_when(
      patchVariable == "area_km_sq" ~ "Patch Area\n(square km)",
      patchVariable == "iso_mean" ~ "Mean ISO\n(square km)",
      patchVariable == "decid" ~ "Percentage\ndeciduous\nwoodland\n(%)",
      TRUE ~ patchVariable
    )) %>%
    ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey25") +
    geom_point(aes(x = connectivity, y = patchValue, colour = patchVariable)) +
    geom_richtext(data = largePatchLabels,
                  aes(x = connectivity, y = 2.7, label = patchValue, hjust = hjust,
                      colour = patchVariable),
                  vjust = 1, fill = NA, fontface = 2) +
    geom_richtext(data = labelData,
                  aes(x = 0.2, y = yPos, label = modLabel,
                      colour = patchVariable), size = 4,
                  vjust = 0.5, hjust = 1, fill = NA, label.padding = unit("0.5", "lines"),
                  label.size = 1.05) +
    scale_colour_manual(values = unname(paletteList$highMidLowPal)) +
    coord_cartesian(clip = "off", xlim = c(0, NA)) +
    facet_wrap(vars(patchVariable), ncol = 1, drop = TRUE, scales = "free_y",
               strip.position = "left") +
    labs(y = "", x = "Connectivity") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      axis.line.y = element_line(colour = "grey25"),
      plot.title = element_text(face = 2),
      axis.title = element_text(face = 2),
      # axis.ticks.y = element_blank(),
      axis.text.y = element_markdown(colour = "grey25"),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
    )

  funcStrucPlot

  ggsave(plot = funcStrucPlot, filename = here::here("figures", paste0("funcStrucConnectPlot.png")),
         width = 240, height = 190, units = "mm", dpi = 300)
  ggsave(plot = funcStrucPlot, filename = here::here("figures", paste0("funcStrucConnectPlot.pdf")),
         width = 240, height = 190, units = "mm")

  return(funcStrucPlot)
}
