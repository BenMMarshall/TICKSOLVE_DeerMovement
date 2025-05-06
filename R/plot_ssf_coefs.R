#' Create a list of ssf coef plots
#'
#' @name plot_ssf_coefs
#' @description abc
#' @return abc
#'
#' @export
plot_ssf_coefs <- function(ssfExtractList){
  # targets::tar_load("tar_ssf_extracts")
  # ssfExtractList <- tar_ssf_extracts
  # library(amt)
  # library(dplyr)
  # library(IndRSA)
  # library(tidyr)
  # library(ggplot2)
  # library(ggridges)
  # library(ggtext)
  # targets::tar_source()

  list2env(ssfExtractList, envir = environment())

  paletteList <- load_deer_palette()

  # indiDataframe %>%
  #   arrange(desc(Animal_ID_colour)) %>%
  #   mutate(Animal_ID_colour = "<i>Individuals</i>") %>%
  #   filter(!is.na(est),
  #          variableType == "Landuse",
  #          !variableType == "Step",
  #          !variable == "cos ta_ ") %>%
  #   ggplot() +
  #   geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25, colour = "grey25") +
  #   geom_hline(yintercept = 1.5, linetype = "solid", linewidth = 0.25, colour = "grey25") +
  #   geom_hline(yintercept = 0.5, linetype = "solid", linewidth = 0.25, colour = "grey25") +
  #   geom_segment(data = popAvg %>%
  #                  filter(variableType == "Landuse",
  #                         !variableType == "Step",
  #                         !variable == "cos ta_ "),
  #                aes(x = LCI, xend = UCI, y = Animal_ID_colour, yend = Animal_ID_colour, colour = sig),
  #                linewidth = 1.2) +
  #   geom_point(data = popAvg %>%
  #                filter(variableType == "Landuse",
  #                       !variableType == "Step",
  #                       !variable == "cos ta_ "),
  #              aes(x = Mean, y = Animal_ID_colour, colour = sig),
  #              size = 2) +
  #   geom_text(data = popAvg %>%
  #               filter(variableType == "Landuse",
  #                      !variableType == "Step",
  #                      !variable == "cos ta_ "),
  #             aes(x = Inf, y = Animal_ID_colour, colour = sig, label = signif(Mean, digits = 2)),
  #             hjust = 1, size = 3) +
    # geom_point(aes(x = est, y = Animal_ID_colour, colour = sig), size = 0.65,
    #            position = position_jitter(width = 0, height = 0.25)) +
  #   facet_grid(rows = vars(variable), scales = "free", space = "free", drop = TRUE,
  #              switch = "y") +
  #   scale_colour_manual(values = paletteList$highSigLowSigNoSig) +
  #   labs(x = "Selection Strength Coefficient", y = "") +
  #   theme_bw() +
  #   theme(
  #     text = element_text(colour = "grey25"),
  #     line = element_line(colour = "grey25"),
  #     panel.grid.minor = element_blank(),
  #     panel.border = element_blank(),
  #     panel.grid = element_blank(),
  #     legend.position = "none",
  #     plot.title = element_text(face = 2),
  #     axis.title = element_text(face = 2),
  #     axis.ticks = element_blank(),
  #     axis.text.y = element_markdown(size = 7),
  #     strip.placement = "outside",
  #     strip.background = element_blank(),
  #     strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1, colour = "grey25")
  #   ) +
  #   guides(colour = guide_legend(override.aes = list(linewidth = 0)))

  coefLanduse <- indiDataframe %>%
    arrange(desc(Animal_ID_colour)) %>%
    mutate(Animal_ID_colour = "<i>Individuals</i>") %>%
    filter(!is.na(est),
           variableType == "Landuse",
           !variableType == "Step",
           !variable == "cos ta_ ") %>%
    filter(!is.na(est)) %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25, colour = "grey25") +
    geom_hline(yintercept = 1.5, linetype = "solid", linewidth = 0.25, colour = "grey25") +
    geom_hline(yintercept = 0.5, linetype = "solid", linewidth = 0.25, colour = "grey25") +
    geom_segment(data = popAvg %>%
                   filter(variableType == "Landuse",
                          !variableType == "Step",
                          !variable == "cos ta_ "),
                 aes(x = LCI, xend = UCI, y = Animal_ID_colour, yend = Animal_ID_colour, colour = sig),
                 linewidth = 1.2) +
    geom_point(data = popAvg %>%
                 filter(variableType == "Landuse",
                        !variableType == "Step",
                        !variable == "cos ta_ "),
               aes(x = Mean, y = Animal_ID_colour, colour = sig),
               size = 2) +
    geom_text(data = popAvg %>%
                filter(variableType == "Landuse",
                       !variableType == "Step",
                       !variable == "cos ta_ "),
              aes(x = Inf, y = Animal_ID_colour, colour = sig, label = signif(Mean, digits = 2)),
              hjust = 1.1, size = 3) +
    geom_point(aes(x = est, y = Animal_ID_colour, colour = sig), size = 0.65,
               position = position_jitter(width = 0, height = 0.25)) +
    facet_grid(rows = vars(variable), scales = "free", space = "free", drop = TRUE,
               switch = "y") +
    scale_colour_manual(values = paletteList$highSigLowSigNoSig) +
    labs(x = "Selection Strength Coefficient", y = "") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.title = element_text(face = 2),
      axis.title = element_text(face = 2),
      axis.ticks = element_blank(),
      axis.text.y = element_markdown(size = 8),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1, colour = "grey25")
    ) +
    guides(colour = guide_legend(override.aes = list(linewidth = 0)))

  coefDistance <- indiDataframe %>%
    arrange(desc(Animal_ID_colour)) %>%
    mutate(Animal_ID_colour = "<i>Individuals</i>") %>%
    filter(!is.na(est),
           variableType == "Distance",
           !variableType == "Step",
           !variable == "cos ta_ ") %>%
    filter(!is.na(est)) %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25, colour = "grey25") +
    geom_hline(yintercept = 1.5, linetype = "solid", linewidth = 0.25, colour = "grey25") +
    geom_hline(yintercept = 0.5, linetype = "solid", linewidth = 0.25, colour = "grey25") +
    geom_segment(data = popAvg %>%
                   filter(variableType == "Distance",
                          !variableType == "Step",
                          !variable == "cos ta_ "),
                 aes(x = LCI, xend = UCI, y = Animal_ID_colour, yend = Animal_ID_colour, colour = sig),
                 linewidth = 1.2) +
    geom_point(data = popAvg %>%
                 filter(variableType == "Distance",
                        !variableType == "Step",
                        !variable == "cos ta_ "),
               aes(x = Mean, y = Animal_ID_colour, colour = sig),
               size = 2) +
    geom_text(data = popAvg %>%
                filter(variableType == "Distance",
                       !variableType == "Step",
                       !variable == "cos ta_ "),
              aes(x = Inf, y = Animal_ID_colour, colour = sig, label = signif(Mean, digits = 2)),
              hjust = 1.1, size = 3) +
    geom_point(aes(x = est, y = Animal_ID_colour, colour = sig), size = 0.65,
               position = position_jitter(width = 0, height = 0.25)) +
    facet_grid(rows = vars(variable), scales = "free", space = "free", drop = TRUE,
               switch = "y") +
    scale_colour_manual(values = paletteList$highSigLowSigNoSig) +
    labs(x = "Selection Strength Coefficient", y = "") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.title = element_text(face = 2),
      axis.title = element_text(face = 2),
      axis.ticks = element_blank(),
      axis.text.y = element_markdown(size = 8),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1, colour = "grey25")
    ) +
    guides(colour = guide_legend(override.aes = list(linewidth = 0)))

  coefOther <- indiDataframe %>%
    arrange(desc(Animal_ID_colour)) %>%
    mutate(Animal_ID_colour = "<i>Individuals</i>") %>%
    filter(!is.na(est),
           variableType == "Other",
           !variableType == "Step",
           !variable == "cos ta_ ") %>%
    filter(!is.na(est)) %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25, colour = "grey25") +
    geom_hline(yintercept = 1.5, linetype = "solid", linewidth = 0.25, colour = "grey25") +
    geom_hline(yintercept = 0.5, linetype = "solid", linewidth = 0.25, colour = "grey25") +
    geom_segment(data = popAvg %>%
                   filter(variableType == "Other",
                          !variableType == "Step",
                          !variable == "cos ta_ "),
                 aes(x = LCI, xend = UCI, y = Animal_ID_colour, yend = Animal_ID_colour, colour = sig),
                 linewidth = 1.2) +
    geom_point(data = popAvg %>%
                 filter(variableType == "Other",
                        !variableType == "Step",
                        !variable == "cos ta_ "),
               aes(x = Mean, y = Animal_ID_colour, colour = sig),
               size = 2) +
    geom_text(data = popAvg %>%
                filter(variableType == "Other",
                       !variableType == "Step",
                       !variable == "cos ta_ "),
              aes(x = Inf, y = Animal_ID_colour, colour = sig, label = signif(Mean, digits = 2)),
              hjust = 1.1, size = 3) +
    geom_point(aes(x = est, y = Animal_ID_colour, colour = sig), size = 0.65,
               position = position_jitter(width = 0, height = 0.25)) +
    facet_grid(rows = vars(variable), scales = "free", space = "free", drop = TRUE,
               switch = "y") +
    scale_colour_manual(values = paletteList$highSigLowSigNoSig) +
    labs(x = "Selection Strength Coefficient", y = "") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.title = element_text(face = 2),
      axis.title = element_text(face = 2),
      axis.ticks = element_blank(),
      axis.text.y = element_markdown(size = 8),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1, colour = "grey25")
    ) +
    guides(colour = guide_legend(override.aes = list(linewidth = 0)))

  # ggsave(
  #   plot = coefLanduse,
  #   filename = here::here("figures", "ssfCoef_landuse.png"),
  #   width = 210, height = 200, units = "mm", dpi = 300,
  #   device = ragg::agg_png
  # )
  # ggsave(
  #   plot = coefDistance,
  #   filename = here::here("figures", "ssfCoef_distance.png"),
  #   width = 210, height = 120, units = "mm", dpi = 300,
  #   device = ragg::agg_png
  # )
  # ggsave(
  #   plot = coefOther,
  #   filename = here::here("figures", "ssfCoef_other.png"),
  #   width = 210, height = 65, units = "mm", dpi = 300,
  #   device = ragg::agg_png
  # )

  coefCombo <- (coefDistance +
    theme(axis.title.x.bottom = element_blank())) /
  (coefOther +
    theme(axis.title.x.bottom = element_blank())) /
  coefLanduse +
    plot_layout(ncol = 1, heights = c(2,1,9))

  ggsave(
    plot = coefCombo,
    filename = here::here("figures", "ssfCoef_indPop.png"),
    width = 210, height = 200, units = "mm", dpi = 300,
    device = ragg::agg_png
  )

  ggsave(
    plot = coefCombo,
    filename = here::here("figures", "ssfCoef_indPop.pdf"),
    width = 210, height = 200, units = "mm"
  )

  simuSDmeans <- simuSDmeans %>%
    mutate(variableType = case_when(
      str_detect(variable, "Distance") ~ "Distance",
      str_detect(variable, "Road") ~ "Other",
      variable %in% c("sl_", "cos ta_ ", "log sl_ ") ~ "Core movement",
      str_detect(variable, "\\:sl_") ~ "Step interactions (sl_)",
      str_detect(variable, "\\:log.sl_.") ~ "Step interactions (log_sl)",
      TRUE ~ "Landuse"
    )) %>%
    filter(!variableType == "Core movement")

  heteroPlot <- simuSD %>%
    filter(!variableType == "Core movement") %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25, colour = "grey25") +
    geom_boxplot(aes(x = value, y = variable),
                 fill = "grey95", colour = "grey25",
                 width = 0.2, position = position_nudge(y = 0),
                 outliers = FALSE) +
    geom_text(data = simuSDmeans,
              aes(x = -Inf, y = variable, label = signif(meanSD, digits = 4)),
              colour = "grey25", hjust = 0, fontface = 2) +
    # geom_density_ridges(aes(x = value, y = variable), alpha = 0.5) +
    facet_wrap(vars(variableType), ncol = 1, drop = TRUE, scales = "free",
               strip.position = "left") +
    labs(x = "Heterogeneity\n(individual variation in response)", y = "", title = "Heterogeneity") +
    scale_x_continuous(limits = c(0, NA), expand = c(0.075,0)) +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.title = element_text(face = 2),
      axis.title = element_text(face = 2),
      axis.ticks.y = element_blank(),
      axis.text.y = element_markdown(),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1, colour = "grey25")
    )

  ggsave(
    plot = heteroPlot,
    filename = here::here("figures", "ssfCoef_Heterogeneity.png"),
    width = 320, height = 160, units = "mm", dpi = 300,
    device = ragg::agg_png
  )


  simuSPEmeans <- simuSPEmeans %>%
    mutate(variableType = case_when(
      str_detect(variable, "Distance") ~ "Distance",
      str_detect(variable, "Road") ~ "Other",
      variable %in% c("sl_", "cos ta_ ", "log sl_ ") ~ "Core movement",
      str_detect(variable, "\\:sl_") ~ "Step interactions (sl_)",
      str_detect(variable, "\\:log.sl_.") ~ "Step interactions (log_sl)",
      TRUE ~ "Landuse"
    )) %>%
    filter(!variableType == "Core movement")

  specialPlot <- simuSPE %>%
    filter(!variableType == "Core movement") %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25, colour = "grey25") +
    geom_boxplot(aes(x = value, y = variable),
                 fill = "grey95", colour = "grey25",
                 width = 0.2, position = position_nudge(y = 0),
                 outliers = FALSE) +
    geom_text(data = simuSPEmeans,
              aes(x = -Inf, y = variable, label = signif(meanSPE, digits = 4)),
              colour = "grey25", hjust = 0, fontface = 2) +
    # geom_density_ridges(aes(x = value, y = variable), alpha = 0.5) +
    facet_wrap(vars(variableType), ncol = 1, drop = TRUE, scales = "free",
               strip.position = "left") +
    labs(x = "Specialisation\n(magnitude of the response independent of the direction)", y = "", title = "Specialisation") +
    scale_x_continuous(limits = c(0, NA), expand = c(0.075,0)) +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.title = element_text(face = 2),
      axis.title = element_text(face = 2),
      axis.ticks.y = element_blank(),
      axis.text.y = element_markdown(),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1, colour = "grey25")
    )

  ggsave(
    plot = specialPlot,
    filename = here::here("figures", "ssfCoef_Specialisation.png"),
    width = 320, height = 160, units = "mm", dpi = 300,
    device = ragg::agg_png
  )

  plotList <- list(
    coefLanduse = coefLanduse,
    coefDistance = coefDistance,
    coefOther = coefOther,
    heteroPlot = heteroPlot,
    specialPlot = specialPlot
  )

  return(plotList)

}
