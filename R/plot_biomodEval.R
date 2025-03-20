#' Plots various plots from biomod models
#'
#' @name plot_biomodEval_
#' @description abc
#' @return abc
#'
#' @export
plot_biomodEval_single <- function(biomodModels){

  # library(here)
  # library(dplyr)
  # library(ggplot2)
  # library(biomod2)
  # library(terra)
  # library(tidyterra)
  # library(stringr)
  # library(ggrepel)
  # targets::tar_load("tar_biomodModels_fallow")
  # biomodModels_fallow <- tar_biomodModels_fallow
  get_evaluations(biomodModels)

  if(biomodModels@sp.name == "Dama.dama"){
    species <- "fallow"
  } else {
    species <- "rodent"
  }

  paletteList <- load_deer_palette()

  ggplotThemeCombo <-
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

  evalModels <- bm_PlotEvalMean(bm.out = biomodModels)

  evalPlot <- evalModels$tab %>%
    ggplot() +
    geom_errorbar(aes(x = mean1, ymin = mean2-sd2, ymax = mean2+sd2, colour = name),
                  width = 0.001)+
    geom_errorbarh(aes(y = mean2, xmin = mean1-sd1, xmax = mean1+sd1, colour = name),
                   height = 0.001)+
    geom_point(aes(x = mean1, y = mean2, colour = name), size = 3) +
    labs(y = "TSS", x = "ROC", colour = "Model") +
    ggplotThemeCombo +
    scale_colour_manual(values = c("#DCBD0A",
                                   "#CD602A",
                                   "#9F7E93",
                                   "#85AB7A"))

  ggsave(plot = evalPlot, filename = here("figures", paste0(species, "_evalModels.png")),
         width = 180, height = 140, units = "mm")

  varImportByModel <- bm_PlotVarImpBoxplot(bm.out = biomodModels, group.by = c('expl.var', 'algo', 'algo'))

  varImportPlot <- varImportByModel$tab %>%
    mutate(expl.var = reorder(expl.var, var.imp, FUN = max)) %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.85) +
    geom_boxplot(aes(y = expl.var, x = var.imp, colour = algo, fill = algo)) +
    facet_grid(cols = vars(algo)) +
    ggplotThemeCombo +
    theme(legend.position = "none") +
    scale_colour_manual(values = c("#DCBD0A",
                                   "#CD602A",
                                   "#9F7E93",
                                   "#85AB7A")) +
    scale_fill_manual(values = c("#DCBD0A",
                                 "#CD602A",
                                 "#9F7E93",
                                 "#85AB7A"))

  ggsave(plot = varImportPlot, filename = here("figures", paste0(species, "_varImportanceModels.png")),
         width = 240, height = 120, units = "mm")

  return(list(evalPlot = evalPlot, varImportPlot = varImportPlot))
}

#' @export
plot_biomodEval_ensemble <- function(biomodEns, patchList){

  if(biomodEns@sp.name == "Dama.dama"){
    species <- "fallow"
  } else {
    species <- "rodent"
  }

  paletteList <- load_deer_palette()

  ggplotThemeCombo <-
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

  # targets::tar_source()
  # targets::tar_load("tar_biomodEns_rodent")
  # biomodEns <- tar_biomodEns_rodent

  # useful ------------------------------------------------------------------
  # targets::tar_load("tar_patchList")
  wessexStack <- read_stack_layers(layerLoc = here("data", "GIS data", "SDM Layers")) %>%
    crop(patchList$Wessex)

  wessexRanges <- as.data.frame(t(data.frame(
    range(values(wessexStack$distanceWoodland), na.rm = TRUE),
    range(values(wessexStack$distanceHumanSettlement), na.rm = TRUE),
    range(values(wessexStack$distanceHedgerow), na.rm = TRUE)
  )))

  wessexRanges$expl.name <- c("distanceWoodland", "distanceHumanSettlement", "distanceHedgerow")
  names(wessexRanges) <- c("min", "max", "expl.name")

  evalEns <- bm_PlotEvalMean(bm.out = biomodEns, group.by = 'full.name')

  evalEnsPlot <- evalEns$tab %>%
    mutate(name = str_extract_all(name, "EM.*?ByTSS", simplify = TRUE)) %>%
    ggplot() +
    geom_point(aes(x = mean1, y = mean2, colour = name, fill = name),
               size = 4, pch = 21) +
    geom_text_repel(aes(x = mean1, y = mean2, colour = name, label = name),
                    hjust = 0, box.padding = unit(1.2, "lines")) +
    labs(y = "TSS", x = "ROC") +
    ggplotThemeCombo +
    theme(legend.position = "none") +
    scale_colour_manual(values = c("#DCBD0A",
                                   "#CD602A",
                                   "#9F7E93",
                                   "#85AB7A")) +
    scale_fill_manual(values = c("#DCBD0A",
                                 "#CD602A",
                                 "#9F7E93",
                                 "#85AB7A"))

  ggsave(plot = evalEnsPlot, filename = here("figures", paste0(species, "_evalEnsemble.png")),
         width = 180, height = 140, units = "mm")

  varImport <- bm_PlotVarImpBoxplot(bm.out = biomodEns,
                                    group.by = c('expl.var', 'algo', 'merged.by.run'))

  varImportPlot <- varImport$tab %>%
    filter(algo %in% c("EMwmean", "EMmean", "EMmedian", "EMca")) %>%
    mutate(expl.var = reorder(expl.var, var.imp, FUN = max)) %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.85) +
    geom_hline(yintercept = seq(1.5,11.5,1), linetype = "dashed", alpha = 0.25) +
    geom_boxplot(aes(y = expl.var, x = var.imp, colour = algo, fill = algo)) +
    # scale_colour_manual(values = c(
    #   "EMca" = "grey25",
    #   "EMciSup" = "grey25",
    #   "EMmean" = "grey25",
    #   "EMwmean" = "#85AB7A",
    #   "EMciInf" = "grey25",
    #   "EMcv" = "grey25",
    #   "EMmedian" = "grey25"
    # )) +
    # scale_fill_manual(values = c(
    #   "EMca" = "grey25",
    #   "EMciSup" = "grey25",
    #   "EMmean" = "grey25",
    #   "EMwmean" = "#85AB7A",
    #   "EMciInf" = "grey25",
    #   "EMcv" = "grey25",
    #   "EMmedian" = "grey25"
    # )) +
    # +
    scale_colour_manual(values = c("#DCBD0A",
                                   "#CD602A",
                                   "#9F7E93",
                                   "#85AB7A")) +
    scale_fill_manual(values = c("#DCBD0A",
                                 "#CD602A",
                                 "#9F7E93",
                                 "#85AB7A")) +
    ggplotThemeCombo +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom")

  ggsave(plot = varImportPlot, filename = here("figures", paste0(species, "_varImportanceEnsemble.png")),
         width = 200, height = 120, units = "mm")

  respCurve <- bm_PlotResponseCurves(bm.out = biomodEns,
                                     models.chosen = get_built_models(biomodEns)[
                                       stringr::str_detect(get_built_models(biomodEns),
                                                           "EMwmeanByTSS")],
                                     fixed.var = 'mean')

  respDistPlot <- respCurve$tab %>%
    filter(str_detect(expl.name, "distance")) %>%
    ggplot() +
    geom_rect(data = wessexRanges, aes(xmin = min, xmax = max,
                                       ymin = -Inf, ymax = Inf), fill = "#85AB7A", alpha = 0.2) +
    geom_path(aes(x = expl.val, y = pred.val, group = expl.name)) +
    facet_wrap(facet = vars(expl.name), scales = "free") +
    ggplotThemeCombo

  ggsave(plot = respDistPlot, filename = here("figures", paste0(species, "_responseDistanceEnsemble.png")),
         width = 200, height = 120, units = "mm")

  respBinaryPlot <- respCurve$tab %>%
    filter(!str_detect(expl.name, "distance")) %>%
    ggplot() +
    geom_vline(xintercept = c(1,2), linetype = "dashed", alpha = 0.85) +
    geom_path(aes(x = expl.val, y = pred.val, group = expl.name)) +
    facet_wrap(facet = vars(expl.name)) +
    scale_x_continuous(breaks = c(1, 2),
                       labels = c("0", "1")) +
    ggplotThemeCombo

  ggsave(plot = respBinaryPlot, filename = here("figures", paste0(species, "_responseBinaryEnsemble.png")),
         width = 240, height = 120, units = "mm")


  return(
    list(
      evalEnsPlot = evalEnsPlot,
      varImportPlot = varImportPlot,
      respDistPlot = respDistPlot,
      respBinaryPlot = respBinaryPlot
    )
  )

}
