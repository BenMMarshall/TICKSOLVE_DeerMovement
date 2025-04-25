#' Plot wrsf effects
#'
#' @name plot_wrsf_effects
#' @description abc
#' @return abc
#'
#' @export
plot_wrsf_effects <- function(wrsfakde){

  paletteList <- load_deer_palette()
  # targets::tar_load("tar_wrsfakde")
  #
  # wrsfakde <- tar_wrsfakde

  meanWrsf <- mean(wrsfakde$wrsfModel)
  meanWrsf <- as.data.frame(summary(meanWrsf)$CI)
  meanWrsf <- meanWrsf[str_detect(rownames(meanWrsf), "1/"),]
  meanWrsf$variable <- rownames(meanWrsf)
  meanWrsf <- meanWrsf %>%
    mutate(sig = ifelse(low > 0 | high < 0, "sig", "nonsig"),
           variable = case_when(
             variable == "Open_Shrubland (1/Open_Shrubland)" ~ "Open Shrubland",
             variable == "Tall_Grassland (1/Tall_Grassland)" ~ "Tall Grassland",
             variable == "distanceWoodland (1/distanceWoodland)" ~ "Distance from Woodland"
           ))

  wrsfCoef <- do.call(rbind, lapply(names(wrsfakde$wrsfModel), function(x){
    # x <- wrsfakde$wrsfModel[[1]]
    y <- summary(wrsfakde$wrsfModel[[x]])$CI
    wrsfSummary <- as.data.frame(y)
    wrsfSummary <- wrsfSummary[str_detect(rownames(wrsfSummary), "1/"),]
    wrsfSummary$variable <- rownames(wrsfSummary)
    wrsfSummary$Animal_ID <- x
    return(wrsfSummary)
  }))

  ESSDF <- do.call(rbind, lapply(names(wrsfakde$wrsfModel), function(x){
    data.frame(
      Animal_ID = x,
      DOF_area = summary(wrsfakde$wrsfModel[[x]])$DOF["area"])
  }))

  wrsfCoefOuputTable <- wrsfCoef %>%
    left_join(ESSDF)

  write.csv(wrsfCoefOuputTable,
            file = here::here("tables", "wrsfSummary.csv"), row.names = FALSE)

  wrsfPlot <- wrsfCoefOuputTable %>%
    mutate(sig = ifelse(low > 0 | high < 0, "sig", "nonsig"),
           variable = case_when(
             variable == "Open_Shrubland (1/Open_Shrubland)" ~ "Open Shrubland",
             variable == "Tall_Grassland (1/Tall_Grassland)" ~ "Tall Grassland",
             variable == "distanceWoodland (1/distanceWoodland)" ~ "Distance from Woodland"
           )) %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey") +
    geom_vline(data = meanWrsf, aes(xintercept = est, colour = sig), alpha = 0.5, linewidth = 0.95) +
    geom_vline(data = meanWrsf, aes(xintercept = low, colour = sig), alpha = 0.25, linewidth = 0.95) +
    geom_vline(data = meanWrsf, aes(xintercept = high, colour = sig), alpha = 0.25, linewidth = 0.95) +
    # geom_errorbar(aes(x = est, y = Animal_ID, xmin = low, xmax = high)) +
    geom_point(aes(x = est, y = Animal_ID, colour = sig)) +
    scale_colour_manual(values = c("#303030", "#B54D17")) +
    facet_wrap(facets = vars(variable), ncol = 1, scale = "free") +
    labs(x = "Coeficient", y = "") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      axis.title = element_text(face = 2),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      legend.position = "none",
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(angle = 0, size = 10, face = 4, hjust = 0, vjust = 1)
    )

  ggsave(wrsfPlot, filename = here("figures", "wrsfEffects.png"),
         width = 180, height = 180, units = "mm", dpi = 300)
  ggsave(wrsfPlot, filename = here("figures", "wrsfEffects.pdf"),
         width = 180, height = 180, units = "mm")

  return(wrsfPlot)

}
