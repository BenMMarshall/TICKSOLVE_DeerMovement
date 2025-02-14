#' Plot variograms
#'
#' @name plot_variograms
#' @description abc
#' @return abc
#'
#' @export
plot_variograms <- function(akdeLists){

  # targets::tar_load("tar_akdeLists")
  # akdeLists <- tar_akdeLists
  # library(ggplot2)

  paletteList <- load_deer_palette()

  varList <- lapply(names(akdeLists$vario), function(x){
    CI.upper <- Vectorize(function(k,Alpha){stats::qchisq(Alpha/2,k,lower.tail=FALSE)/k})
    CI.lower <- Vectorize(function(k,Alpha){stats::qchisq(Alpha/2,k,lower.tail=TRUE)/k})

    varioData <- data.frame("lag" = akdeLists$vario[[x]]$lag,
                            "SVF" = akdeLists$vario[[x]]$SVF,
                            "DOF" = akdeLists$vario[[x]]$DOF,
                            "Animal_ID" = x)

    varioData$SVFLow95 <- akdeLists$vario[[x]]$SVF * CI.lower(akdeLists$vario[[x]]$DOF, 0.05)
    varioData$SVFUpp95 <- akdeLists$vario[[x]]$SVF * CI.upper(akdeLists$vario[[x]]$DOF, 0.05)
    varioData$SVFLow50 <- akdeLists$vario[[x]]$SVF * CI.lower(akdeLists$vario[[x]]$DOF, 0.5)
    varioData$SVFUpp50 <- akdeLists$vario[[x]]$SVF * CI.upper(akdeLists$vario[[x]]$DOF, 0.5)

    return(varioData)
  })
  varioData <- do.call(rbind, varList)

  varioPlot <- varioData %>%
    mutate(Sex = str_extract(Animal_ID, "M|F$")) %>%
    filter(str_detect(Animal_ID, "Roe")) %>%
    group_by(Animal_ID) %>%
    mutate(halfLag = max(lag)/2) %>%
    filter(lag < halfLag) %>%
    # filter(Animal_ID == "Roe10_F") %>%
    mutate(lag = lag /60/60/24/30.5,
           SVF = SVF /10000) %>%
    ggplot() +
    geom_ribbon(aes(x = lag, ymin = SVFLow95/10000, ymax = SVFUpp95/10000, fill = Sex),
                alpha = 0.25) +
    geom_ribbon(aes(x = lag, ymin = SVFLow50/10000, ymax = SVFUpp50/10000, fill = Sex),
                alpha = 0.25) +
    geom_path(aes(x = lag, y = SVF, color = Sex), linewidth = 0.5) +
    scale_fill_manual(values = paletteList$deerSexPal) +
    scale_colour_manual(values = paletteList$deerSexPal) +
    facet_wrap(facets = vars(Animal_ID),
               scales = "free") +
    labs(x = "Lag (months)",
         y = "Semi-variance (ha)") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      axis.line = element_line(colour = "grey25"),
      plot.title = element_text(face = 2),
      axis.title = element_text(face = 2),
      # axis.ticks.y = element_blank(),
      axis.text.y = element_markdown(colour = "grey25"),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(angle = 0, face = 4, hjust = 0, vjust = 1)
    )

  varioPlot

  ggsave(plot = varioPlot, filename = here::here("figures", paste0("varioRoePlot.png")),
         width = 240, height = 190, units = "mm", dpi = 300)
  ggsave(plot = varioPlot, filename = here::here("figures", paste0("varioRoePlot.pdf")),
         width = 240, height = 190, units = "mm")

  return(varioPlot)

}
