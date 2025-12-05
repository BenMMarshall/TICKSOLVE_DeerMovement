#' Generate tracking summary plots
#'
#' @name generate_tracking_plots
#' @description Take all deer data and generate plots of tracking duration and timelag.
#' @return A two object list, one object for duration plot one for timelag.
#'
#' @export
generate_tracking_plots <- function(deerData){

  paletteList <- load_deer_palette()

  durationPlot <- deerData %>%
    filter(str_detect(Animal_ID, "Roe")) %>%
    mutate(Animal_ID = factor(Animal_ID,
                              levels = c(
                                sort(unique(deerData$Animal_ID),
                                     decreasing = TRUE)
                              ))) %>%
    ggplot() +
    geom_point(aes(x = datetime, y = Animal_ID, colour = Sex)) +
    scale_x_datetime(breaks = "month",
                     date_labels = "%b %y") +
    scale_colour_manual(values = paletteList$deerSexPal) +
    facet_grid(rows = vars(region), scales = "free_y", space = "free_y", switch = "y",
               axes = "all_y") +
    labs(y = "", x = "Month Year") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      axis.title = element_text(face = 2),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "none",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1),
      strip.placement = "outside"
    )

  timelagPlot <- deerData %>%
    filter(str_detect(Animal_ID, "Roe")) %>%
    mutate(Animal_ID = factor(Animal_ID,
                              levels = c(
                                sort(unique(deerData$Animal_ID),
                                     decreasing = TRUE)
                              ))) %>%
    ggplot() +
    geom_density_ridges(aes(x = timelag/60/60, y = Animal_ID, fill = Sex, colour = Sex)) +
    scale_x_log10(limits = range(deerData$timelag/60/60, na.rm = TRUE)) +
    scale_colour_manual(values = scales::muted(paletteList$deerSexPal)) +
    scale_fill_manual(values = paletteList$deerSexPal) +
    facet_grid(rows = vars(region), scales = "free_y", space = "free_y", switch = "y",
               axes = "all_y") +
    labs(y = "", x = "Timelag (hours)") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      axis.title = element_text(face = 2),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1),
      legend.position = "none"
    )

  ggsave(durationPlot, filename = here("figures", "trackingDuration.png"),
         width = 210, height = 120, units = "mm", dpi = 300)
  ggsave(timelagPlot, filename = here("figures", "trackingTimelag.png"),
         width = 180, height = 120, units = "mm", dpi = 300)
  ggsave(durationPlot, filename = here("figures", "trackingDuration.pdf"),
         width = 210, height = 120, units = "mm")
  ggsave(timelagPlot, filename = here("figures", "trackingTimelag.pdf"),
         width = 180, height = 120, units = "mm")

  return(list(
    "durationPlot" = durationPlot,
    "timelagPlot" = timelagPlot
  ))

}
