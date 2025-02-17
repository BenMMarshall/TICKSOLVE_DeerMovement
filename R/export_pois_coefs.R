#' Create a plot of poisson models
#'
#' @name export_pois_coefs
#' @description abc
#' @return abc
#'
#' @export
export_pois_coefs <- function(poisModel){

  # targets::tar_load("tar_pois_model")
  # poisModel <- tar_pois_model
  # poisModel <- inlaOUT

  paletteList <- load_deer_palette()
  sigColourDF <- data.frame(colour = paletteList$highSigLowSigNoSig,
                            sig = names(paletteList$highSigLowSigNoSig))

  poisFixed <- poisModel$summary.fixed %>%
    as.data.frame()
  poisFixed$term <- rownames(poisFixed)

  poisFixed <- poisFixed %>%
    mutate(termType = case_when(
      term %in% c("roadCrossings", "distanceWoodland", "distanceHedges") ~ "Non-landuse",
      term %in% c("sl_", "cos_ta", "log_sl") ~ "Core movement",
      str_detect(term, "\\:sl") ~ "Step interactions (sl_)",
      str_detect(term, "\\:log_sl") ~ "Step interactions (log_sl)",
      TRUE ~ "Landuse"
    )) %>%
    rename(lower = `0.025quant`, upper = `0.975quant`) %>%
    mutate(sig = ifelse(lower > 0, "Significant +", ifelse(upper < 0, "Significant -", "Not Significant"))) %>%
    left_join(sigColourDF, by = "sig") %>%
    mutate(term = case_when(
      str_detect(term, "\\.") ~ str_replace_all(term, "\\.", " "),
      str_detect(term, "distance") ~ str_replace_all(term, "distance", "Distance to "),
      term == "roadCrossings" ~ "Road Crossing",
      TRUE ~ term
    )) %>%
    mutate(term_colour = glue::glue("<i style='color:{colour}'>{term}</i>"),
           term_colour = ifelse(sig == "Not Significant", term_colour,
                                glue::glue("<b>{term_colour}</b>"))) %>%
    arrange(termType, mean) %>%
    mutate(term_colour = factor(term_colour, levels = unique(term_colour)),
           termType = factor(termType, levels = c(
             "Core movement",
             "Non-landuse",
             "Landuse",
             "Step interactions (sl_)",
             "Step interactions (log_sl)"
           )))

  write.csv(poisFixed, file = here::here("tables", "poisFixed.csv"), row.names = FALSE)

  minY <- -1.5
  maxY <- 2

  outliers <- poisFixed %>%
    filter(mean < minY | mean > maxY) %>%
    mutate(term_colour = glue::glue("<i style='color:{colour}'>{term}</i>"),
           term_colour = ifelse(sig == "Not Significant", term_colour,
                                glue::glue("<b>{term_colour}</b>")))

  (poisCoef_plot <- poisFixed %>%
      filter(!termType == "Core movement") %>%
      ggplot() +
      geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
      geom_errorbarh(aes(xmin = lower, xmax = upper, y = term_colour, color = sig), height = 0.2,
                     alpha = 0.35) +
      geom_point(aes(x = mean, y = term_colour, color = sig)) +
      geom_richtext(data = outliers,
                aes(x = 0, y = term_colour, label = round(mean, digits = 2)),
                vjust = 0.5, size = 3, colour = "grey25") +
      scale_colour_manual(values = paletteList$highSigLowSigNoSig) +
      facet_wrap(vars(termType), ncol = 1, drop = TRUE, scales = "free_y",
                 strip.position = "left") +
      coord_cartesian(xlim = c(minY, maxY)) +
      labs(x = "Coefficient", y = "Term", title = "Poisson Model Coefficients") +
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
        strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
      ))

  ggsave(plot = poisCoef_plot, filename = here::here("figures", paste0("poisCoef.png")),
         width = 210, height = 190, units = "mm", dpi = 300)
  ggsave(plot = poisCoef_plot, filename = here::here("figures", paste0("poisCoef.pdf")),
         width = 210, height = 190, units = "mm")

  return(poisCoef_plot)

}
