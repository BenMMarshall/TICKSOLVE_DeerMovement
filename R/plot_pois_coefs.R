#' Create a plot of poisson models
#'
#' @name plot_pois_coefs
#' @description abc
#' @return abc
#'
#' @export
plot_pois_coefs <- function(poisModel){

  # targets::tar_load("tar_pois_model")
  # poisModel <- tar_pois_model

  poisFixed <- poisModel$summary.fixed %>%
    as.data.frame()
  poisFixed$term <- rownames(poisFixed)

  paletteList <- load_deer_palette()
  sigColourDF <- data.frame(colour = paletteList$highSigLowSigNoSig,
                            sig = names(paletteList$highSigLowSigNoSig))

  poisFixed <- poisFixed %>%
    mutate(termType = case_when(
      term %in% c("roadCrossings", "distanceWoodland") ~ "Non-landuse",
      TRUE ~ "Landuse"
    )) %>%
    rename(lower = `0.025quant`, upper = `0.975quant`) %>%
    mutate(sig = ifelse(lower > 0, "Significant +", ifelse(upper < 0, "Significant -", "Not Significant"))) %>%
    left_join(sigColourDF, by = "sig") %>%
    mutate(term_colour = glue::glue("<i style='color:{colour}'>{term}</i>"),
           term_colour = ifelse(sig == "Not Significant", term_colour,
                                glue::glue("<b>{term_colour}</b>"))) %>%
    arrange(termType, mean) %>%
    mutate(term_colour = factor(term_colour, levels = unique(term_colour)))

  (poisCoef_plot <- poisFixed %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
    geom_errorbarh(aes(xmin = lower, xmax = upper, y = term_colour, color = sig), height = 0.2,
                   alpha = 0.35) +
      geom_point(aes(x = mean, y = term_colour, color = sig)) +
      scale_colour_manual(values = paletteList$highSigLowSigNoSig) +
      facet_wrap(vars(termType), ncol = 1, drop = TRUE, scales = "free_y",
                 strip.position = "left") +
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

  ggsave(plot = poisCoef_plot, filename = here::here("modelOutput", paste0("poisCoef.png")),
         width = 160, height = 100, units = "mm", dpi = 300)
  ggsave(plot = poisCoef_plot, filename = here::here("modelOutput", paste0("poisCoef.pdf")),
         width = 160, height = 100, units = "mm")

  return(poisCoef_plot)

}
