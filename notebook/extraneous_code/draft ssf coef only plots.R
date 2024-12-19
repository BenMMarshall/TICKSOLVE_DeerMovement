
library(amt)
library(dplyr)

targets::tar_load("tar_ssf_models")
targets::tar_load("tar_deerData")

targets::tar_source()

paletteList <- load_deer_palette()

deerData <- tar_deerData

aberDeer <- deerData %>%
  filter(region == "Aberdeenshire") %>%
  pull(Animal_ID) %>% unique()

ssfModels <- tar_ssf_models
ssfModels <- ssfModels[aberDeer]
summary(ssfModels$Roe06_F$model)
confint(ssfModels$Roe06_F$model)

ssfCoefs <- do.call(rbind, lapply(names(ssfModels), function(x){
  mod <- ssfModels[[x]]
  coef <- coef(mod$model)
  conf <- confint(mod$model)
  modelSummary <- cbind(coef, conf) %>%
    as.data.frame()
  modelSummary$Animal_ID <- x
  return(modelSummary)
  }))
ssfCoefs$term <- gsub("[[:digit:]]+$", "", row.names(ssfCoefs))

naiveMeanSsfCoefs <- ssfCoefs %>%
  group_by(term) %>%
  summarise(meanEffect = mean(coef, na.rm = TRUE),
            medianEffect = median(coef, na.rm = TRUE))

# return(naiveMeanSsfCoefs)

library(ggplot2)
library(tidyr)

ssfCoefs_long <- ssfCoefs %>%
  rename(lower = `2.5 %`, upper = `97.5 %`) %>%
  # pivot_longer(cols = !contains("Animal_ID"), names_to = "term", values_to = "coef") %>%
  filter(!term %in% c("landuseOther",
                      "distanceWoodland:landuseHuman Settlements",
                      "distanceWoodland:landuseOther",
                      "landuseOther:log(sl_)")) %>%
  mutate(termType = case_when(
    grepl("sl_", term) ~ "Step",
    grepl("distance", term) ~ "Distance",
    grepl("landuse", term) ~ "Landuse",
    TRUE ~ "Other"
  )) %>%
  mutate(sig = ifelse(lower > 0, "Significant +", ifelse(upper < 0, "Significant -", "Not Significant")))

sigColourDF <- data.frame(colour = paletteList$highSigLowSigNoSig,
           sig = names(paletteList$highSigLowSigNoSig))

ssfCoefs_long <- ssfCoefs_long %>%
  left_join(sigColourDF, by = "sig") %>%
  mutate(Animal_ID_colour = glue::glue("<i style='color:{colour}'>{Animal_ID}</i>"),
         Animal_ID_colour = ifelse(sig == "Not Significant", Animal_ID_colour,
                                   glue::glue("<b>{Animal_ID_colour}</b>")))

ssfCoef_plotList <- vector("list", length = length(unique(ssfCoefs_long$term)))
names(ssfCoef_plotList) <- unique(ssfCoefs_long$term)
for(t in unique(ssfCoefs_long$term)){
  # t <- unique(ssfCoefs_long$term)[1]
  ssfCoefs_curr <- ssfCoefs_long %>%
    filter(term == t) %>%
    arrange(term, coef) %>%
    mutate(Animal_ID_colour = factor(Animal_ID_colour, levels = unique(Animal_ID_colour)))

  outQuant <- ssfCoefs_curr %>%
    pull(coef) %>% quantile(probs = c(0.05, 0.95), na.rm = TRUE)

  outlierLabs <- ssfCoefs_curr %>%
    filter(coef < outQuant[1]) %>%
    mutate(xloc = -Inf, hjust = 0,
           text = paste0("<", round(coef, digits = 1))) %>%
    rbind(ssfCoefs_curr %>%
            filter(coef > outQuant[2]) %>%
            mutate(xloc = Inf, hjust = 1) %>%
            mutate(text = paste0(round(coef, digits = 1), ">")))

  ssfCoef_plot <- ssfCoefs_curr %>%
    filter(coef > outQuant[1] & coef < outQuant[2]) %>%
    # ssfCoefs_long %>%
    #   filter(termType == "Distance") %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
    geom_text(data = outlierLabs,
              aes(x = xloc, y = Animal_ID_colour, color = sig, label = text, hjust = hjust),
              fontface = 2) +
    geom_errorbarh(aes(xmin = lower, xmax = upper, y = Animal_ID_colour, color = sig), height = 0.2,
                   alpha = 0.35) +
    geom_point(aes(x = coef, y = Animal_ID_colour, color = sig)) +
    coord_cartesian(xlim = ssfCoefs_curr %>%
                      filter(coef > outQuant[1] & coef < outQuant[2]) %>%
                      pull(coef) %>% range(na.rm = TRUE)) +
    scale_colour_manual(values = paletteList$highSigLowSigNoSig) +
    facet_wrap(vars(term), ncol = 1, drop = TRUE, scales = "free_y",
               strip.position = "left") +
    labs(x = "Coefficient", y = "Animal ID") +
    theme_bw() +
    theme(
      text = element_text(colour = "grey25"),
      line = element_line(colour = "grey25"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      axis.title = element_text(face = 2),
      axis.ticks.y = element_blank(),
      axis.text.y = element_markdown(),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
    )

  ssfCoef_plotList[[t]] <- ssfCoef_plot

  ggsave(filename = here::here("modelOutput", paste0("ssfCoef_", sub(":", "__", t), ".png")),
         width = 210, height = 160, units = "mm", dpi = 300)

}


targets::tar_load("tar_pois_model")
poisModel <- tar_pois_model

poisFixed <- poisModel$summary.fixed %>%
  as.data.frame()
poisFixed$term <- rownames(poisFixed)

paletteList <- load_deer_palette()
sigColourDF <- data.frame(colour = paletteList$highSigLowSigNoSig,
                          sig = names(paletteList$highSigLowSigNoSig))

poisFixed <- poisFixed %>%
  rename(lower = `0.025quant`, upper = `0.975quant`) %>%
  mutate(sig = ifelse(lower > 0, "Significant +", ifelse(upper < 0, "Significant -", "Not Significant"))) %>%
  left_join(sigColourDF, by = "sig") %>%
  mutate(term_colour = glue::glue("<i style='color:{colour}'>{term}</i>"),
         term_colour = ifelse(sig == "Not Significant", term_colour,
                              glue::glue("<b>{term_colour}</b>")))

poisFixed %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_errorbarh(aes(xmin = lower, xmax = upper, y = term_colour, color = sig), height = 0.2,
                 alpha = 0.35) +
  geom_point(aes(x = mean, y = term_colour, color = sig)) +
  # coord_cartesian(xlim = ssfCoefs_curr %>%
  #                   filter(coef > outQuant[1] & coef < outQuant[2]) %>%
  #                   pull(coef) %>% range(na.rm = TRUE)) +
  scale_colour_manual(values = paletteList$highSigLowSigNoSig) +
  # facet_wrap(vars(term), ncol = 1, drop = TRUE, scales = "free_y",
  #            strip.position = "left") +
  labs(x = "Coefficient", y = "Term", title = "Poisson Model Coefficients") +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title = element_text(face = 2),
    axis.ticks.y = element_blank(),
    axis.text.y = element_markdown(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
  )

# Unused below ------------------------------------------------------------



# Distance woodland -------------------------------------------------------


ssfCoefs_dist <- ssfCoefs_long %>%
  filter(termType == "Distance") %>%
  left_join(sigColourDF, by = "sig") %>%
  mutate(Animal_ID_colour = glue::glue("<i style='color:{colour}'>{Animal_ID}</i>")) %>%
  arrange(term, coef) %>%
  mutate(Animal_ID_colour = factor(Animal_ID_colour, levels = unique(Animal_ID_colour)))

distQuant <- ssfCoefs_dist %>%
  pull(coef) %>% quantile(probs = c(0.05, 0.95), na.rm = TRUE)

outlierDistLabs <- ssfCoefs_dist %>%
  filter(coef < distQuant[1]) %>%
  mutate(xloc = -Inf, hjust = 0,
         text = paste0("<", round(coef, digits = 1))) %>%
  rbind(ssfCoefs_dist %>%
          filter(coef > distQuant[2]) %>%
          mutate(xloc = Inf, hjust = 1) %>%
          mutate(text = paste0(round(coef, digits = 1), ">")))

ssfCoefs_dist %>%
  filter(coef > distQuant[1] & coef < distQuant[2]) %>%
# ssfCoefs_long %>%
#   filter(termType == "Distance") %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_errorbarh(aes(xmin = lower, xmax = upper, y = Animal_ID_colour, color = sig), height = 0.2,
                 alpha = 0.35) +
  geom_point(aes(x = coef, y = Animal_ID_colour, color = sig)) +
  geom_text(data = outlierDistLabs,
             aes(x = xloc, y = Animal_ID_colour, color = sig, label = text, hjust = hjust),
            fontface = 2) +
  coord_cartesian(xlim = ssfCoefs_dist %>%
                    filter(coef > distQuant[1] & coef < distQuant[2]) %>%
                    pull(coef) %>% range(na.rm = TRUE)) +
  scale_colour_manual(values = paletteList$highSigLowSigNoSig) +
  facet_wrap(vars(term), ncol = 1, drop = TRUE, scales = "free_y",
             strip.position = "left") +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    # legend.position = "none",
    axis.title = element_text(face = 2),
    axis.ticks.y = element_blank(),
    axis.text.y = element_markdown(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
  )


# land use ----------------------------------------------------------------

landQuant <- ssfCoefs_long %>%
  filter(termType == "Landuse") %>%
  pull(coef) %>% quantile(probs = c(0.05, 0.95), na.rm = TRUE)

outlierLandLabs <- ssfCoefs_long %>%
  filter(termType == "Landuse") %>%
  filter(coef < landQuant[1]) %>%
  mutate(xloc = -Inf, hjust = 0,
         text = paste0("<", round(coef, digits = 1))) %>%
  rbind(ssfCoefs_long %>%
          filter(termType == "Landuse") %>%
          filter(coef > landQuant[2]) %>%
          mutate(xloc = Inf, hjust = 1) %>%
          mutate(text = paste0(round(coef, digits = 1), ">")))

ssfCoefs_long %>%
  filter(termType == "Landuse") %>%
  filter(coef > landQuant[1] & coef < landQuant[2]) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_errorbarh(aes(xmin = lower, xmax = upper, y = Animal_ID, color = sig), height = 0.2) +
  geom_point(aes(x = coef, y = Animal_ID, color = sig)) +
  geom_text(data = outlierLandLabs,
            aes(x = xloc, y = Animal_ID, color = sig, label = text, hjust = hjust),
            fontface = 2) +
  coord_cartesian(xlim = ssfCoefs_long %>%
                    filter(termType == "Landuse") %>%
                    filter(coef > landQuant[1] & coef < landQuant[2]) %>%
                    pull(coef) %>% range(na.rm = TRUE)) +
  scale_colour_manual(values = paletteList$highSigLowSigNoSig) +
  facet_wrap(vars(term), ncol = 1, drop = TRUE, scales = "free_y",
             strip.position = "left") +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    axis.title = element_text(face = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    # legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
  )

ssfCoefs_long %>%
  filter(termType == "Step") %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_errorbarh(aes(xmin = lower, xmax = upper, y = Animal_ID, color = sig), height = 0.2) +
  geom_point(aes(x = coef, y = Animal_ID, color = sig)) +
  coord_cartesian(xlim = ssfCoefs_long %>%
                    filter(termType == "Step") %>%
                    pull(coef) %>% range(na.rm = TRUE)) +
  scale_colour_manual(values = paletteList$highSigLowSigNoSig) +
  facet_wrap(vars(term), ncol = 1, drop = TRUE, scales = "free_y",
             strip.position = "left") +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    axis.title = element_text(face = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    # legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
  )

ssfCoefs_long %>%
  filter(termType == "Other") %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_errorbarh(aes(xmin = lower, xmax = upper, y = Animal_ID, color = sig), height = 0.2) +
  geom_point(aes(x = coef, y = Animal_ID, color = sig)) +
  coord_cartesian(xlim = ssfCoefs_long %>%
                    filter(termType == "Other") %>%
                    pull(coef) %>% range(na.rm = TRUE)) +
  scale_colour_manual(values = paletteList$highSigLowSigNoSig) +
  facet_wrap(vars(term), ncol = 1, drop = TRUE, scales = "free_y",
             strip.position = "left") +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    axis.title = element_text(face = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    # legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
  )



