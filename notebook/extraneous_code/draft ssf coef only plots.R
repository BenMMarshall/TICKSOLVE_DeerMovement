
targets::tar_load("tar_ssf_models")

ssfModels <- tar_ssf_models

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
ssfCoefs$term <- gsub("[[:digit:]]{0,2}$", "", row.names(ssfCoefs))

naiveMeanSsfCoefs <- ssfCoefs %>%
  group_by(term) %>%
  summarise(meanEffect = mean(coef, na.rm = TRUE))

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
  mutate(sig = ifelse(lower > 0, "Sig +", ifelse(upper < 0, "Sig -", "Not Significant")))

ssfCoefs_long %>%
  filter(termType == "Distance") %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  # geom_errorbarh(aes(xmin = lower, xmax = upper, y = Animal_ID, height = 0.2)) +
  geom_point(aes(x = coef, y = Animal_ID, color = sig)) +
  coord_cartesian() +
  facet_wrap(vars(term), ncol = 1, drop = TRUE, scales = "free_y",
             switch = "y") +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    axis.title = element_text(face = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    # legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
  )

ssfCoefs_long %>%
  filter(termType == "Landuse") %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  # geom_errorbarh(aes(xmin = lower, xmax = upper, y = Animal_ID, height = 0.2)) +
  geom_point(aes(x = coef, y = Animal_ID, color = sig)) +
  coord_cartesian() +
  facet_wrap(vars(term), ncol = 1, drop = TRUE, scales = "free_y",
             switch = "y") +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    axis.title = element_text(face = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    # legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
  )

ssfCoefs_long %>%
  filter(termType == "Step") %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  # geom_errorbarh(aes(xmin = lower, xmax = upper, y = Animal_ID, height = 0.2)) +
  geom_point(aes(x = coef, y = Animal_ID, color = sig)) +
  coord_cartesian() +
  facet_wrap(vars(term), ncol = 1, drop = TRUE, scales = "free_y",
             switch = "y") +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    axis.title = element_text(face = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    # legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
  )

ssfCoefs_long %>%
  filter(termType == "Other") %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  # geom_errorbarh(aes(xmin = lower, xmax = upper, y = Animal_ID, height = 0.2)) +
  geom_point(aes(x = coef, y = Animal_ID, color = sig)) +
  coord_cartesian() +
  facet_wrap(vars(term), ncol = 1, drop = TRUE, scales = "free_y",
             switch = "y") +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    axis.title = element_text(face = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    # legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
  )



