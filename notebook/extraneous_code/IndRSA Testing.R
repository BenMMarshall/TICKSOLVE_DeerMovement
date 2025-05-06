
ssfCoefs_long %>%
  filter(!is.na(coef)) %>%
  ggplot() +
  geom_point(aes(x = coef, y = Animal_ID)) +
  facet_wrap(facet = vars(termType, term), scales = "free", drop = TRUE)

ssfModels

library(amt)
library(dplyr)
library(IndRSA)

library(tidyr)
library(ggplot2)
library(ggridges)
library(ggtext)

targets::tar_load("tar_ssf_data")
# targets::tar_load("tar_ssf_models")
# ssfModels <- tar_ssf_models
ssfDataList <- tar_ssf_data

# models ------------------------------------------------------------------

ssfData <- do.call(rbind, lapply(ssfDataList, function(x){x$steps}))
ssfData <- ssfData %>%
  select(Animal_ID, step_id_, case_,
         landuse, distanceWoodland,
         distanceHedges, roadCrossings,
         sl_, ta_)

ssfModels <- ssf_ind(id = ssfData$Animal_ID, data = ssfData,
                     form_ls = list(as.formula(case_ ~ landuse +
                                                 distanceWoodland +
                                                 distanceHedges +
                                                 roadCrossings +
                                                 sl_ + log(sl_) + cos(ta_) +
                                                 # sl_:landuse +
                                                 # log(sl_):landuse +
                                                 strata(step_id_))),
                     cleanModel = FALSE, method = "exact")


# Outputs and tables ------------------------------------------------------
paletteList <- load_deer_palette()
sigColourDF <- data.frame(colour = paletteList$highSigLowSigNoSig,
                          sig = names(paletteList$highSigLowSigNoSig))

indiCoef <- ind_coef(1, ssfModels)
indiSE <- ind_se(1, ssfModels)

ssfPopAvg <- pop_avg(m = 1, mod_ls = ssfModels)

popAvg <- as.data.frame(ssfPopAvg[[1]])
popAvg$variable <- rownames(popAvg)
popAvg$Animal_ID <- "<b>Population</b>"
popAvg <- popAvg %>%
  mutate(sig = ifelse(LCI > 0, "Significant +", ifelse(UCI < 0, "Significant -", "Not Significant")),
         variableType = case_when(
           grepl("sl_", variable) ~ "Step",
           grepl("distance", variable) ~ "Distance",
           grepl("landuse", variable) ~ "Landuse",
           TRUE ~ "Other"
         ),
         variable = case_when(
           str_detect(variable, "\\.") ~ str_replace_all(gsub("landuse", "", variable), "\\.", " "),
           str_detect(variable, "distance") ~ str_replace_all(variable, "distance", "Distance to "),
           variable == "roadCrossingsTRUE" ~ "Road Crossing",
           TRUE ~ gsub("landuse", "", variable)
         )) %>%
  filter(!variable == "landuseBarren") %>%
  left_join(sigColourDF, by = "sig") %>%
  mutate(Animal_ID_colour = glue::glue("<i style='color:{colour}'>{Animal_ID}</i>")) %>%
  mutate(Animal_ID_colour = factor(Animal_ID_colour, levels = c(unique(Animal_ID_colour))))

indiDataframe <- indiCoef %>%
  as.data.frame() %>%
  select(-ID, -Freq, -landuseBarren) %>%
  pivot_longer(where(is.numeric), names_to = "variable", values_to = "est") %>%
  left_join(indiSE %>%
              as.data.frame() %>%
              select(-ID, -Freq, -landuseBarren) %>%
              pivot_longer(where(is.numeric), names_to = "variable", values_to = "se")) %>%
  rename("Animal_ID" = name) %>%
  mutate(sig = ifelse(est-se > 0, "Significant +", ifelse(est+se < 0, "Significant -", "Not Significant")),
         variableType = case_when(
           grepl("sl_", variable) ~ "Step",
           grepl("distance", variable) ~ "Distance",
           grepl("landuse", variable) ~ "Landuse",
           TRUE ~ "Other"
         ),
         variable = case_when(
           str_detect(variable, "\\.") ~ str_replace_all(gsub("landuse", "", variable), "\\.", " "),
           str_detect(variable, "distance") ~ str_replace_all(variable, "distance", "Distance to "),
           variable == "roadCrossingsTRUE" ~ "Road Crossing",
           TRUE ~ gsub("landuse", "", variable)
         )) %>%
  left_join(sigColourDF, by = "sig") %>%
  mutate(Animal_ID_colour = glue::glue("<i style='color:{colour}'>{Animal_ID}</i>")) %>%
  arrange(desc(Animal_ID)) %>%
  mutate(Animal_ID_colour = factor(Animal_ID_colour, levels = c(unique(popAvg$Animal_ID_colour), unique(Animal_ID_colour))))

simuCoefs <- simu_coefs(coef = indiCoef, se = indiSE, n = 10000)

simuSD <- simu_sd(simuCoefs)
colnames(simuSD) <- names(indiCoef)
apply(simuSD, 2, quantile, na.rm=T) #Show variation around estimate of each covariate
simuSDmeans <- colMeans(simuSD) #Calculate average heterogeneity for each covariate
simuSDmeans <- data.frame(
  variables = names(simuSDmeans),
  meanSD = simuSDmeans) %>%
  filter(!is.na(simuSDmeans))

simuSD <- simuSD %>%
  as.data.frame() %>%
  select(-name, -ID, -Freq, -landuseBarren) %>%
  pivot_longer(everything(), values_to = "value") %>%
  rename("variable" = name) %>%
  mutate(variableType = case_when(
    variable %in% c("distanceWoodland", "distanceHedges") ~ "Distance from",
    variable %in% c("roadCrossingsTRUE") ~ "Other",
    variable %in% c("sl_", "cos.ta_.", "log.sl_.") ~ "Core movement",
    str_detect(variable, "\\:sl_") ~ "Step interactions (sl_)",
    str_detect(variable, "\\:log.sl_.") ~ "Step interactions (log_sl)",
    TRUE ~ "Landuse"
  ),
  valueMeasure = "Heterogeneity",
  variable = case_when(
    str_detect(variable, "\\.") ~ str_replace_all(gsub("landuse", "", variable), "\\.", " "),
    str_detect(variable, "distance") ~ str_replace_all(variable, "distance", "Distance to "),
    variable == "roadCrossingsTRUE" ~ "Road Crossing",
    TRUE ~ gsub("landuse", "", variable)
  )) %>%
  arrange(desc(variable)) %>%
  mutate(variable = factor(variable, levels = unique(variable)))

simuSPE <- simu_spe(simuCoefs)
colnames(simuSPE) <- names(indiCoef)
apply(simuSPE, 2, quantile, na.rm=T) #Show variation around estimate of each covariate
simuSPEmeans <- colMeans(simuSPE) #Calculate average specialization for each covariate
simuSPEmeans <- data.frame(
  variables = names(simuSPEmeans),
  meanSPE = simuSPEmeans) %>%
  filter(!is.na(simuSPEmeans))

simuSPE <- simuSPE %>%
  as.data.frame() %>%
  select(-name, -ID, -Freq, -landuseBarren) %>%
  pivot_longer(everything(), values_to = "value") %>%
  rename("variable" = name) %>%
  mutate(variableType = case_when(
    variable %in% c("distanceWoodland", "distanceHedges") ~ "Distance from",
    variable %in% c("roadCrossingsTRUE") ~ "Other",
    variable %in% c("sl_", "cos.ta_.", "log.sl_.") ~ "Core movement",
    str_detect(variable, "\\:sl_") ~ "Step interactions (sl_)",
    str_detect(variable, "\\:log.sl_.") ~ "Step interactions (log_sl)",
    TRUE ~ "Landuse"
  ),
  valueMeasure = "Specialisation",
  variable = case_when(
    str_detect(variable, "\\.") ~ str_replace_all(gsub("landuse", "", variable), "\\.", " "),
    str_detect(variable, "distance") ~ str_replace_all(variable, "distance", "Distance to "),
    variable == "roadCrossingsTRUE" ~ "Road Crossing",
    TRUE ~ gsub("landuse", "", variable)
  )) %>%
  arrange(desc(variable)) %>%
  mutate(variable = factor(variable, levels = unique(variable)))

# Plots -------------------------------------------------------------------

indiDataframe %>%
  arrange(desc(Animal_ID_colour)) %>%
  filter(variableType == "Landuse") %>%
  filter(!is.na(est)) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25) +
  geom_hline(yintercept = 1.5, linetype = "solid", linewidth = 0.25) +
  geom_hline(yintercept = 0.5, linetype = "solid", linewidth = 0.25) +
  geom_errorbarh(data = popAvg %>%
                   filter(variableType == "Landuse"),
                 aes(xmin = LCI, xmax = UCI, y = Animal_ID_colour, colour = sig)) +
  geom_point(data = popAvg %>%
               filter(variableType == "Landuse"),
             aes(x = Mean, y = Animal_ID_colour, colour = sig)) +
  # geom_errorbarh(aes(xmin = est-se, xmax = est+se, y = variable),
  #                height = 0.1) +
  geom_point(aes(x = est, y = Animal_ID_colour, colour = sig), size = 0.65) +
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
    # legend.position = "none",
    plot.title = element_text(face = 2),
    axis.title = element_text(face = 2),
    axis.ticks.y = element_blank(),
    axis.text.y = element_markdown(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
  )

simuSD %>%
  filter(!variableType == "Core movement") %>%
  ggplot() +
  geom_boxplot(aes(x = value, y = variable), width = 0.2, position = position_nudge(y = -0.1)) +
  # geom_density_ridges(aes(x = value, y = variable), alpha = 0.5) +
  facet_wrap(vars(variableType), ncol = 1, drop = TRUE, scales = "free",
             strip.position = "left") +
  labs(x = "Heterogeneity\n(individual variation in response)", y = "Term", title = "Heterogeneity") +
  scale_x_continuous(limits = c(0, NA)) +
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
  )

simuSPE %>%
  filter(!variableType == "Core movement") %>%
  ggplot() +
  geom_boxplot(aes(x = value, y = variable), width = 0.2, position = position_nudge(y = -0.1)) +
  # geom_density_ridges(aes(x = value, y = variable), alpha = 0.5) +
  facet_wrap(vars(variableType), ncol = 1, drop = TRUE, scales = "free",
             strip.position = "left") +
  labs(x = "Specialisation\n(magnitude of the response independent of the direction)", y = "Term", title = "Specialisation") +
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
  )
