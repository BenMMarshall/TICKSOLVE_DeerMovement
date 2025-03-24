
library(biomod2)
targets::tar_load("tar_biomodModels_rodent")
biomodModels <- tar_biomodModels_rodent

# Get evaluation scores & variables importance
get_evaluations(biomodModels)
get_variables_importance(biomodModels)

# Represent evaluation scores & variables importance
bm_PlotEvalMean(bm.out = biomodModels)
bm_PlotEvalBoxplot(bm.out = biomodModels, group.by = c('algo', 'algo'))
bm_PlotEvalBoxplot(bm.out = biomodModels, group.by = c('algo', 'run'))
bm_PlotVarImpBoxplot(bm.out = biomodModels, group.by = c('expl.var', 'algo', 'algo'))
bm_PlotVarImpBoxplot(bm.out = biomodModels, group.by = c('expl.var', 'algo', 'run'))
bm_PlotVarImpBoxplot(bm.out = biomodModels, group.by = c('algo', 'expl.var', 'run'))

# Represent response curves
bm_PlotResponseCurves(bm.out = biomodModels,
                      models.chosen = get_built_models(biomodModels),
                      fixed.var = 'mean')
bm_PlotResponseCurves(bm.out = biomodModels,
                      models.chosen = get_built_models(biomodModels)[c(1:3, 12:14)],
                      fixed.var = 'min')
bm_PlotResponseCurves(bm.out = biomodModels,
                      models.chosen = get_built_models(biomodModels)[3],
                      fixed.var = 'median',
                      do.bivariate = TRUE)


respCurve <- bm_PlotResponseCurves(bm.out = biomodModels,
                                      models.chosen = get_built_models(biomodModels),
                                      fixed.var = 'mean')

respCurve$tab %>%
  mutate(model.type = str_extract(pred.name, "[^_]+$")) %>%
  filter(str_detect(expl.name, "distance")) %>%
  ggplot() +
  # geom_rect(data = wessexRanges, aes(xmin = min, xmax = max,
  #                                    ymin = -Inf, ymax = Inf), fill = "#85AB7A", alpha = 0.2) +
  geom_path(aes(x = expl.val, y = pred.val, group = pred.name, colour = model.type)) +
  facet_wrap(facet = vars(expl.name), scales = "free") +
  ggplotThemeCombo +
  # theme(legend.position = "none") +
  scale_colour_manual(values = c("#DCBD0A",
                                 "#CD602A",
                                 "#9F7E93",
                                 "#85AB7A")) +
  scale_fill_manual(values = c("#DCBD0A",
                               "#CD602A",
                               "#9F7E93",
                               "#85AB7A"))

respCurve$tab %>%
  mutate(model.type = str_extract(pred.name, "[^_]+$")) %>%
  filter(!str_detect(expl.name, "distance")) %>%
  ggplot() +
  geom_vline(xintercept = c(1,2), linetype = "dashed", alpha = 0.85) +
  geom_path(aes(x = expl.val, y = pred.val, group = pred.name, colour = model.type)) +
  facet_wrap(facet = vars(expl.name)) +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("0", "1")) +
  ggplotThemeCombo +
  scale_colour_manual(values = c("#DCBD0A",
                                 "#CD602A",
                                 "#9F7E93",
                                 "#85AB7A")) +
  scale_fill_manual(values = c("#DCBD0A",
                               "#CD602A",
                               "#9F7E93",
                               "#85AB7A"))
