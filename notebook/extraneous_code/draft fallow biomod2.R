library(here)
library(dplyr)
library(terra)
library(biomod2)

#### TEMP EXAMPLE BIOCLIM DATA
# Load environmental variables extracted from BIOCLIM (bio_3, bio_4, bio_7, bio_11 & bio_12)
data("bioclim_current")
TEMP_myExpl <- rast(bioclim_current)
####

occData <- read.csv(here("data", "GBIF data", "fallowUK.csv"),
                    sep = "\t")

occData$resp <- 1
occData$decimalLatitude
# expl.var should be a spatRaster

# Format Data with pseudo-absences : random method
myBiomodData.multi <- BIOMOD_FormatingData(resp.var = occData$resp,
                                           expl.var = TEMP_myExpl,
                                           resp.xy = occData[, c("decimalLongitude",
                                                                 "decimalLatitude")],
                                           resp.name = occData$verbatimScientificName[1],
                                           PA.nb.rep = 3,
                                           PA.nb.absences = 1000,
                                           PA.strategy = "sre")
myBiomodData.multi
summary(myBiomodData.multi)
plot(myBiomodData.multi)

# stratified selection (geographic)
cv.e <- bm_CrossValidation(bm.format = myBiomodData.multi,
                           strategy = "block",
                           balance = "env",
                           strat = "both")
head(cv.e)

# Model single models
myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData.multi,
                                    modeling.id = "AllModels",
                                    models = c("GAM", "GLM"),
                                    CV.strategy = "user.defined",
                                    CV.user.table = cv.e,
                                    # CV.strategy = 'random',
                                    # CV.nb.rep = 2,
                                    # CV.perc = 0.8,
                                    OPT.strategy = 'bigboss',
                                    var.import = 3,
                                    metric.eval = c('TSS','ROC'),
                                    seed.val = 2025,
                                    nb.cpu = 2)
myBiomodModelOut

# Get evaluation scores & variables importance
get_evaluations(myBiomodModelOut)
get_variables_importance(myBiomodModelOut)

# Represent evaluation scores & variables importance
bm_PlotEvalMean(bm.out = myBiomodModelOut)
bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'algo'))
bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'run'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'algo'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'run'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'expl.var', 'run'))

# Represent response curves
bm_PlotResponseCurves(bm.out = myBiomodModelOut,
                      models.chosen = get_built_models(myBiomodModelOut)[c(1:3, 12:14)],
                      fixed.var = 'median')
bm_PlotResponseCurves(bm.out = myBiomodModelOut,
                      models.chosen = get_built_models(myBiomodModelOut)[c(1:3, 12:14)],
                      fixed.var = 'min')
bm_PlotResponseCurves(bm.out = myBiomodModelOut,
                      models.chosen = get_built_models(myBiomodModelOut)[3],
                      fixed.var = 'median',
                      do.bivariate = TRUE)

# Model ensemble models
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut,
                                      models.chosen = 'all',
                                      em.by = 'all',
                                      em.algo = c('EMmean', 'EMcv', 'EMci',
                                                  'EMmedian', 'EMca', 'EMwmean'),
                                      metric.select = c('TSS'),
                                      metric.select.thresh = c(0.7),
                                      metric.eval = c('TSS', 'ROC'),
                                      var.import = 3,
                                      EMci.alpha = 0.05,
                                      EMwmean.decay = 'proportional')
myBiomodEM

# Get evaluation scores & variables importance
get_evaluations(myBiomodEM)
get_variables_importance(myBiomodEM)

# Represent evaluation scores & variables importance
bm_PlotEvalMean(bm.out = myBiomodEM, group.by = 'full.name')
bm_PlotEvalBoxplot(bm.out = myBiomodEM, group.by = c('full.name', 'full.name'))
bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'full.name', 'full.name'))
bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'algo', 'merged.by.run'))
bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('algo', 'expl.var', 'merged.by.run'))

# Represent response curves
bm_PlotResponseCurves(bm.out = myBiomodEM,
                      models.chosen = get_built_models(myBiomodEM)[c(1, 6, 7)],
                      fixed.var = 'median')
bm_PlotResponseCurves(bm.out = myBiomodEM,
                      models.chosen = get_built_models(myBiomodEM)[c(1, 6, 7)],
                      fixed.var = 'min')
bm_PlotResponseCurves(bm.out = myBiomodEM,
                      models.chosen = get_built_models(myBiomodEM)[7],
                      fixed.var = 'median',
                      do.bivariate = TRUE)

UKbbox <- st_bbox(c(xmin = -20, xmax = 10, ymin = 35, ymax = 65))

projTarget <- crop(TEMP_myExpl, UKbbox)

# Project ensemble models (building single projections)
myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                             proj.name = 'CurrentEM',
                                             new.env = projTarget,
                                             models.chosen = 'all',
                                             metric.binary = 'all',
                                             metric.filter = 'all')
myBiomodEMProj

library(tidyterra)
library(ggplot2)

ggplot() +
  geom_spatraster(data = rast(myBiomodEMProj@proj.out@link[1])) +
  facet_wrap(facet = vars(lyr)) +
  coord_sf()

## could then examine which best ensemble projection matches the movement data
## we have and use that
