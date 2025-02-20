# install.packages("biomod2")

install.packages("gam")
install.packages("mgcv")
install.packages("earth")
install.packages("maxnet")
install.packages("randomForest")
install.packages("xgboost")
install.packages("ENMeval")

library(biomod2)
library(terra)

vignette("examples_1_mainFunctions", package = "biomod2")

# Load species occurrences (6 species available)
data("DataSpecies")
head(DataSpecies)

# Select the name of the studied species
myRespName <- 'GuloGulo'

# Get corresponding presence/absence data
myResp <- as.numeric(DataSpecies[, myRespName])

# Get corresponding XY coordinates
myRespXY <- DataSpecies[, c('X_WGS84', 'Y_WGS84')]

# Load environmental variables extracted from BIOCLIM (bio_3, bio_4, bio_7, bio_11 & bio_12)
data("bioclim_current")
myExpl <- rast(bioclim_current)

# Format Data with true absences
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName)
myBiomodData
plot(myBiomodData)

# Transform true absences into potential pseudo-absences
myResp.PA <- ifelse(myResp == 1, 1, NA)
myResp.PA.vect <- vect(cbind(myRespXY, myResp.PA), geom = c("X_WGS84","Y_WGS84"))
# user.defined method
myPAtable <- data.frame(PA1 = ifelse(myResp == 1, TRUE, FALSE),
                        PA2 = ifelse(myResp == 1, TRUE, FALSE))
for (i in 1:ncol(myPAtable)) myPAtable[sample(which(myPAtable[, i] == FALSE), 500), i] = TRUE
PA.u <- bm_PseudoAbsences(resp.var = myResp.PA.vect,
                          expl.var = myExpl,
                          strategy = 'user.defined',
                          user.table = myPAtable)

# Format Data with pseudo-absences : random method
myBiomodData.multi <- BIOMOD_FormatingData(resp.var = myResp.PA,
                                           expl.var = myExpl,
                                           resp.xy = myRespXY,
                                           resp.name = myRespName,
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
                                    models = c("GLM"),
                                    CV.strategy = "user.defined",
                                    CV.user.table = cv.e,
                                    # CV.strategy = 'random',
                                    # CV.nb.rep = 2,
                                    # CV.perc = 0.8,
                                    OPT.strategy = 'bigboss',
                                    var.import = 3,
                                    metric.eval = c('TSS','ROC'))
# seed.val = 123)
# nb.cpu = 8)
myBiomodModelOut

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

# Project ensemble models (building single projections)
myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                             proj.name = 'CurrentEM',
                                             new.env = myExpl,
                                             models.chosen = 'all',
                                             metric.binary = 'all',
                                             metric.filter = 'all')
myBiomodEMProj
plot(myBiomodEMProj)

## could then examine which best ensemble projection matches the movement data
## we have and use that
