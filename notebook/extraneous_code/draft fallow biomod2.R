library(here)
library(dplyr)
library(terra)
library(biomod2)
library(CoordinateCleaner)

#### TEMP EXAMPLE BIOCLIM DATA
# Load environmental variables extracted from BIOCLIM (bio_3, bio_4, bio_7, bio_11 & bio_12)
data("bioclim_current")
TEMP_myExpl <- rast(bioclim_current)
####

occData <- read.csv(here("data", "GBIF data", "fallowUK.csv"),
                    sep = "\t")

flags <- clean_coordinates(x = occData,
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           species = "species",
                           tests = c("capitals", "centroids",
                                     "equal", "zeros",
                                     "outliers"),
                           capitals_rad = 10000,
                           centroids_rad = 1000,
                           outliers_mtp = 5,
                           outliers_method = "quantile",
                           outliers_size = 7)

# flags[!flags$.summary,]
# summary(flags)
# plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

occData <- occData[flags$.summary,]

occData$resp <- 1
occData$decimalLatitude
# expl.var should be a spatRaster

# Format Data with pseudo-absences : random method
myBiomodData.multi <- BIOMOD_FormatingData(resp.var = occData$resp,
                                           expl.var = TEMP_myExpl,
                                           resp.xy = occData[, c("decimalLongitude",
                                                                 "decimalLatitude")],
                                           resp.name = occData$verbatimScientificName[1],
                                           PA.nb.rep = 1,
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
                                    models = c("ANN",
                                               "GBM", "GLM",
                                               "MAXNET",
                                               "RF", "XGBOOST"),
                                    CV.strategy = "user.defined",
                                    CV.user.table = cv.e,
                                    # CV.strategy = 'random',
                                    # CV.nb.rep = 2,
                                    # CV.perc = 0.8,
                                    OPT.strategy = 'bigboss',
                                    var.import = 3,
                                    metric.eval = c('TSS','ROC'),
                                    seed.val = 2025,
                                    nb.cpu = 6)
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
targets::tar_source()
stack <- read_stack_layers()

ggplot() +
  geom_spatraster(data = stack %>%
                    dplyr::select(contains("distance"))) +
  facet_wrap(facet = vars(lyr))

ggplot() +
  geom_spatraster(data = stack %>%
                    dplyr::select(!contains("distance"))) +
  facet_wrap(facet = vars(lyr))

seaClip <- rnaturalearth::ne_download(scale = 10, type = 'land', category = 'physical', returnclass = "sf")
plot(seaClip)
seaClip <- st_transform(seaClip, crs(landuseWessex))
plot(st_crop(seaClip, landuseWessex))

r <- rast(ncols=5, nrows=5, xmin=0, xmax=1, ymin=0, ymax=1, crs="")
r <- init(r, 1:6)
x <- rast(ncols=5, nrows=5, xmin=0, xmax=1, ymin=0, ymax=1, crs="")
x <- init(x, c(1, NA))
plot(x)
plot(r*x)


x <- subst(r, 3, 7)
x <- subst(r, 2:3, NA)
x <- subst(x, NA, 10)

# multiple output layers
z <- subst(r, 2:3, cbind(20,30))

# multiple input layers
rr <- c(r, r+1, r+2)
m <- rbind(c(1:3), c(3:5))
zz <- subst(rr, m, c(100, 200))


# check forecast ----------------------------------------------------------

targets::tar_load("tar_biomodForecast")

projOUT <- terra::unwrap(tar_biomodForecast@proj.out@val)
projEMmeanTSS <- projOUT %>%
  dplyr::select(contains("EMmeanByTSS"))
names(projEMmeanTSS) <- "projOcc"

targets::tar_load("tar_patchList")
targets::tar_load("tar_deerData")
library(stringr)

ggplot() +
  geom_spatraster(data = projEMmeanTSS, aes(fill = projOcc)) +
  geom_sf(data = tar_patchList$Wessex, alpha = 0.5, colour = NA, fill = "white") +
  geom_point(data = tar_deerData %>% filter(str_detect(Animal_ID, "Fall")), aes(x = x, y = y),
             size = 0.5)

targets::tar_load("tar_biomodEns")

bm_PlotResponseCurves(bm.out = tar_biomodEns,
                      models.chosen = get_built_models(tar_biomodEns)[str_detect(get_built_models(tar_biomodEns), "EMmeanByTSS")],
                      fixed.var = 'mean')


# human footprint ---------------------------------------------------------

hfData <- terra::rast(here("data", "Human Footprint", "hfp2022.tif"))
UKbbox <- st_bbox(c(xmin = -900000, xmax = 200000, ymin = 5500000, ymax = 7000000))
hfDataCrop <- terra::crop(hfData, UKbbox)
hfDataBNG <- terra::project(hfDataCrop, landuseWessex)
plot(hfDataBNG)
rescale <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}
hfDataBNG <- hfDataBNG %>%
  mutate(hfp2022 = rescale(hfp2022))

pseudoWeighted <- spatSample(hfDataBNG, 1000, method = "weights",
                             na.rm = TRUE, as.df = TRUE, values = TRUE,
                             xy = TRUE)

pseudoNoWeighted <- spatSample(hfDataBNG, 1000, method = "random",
                             na.rm = TRUE, as.df = TRUE, values = TRUE,
                             xy = TRUE)

ggplot() +
  geom_spatraster(data = hfDataBNG) +
  geom_point(data = pseudoNoWeighted, aes(x = x, y = y), colour = "red") +
  geom_point(data = pseudoWeighted, aes(x = x, y = y))

ggplot() +
  geom_density(data = pseudoNoWeighted, aes(x = hfp2022), colour = "red") +
  geom_density(data = pseudoWeighted, aes(x = hfp2022))


# -------------------------------------------------------------------------
View(tar_pseudoAbs$pa.tab)
tar_pseudoAbs$sp

BIOMOD_FormatingData(resp.var = tar_occData$resp,
                     expl.var = read_stack_layers(layerLoc = here("data", "GIS data", "SDM Layers"),
                                                  tar_sdm_layers),
                     resp.xy = st_coordinates(tar_occData),
                     resp.name = "Dama.dama",
                     PA.strategy = "user.defined",
                     PA.user.table = tar_pseudoAbs)

BIOMOD_FormatingData(resp.var = ifelse(tar_pseudoAbs$sp == 1, 1, NA),
                     expl.var = read_stack_layers(layerLoc = here("data", "GIS data", "SDM Layers"),
                                                  tar_sdm_layers),
                     resp.xy = tar_pseudoAbs$xy,
                     resp.name = "Dama.dama",
                     PA.strategy = "user.defined",
                     filter.raster = TRUE,
                     PA.user.table = tar_pseudoAbs)
