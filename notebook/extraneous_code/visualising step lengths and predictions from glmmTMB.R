
library(dplyr)
library(sjmisc)

targets::tar_load("tar_ssf_data")
ssfDataList <- tar_ssf_data
ssfData <- do.call(rbind, lapply(ssfDataList, function(x){x$steps}))

poisModelData <- ssfData %>%
  ungroup() %>%
  mutate(
    y = as.numeric(case_),
    id = as.numeric(factor(Animal_ID)),
    step_id = paste0(id, "-", step_id_),
    cos_ta = cos(ta_),
    log_sl = log(sl_)) %>%
  mutate(roadCrossings = as.numeric(roadCrossings))

poisModelData

dummyVars <- poisModelData %>%
  mutate(landuse = as.factor(landuse)) %>%
  to_dummy(landuse, suffix = "label") %>%
  rename(
    Deciduous.Broadleaf.Forest = `landuse_Deciduous Broadleaf Forest`,
    Evergreen.Needleleaf.Forest = `landuse_Evergreen Needleleaf Forest`,
    Cropland = landuse_Cropland,
    Tall.Grassland = `landuse_Tall Grassland`,
    Short.Grassland = `landuse_Short Grassland`,
    Open.Shrubland = `landuse_Open Shrubland`,
    Permanent.Wetland = `landuse_Permanent Wetland`,
    Human.Settlements = `landuse_Human Settlements`,
    Other = landuse_Other
  )
apply(dummyVars, 2, sum)

poisModelData <- cbind(poisModelData, dummyVars)

glmmForm <- y ~ -1 +
  distanceWoodland +
  distanceHedges +
  # Deciduous.Broadleaf.Forest +
  Evergreen.Needleleaf.Forest +
  Cropland +
  Tall.Grassland +
  Short.Grassland +
  Open.Shrubland +
  Permanent.Wetland +
  Human.Settlements +
  # Other +
  roadCrossings +
  # step and turn
  sl_ +
  log_sl +
  cos_ta +
  # step interactions
  sl_:Evergreen.Needleleaf.Forest +
  sl_:Cropland +
  sl_:Tall.Grassland +
  sl_:Short.Grassland +
  sl_:Open.Shrubland +
  sl_:Permanent.Wetland +
  sl_:Human.Settlements +
  # log step interactions
  log_sl:Evergreen.Needleleaf.Forest +
  log_sl:Cropland +
  log_sl:Tall.Grassland +
  log_sl:Short.Grassland +
  log_sl:Open.Shrubland +
  log_sl:Permanent.Wetland +
  log_sl:Human.Settlements +
  (1 | step_id_) +
  (0 + distanceWoodland | id) +
  (0 + distanceHedges | id) +
  (0 + Evergreen.Needleleaf.Forest | id) +
  (0 + Cropland | id) +
  (0 + Tall.Grassland | id) +
  (0 + Short.Grassland | id) +
  (0 + Open.Shrubland | id) +
  (0 + Permanent.Wetland | id) +
  (0 + Human.Settlements | id) +
  (0 + roadCrossings | id)

library(glmmTMB)

TMBStruc = glmmTMB(glmmForm,
                   family = poisson, data = poisModelData, doFit = FALSE)

TMBStruc$parameters$theta[1] = log(1e3)
TMBStruc$mapArg = list(theta=factor(c(NA,1:10)))
glmm.TMB.random <- glmmTMB:::fitTMB(TMBStruc)
summary(glmm.TMB.random)

# extract the coefs from the models
coefTable <- as.data.frame(summary(glmm.TMB.random)$coef$cond)
coefTable$term <- row.names(coefTable)

# pull out the step lengths in the observed data
steps <- poisModelData %>%
  filter(case_) %>%
  pull(sl_)

# https://conservancy.umn.edu/server/api/core/bitstreams/63727072-87b1-4b35-b81c-8fd31b8f1e57/content
# step length (sl_) and its natural logarithm (log_sl_). We included these
# parameters to update the scale and shape parameters of our tentative gamma
# distribution, respectively.

# use amt pacakge to get the baseline shape and scale from the real data
fittedDist <- amt::fit_distr(steps, "gamma")

# how many values to generate to show the distributions
nVals <- 20000

# generated a bunch of values so we can see the baseline step length distribution, we'll plot this later
stepData <- data.frame(
  name = "overallDataFit",
  values = rgamma(n = nVals, scale = fittedDist$params$scale, shape = fittedDist$params$shape)
)

library(stringr)

slTable <- coefTable %>%
  filter(str_detect(term, ":sl"))
logTable <- coefTable %>%
  filter(str_detect(term, ":log_"))

slCoef <- coefTable$Estimate[coefTable$term == "sl_"]
logslCoef <- coefTable$Estimate[coefTable$term == "log_sl"]

# use lappaly to go through each term/habitat and generated 2000 values that approximate the step length for each
stepDataAdj <- do.call(rbind, lapply(1:nrow(slTable), function(x){
  data.frame(
    name = gsub(":.*", "", slTable$term[x]),
    values = rgamma(n = nVals,
                    scale = fittedDist$params$scale + slCoef + slTable$Estimate[x],
                    shape = fittedDist$params$shape + logslCoef + logTable$Estimate[x])
  )
}))

library(ggplot2)
library(ggridges)

stepAll <- rbind(stepDataAdj, stepData)

ggplot(stepAll) +
  geom_density_ridges(aes(x = values, y = name, fill = name))


# predict -----------------------------------------------------------------

library(stringr)
library(dplyr)
library(ggplot2)

newData <- poisModelData
newData$case_ <- NA

# preds <- predict(glmm.TMB.random, newdata = newData)

# CREATE NEW DATA SET TO PASS TO MODEL MATRIX
distanceWoodland <- newData$distanceWoodland
distanceHedges <- newData$distanceHedges
roadCrossings <- newData$roadCrossings

dataMatrix <- cbind(roadCrossings, distanceWoodland)
dataMatrix <- cbind(dataMatrix, distanceHedges)
dataMatrix <- as.data.frame(dataMatrix)
dataMatrix <- na.omit(dataMatrix)
dataMatrix$step_id_ <- 4
dataMatrix$sl_ <- mean(newData$sl_)
dataMatrix$log_sl <- mean(newData$log_sl)
dataMatrix$ta_ <- mean(newData$ta_)
dataMatrix$cos_ta <- mean(newData$cos_ta)
# land use columns
dataMatrix <- cbind(dataMatrix,
      newData[,(27:35)])
# dummy ID as we will ignore the random effect for this
dataMatrix$id <- 1

# extract the formula used
textForm <- gsub("\n", "", as.character(glmm.TMB.random$call$formula)[3])
textForm <- str_remove(textForm, " \\+ f\\((.*?$)")
textForm <- paste0("~ ", textForm)

# create the model matrix using the new data
modelMatriX_trim <- model.matrix(as.formula(textForm),
                                 data = dataMatrix)
# extract the coefs we need from the glmmTMB obj
poisSumm <- summary(glmm.TMB.random)
poisCoefDF <- as.data.frame(poisSumm$coefficients$cond)
poisCoefDF$term <- rownames(poisCoefDF)
poisCoef <- poisCoefDF$Estimate
names(poisCoef) <- poisCoefDF$term

# limit the matrix to just the coefs we care about and remove the random ones
modelMatriX_trim <- as.matrix(modelMatriX_trim)
coef_trim <- poisCoef[names(poisCoef) %in% colnames(modelMatriX_trim)]
# get rid of random effect aspects
modelMatriX_trim <- modelMatriX_trim[,!str_detect(colnames(modelMatriX_trim), "\\|")]

# colnames() bit so that they are ordered the same, so we are applying correct coefs to correct data
predvals <- (modelMatriX_trim %*% coef_trim[colnames(modelMatriX_trim)])[,1]
## Convert to 0-1 scale
dataMatrix$pred <- boot::inv.logit(predvals)

# test plot
ggplot(dataMatrix) +
  geom_smooth(aes(x = distanceWoodland, y = pred)) +
  geom_point(aes(x = distanceWoodland, y = pred, colour = roadCrossings))
