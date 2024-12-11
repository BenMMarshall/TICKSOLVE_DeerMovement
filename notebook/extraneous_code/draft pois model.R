targets::tar_load("tar_ssf_data")

library(dplyr)
library(here)
library(sjmisc)
library(INLA)

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

table(poisModelData$landuse)
dummyVars <- poisModelData %>%
  mutate(landuse = as.factor(landuse)) %>%
  to_dummy(landuse) %>%
  rename(
    Woodland = landuse_1,
    Grasslands = landuse_2,
    Heathland = landuse_3,
    Aquatic = landuse_4,
    Arable = landuse_5,
    Human.Settlements = landuse_6
  )
apply(dummyVars, 2, sum)

poisModelData <- cbind(poisModelData, dummyVars)

# We can run the INLA model using the priors and set-up from Muff et al.
# Precision for the priors of slope coefficients
prec.beta.trls <- 1e-4

poisModelData$id1 <- poisModelData$id
poisModelData$id2 <- poisModelData$id
poisModelData$id3 <- poisModelData$id
poisModelData$id4 <- poisModelData$id
poisModelData$id5 <- poisModelData$id
poisModelData$id6 <- poisModelData$id
poisModelData$id7 <- poisModelData$id
poisModelData$id8 <- poisModelData$id

inlaFormula <- y ~ -1 +
  distanceWoodland +
  Grasslands +
  Heathland +
  Aquatic +
  Arable +
  Human.Settlements +
  roadCrossings +
  f(step_id, model="iid", hyper = list(theta = list(initial = log(1e-6), fixed = TRUE))) +
  f(id1, Grasslands, values = 1:length(unique(poisModelData$id)), model="iid",
    hyper = list(theta = list(initial = log(1), fixed = FALSE,
                              prior = "pc.prec", param = c(3, 0.05)))) +
  f(id2, Heathland, values = 1:length(unique(poisModelData$id)), model="iid",
    hyper = list(theta = list(initial = log(1), fixed = FALSE,
                              prior = "pc.prec", param = c(3, 0.05)))) +
  f(id3, Aquatic, values = 1:length(unique(poisModelData$id)), model="iid",
    hyper = list(theta = list(initial = log(1), fixed = FALSE,
                              prior = "pc.prec", param = c(3, 0.05)))) +
  f(id4, Arable, values = 1:length(unique(poisModelData$id)), model="iid",
    hyper = list(theta = list(initial = log(1), fixed = FALSE,
                              prior = "pc.prec", param = c(3, 0.05)))) +
  f(id5, Human.Settlements, values = 1:length(unique(poisModelData$id)), model="iid",
    hyper = list(theta = list(initial = log(1), fixed = FALSE,
                              prior = "pc.prec", param = c(3, 0.05)))) +
  f(id6, distanceWoodland, values = 1:length(unique(poisModelData$id)), model="iid",
    hyper = list(theta = list(initial = log(1), fixed = FALSE,
                              prior = "pc.prec", param = c(3, 0.05)))) +
  f(id7, roadCrossings, values = 1:length(unique(poisModelData$id)), model="iid",
    hyper = list(theta = list(initial = log(1), fixed = FALSE,
                              prior = "pc.prec", param = c(3, 0.05))))

# natural model
inlaOUT <- inla(inlaFormula,
       family = "Poisson",
       data = poisModelData, #verbose=TRUE,
       control.fixed = list(
         mean = 0,
         prec = list(default = prec.beta.trls)),
       control.inla = list(control.vb = list(emergency = 30)))

summary(inlaOUT)
inlaOUT$summary.fixed
inlaOUT$summary.hyperpar
