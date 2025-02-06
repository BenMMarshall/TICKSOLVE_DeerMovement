#' Run Poisson model for overall selection
#'
#' @name run_pois_model
#' @description Run the Muff et al poisson model.
#' @return The INLA output.
#'
#' @export
run_pois_model <- function(ssfDataList){
  # targets::tar_load("tar_ssf_data")

  # library(dplyr)
  # library(here)
  # library(sjmisc)
  # library(INLA)
  # ssfDataList <- tar_ssf_data

  inlaFormula <- y ~ -1 +
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
    # random effects
    f(step_id, model="iid", hyper = list(theta = list(initial = log(1e-6), fixed = TRUE))) +
    f(id1, distanceWoodland, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    f(id2, distanceHedges, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    # f(id3, Deciduous.Broadleaf.Forest, values = 1:length(unique(poisModelData$id)), model="iid",
    #   hyper = list(theta = list(initial = log(1), fixed = FALSE,
    #                             prior = "pc.prec", param = c(3, 0.05)))) +
    f(id4, Evergreen.Needleleaf.Forest, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    f(id5, Cropland, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    f(id6, Tall.Grassland, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    f(id7, Short.Grassland, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    f(id8, Open.Shrubland, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    f(id9, Permanent.Wetland, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    f(id10, Human.Settlements, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    # f(id11, Other, values = 1:length(unique(poisModelData$id)), model="iid",
    #   hyper = list(theta = list(initial = log(1), fixed = FALSE,
    #                             prior = "pc.prec", param = c(3, 0.05)))) +
    f(id12, roadCrossings, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    f(id13, sl_, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    f(id14, log_sl, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    f(id15, cos_ta, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05))))

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
  poisModelData$id9 <- poisModelData$id
  poisModelData$id10 <- poisModelData$id
  poisModelData$id11 <- poisModelData$id
  poisModelData$id12 <- poisModelData$id
  poisModelData$id13 <- poisModelData$id
  poisModelData$id14 <- poisModelData$id
  poisModelData$id15 <- poisModelData$id

  # natural model
  inlaOUT <- inla(inlaFormula,
                  family = "Poisson",
                  data = poisModelData, #verbose=TRUE,
                  control.fixed = list(
                    mean = 0,
                    prec = list(default = prec.beta.trls)),
                  control.inla = list(control.vb = list(emergency = 30)))

  # summary(inlaOUT)
  # inlaOUT$summary.fixed
  # inlaOUT$summary.hyperpar

  return(inlaOUT)

}
