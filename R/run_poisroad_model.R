#' Run Poisson model for road crossing selection
#'
#' @name run_poisroad_model
#' @description Run the Muff et al poisson model.
#' @return The INLA output.
#'
#' @export
run_poisroad_model <- function(ssfDataList){
  # targets::tar_load("tar_ssfroad_data")
  # library(dplyr)
  # library(here)
  # library(sjmisc)
  # library(stringr)
  # library(INLA)
  # ssfDataList <- tar_ssfroad_data
  # tempData <- lapply(ssfDataList, function(x){
  #   x$steps
  # })
  # tempData <- do.call(rbind, tempData)
  # tempData <- tempData %>%
  #   mutate(roadTypeCrossing =
  #            case_when(
  #              str_detect(roadSize, "A roads") ~ "A roads",
  #              str_detect(roadSize, "B roads") ~ "B roads",
  #              str_detect(roadSize, "C roads") ~ "C roads",
  #              TRUE ~ "No crossing"
  #            ))
  # tempData %>%
  #   select(roadSize, roadTypeCrossing)


  inlaFormula <- y ~ -1 +
    distanceWoodland +
    # Other +
    A_roads +
    B_roads +
    C_roads +
    # No_crossing +
    # step and turn
    sl_ +
    log_sl +
    cos_ta +
    # step interactions
    # sl_:Evergreen.Needleleaf.Forest +
    # sl_:Cropland +
    # sl_:Tall.Grassland +
    # sl_:Short.Grassland +
    # sl_:Open.Shrubland +
    # sl_:Permanent.Wetland +
    # sl_:Human.Settlements +
    # log step interactions
    # log_sl:Evergreen.Needleleaf.Forest +
    # log_sl:Cropland +
    # log_sl:Tall.Grassland +
    # log_sl:Short.Grassland +
    # log_sl:Open.Shrubland +
    # log_sl:Permanent.Wetland +
    # log_sl:Human.Settlements +
    # random effects
    f(step_id, model="iid", hyper = list(theta = list(initial = log(1e-6), fixed = TRUE))) +
    f(id1, distanceWoodland, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    # f(id4, A_roads, values = 1:length(unique(poisModelData$id)), model="iid",
    #   hyper = list(theta = list(initial = log(1), fixed = FALSE,
    #                             prior = "pc.prec", param = c(3, 0.05)))) +
    # f(id5, B_roads, values = 1:length(unique(poisModelData$id)), model="iid",
    #   hyper = list(theta = list(initial = log(1), fixed = FALSE,
    #                             prior = "pc.prec", param = c(3, 0.05)))) +
    # f(id6, C_roads, values = 1:length(unique(poisModelData$id)), model="iid",
    #   hyper = list(theta = list(initial = log(1), fixed = FALSE,
    #                             prior = "pc.prec", param = c(3, 0.05)))) +
    # f(id7, No_crossing, values = 1:length(unique(poisModelData$id)), model="iid",
    #   hyper = list(theta = list(initial = log(1), fixed = FALSE,
    #                             prior = "pc.prec", param = c(3, 0.05)))) +
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
    mutate(crossings = as.numeric(crossings)) %>%
    mutate(roadTypeCrossing =
             case_when(
               str_detect(roadSize, "A roads") ~ "A_roads",
               str_detect(roadSize, "B roads") ~ "B_roads",
               str_detect(roadSize, "C roads") ~ "C_roads",
               TRUE ~ "No_crossing"
             ))

  table(poisModelData$roadTypeCrossing)
  dummyVars <- poisModelData %>%
    mutate(roadTypeCrossing = as.factor(roadTypeCrossing)) %>%
    to_dummy(roadTypeCrossing, suffix = "label") %>%
    rename(
      A_roads = roadTypeCrossing_A_roads,
      B_roads = roadTypeCrossing_B_roads,
      C_roads = roadTypeCrossing_C_roads,
      No_crossing = roadTypeCrossing_No_crossing
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
