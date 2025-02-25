#' Run Poisson model for overall selection
#'
#' @name run_poisFallow_model
#' @description Run the Muff et al poisson model.
#' @return The INLA output.
#'
#' @export
run_poisFallow_model <- function(ssfDataList){
  # targets::tar_load("tar_ssf_data")

  # library(dplyr)
  # library(here)
  # library(sjmisc)
  # library(INLA)
  # ssfDataList <- tar_ssf_data

  inlaFormula <- y ~ -1 +
    # Other +
    roadCrossings +
    # step and turn
    sl_ +
    log_sl +
    cos_ta +
    # random effects
    f(step_id, model="iid", hyper = list(theta = list(initial = log(1e-6), fixed = TRUE))) +
    f(id1, roadCrossings, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    f(id2, sl_, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    f(id3, log_sl, values = 1:length(unique(poisModelData$id)), model="iid",
      hyper = list(theta = list(initial = log(1), fixed = FALSE,
                                prior = "pc.prec", param = c(3, 0.05)))) +
    f(id4, cos_ta, values = 1:length(unique(poisModelData$id)), model="iid",
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

  # We can run the INLA model using the priors and set-up from Muff et al.
  # Precision for the priors of slope coefficients
  prec.beta.trls <- 1e-4

  poisModelData$id1 <- poisModelData$id
  poisModelData$id2 <- poisModelData$id
  poisModelData$id3 <- poisModelData$id
  poisModelData$id4 <- poisModelData$id

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
