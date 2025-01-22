#' Run logisitic regressions on real data to compare to connectivity values
#'
#' @name test_validation_data
#' @description abc
#' @return abc
#'
#' @export
test_validation_data <- function(
  validData){

  # library(here)
  # library(dplyr)
  # library(stringr)
  # library(lme4)
  #
  # targets::tar_load("tar_validation_data")
  #
  # validData <- tar_validation_data

  modelDataAber <- validData %>%
    filter(region == "Aberdeenshire") %>%
    filter(area %in% c("knownlocations", "landscape")) %>%
    rename(case = case_)
  modelDataWess <- validData %>%
    filter(region == "Wessex") %>%
    filter(area %in% c("knownlocations", "landscape")) %>%
    rename(case = case_)

  # glmFit <- glmer(case ~ connectivity + (1|region/id),
  #                 data = modelData, family = binomial)
  glmFitAber <- glmer(case ~ connectivity + (1|id),
                      data = modelDataAber, family = binomial)

  summary(glmFitAber)

  qqnorm(resid(glmFitAber))
  qqline(resid(glmFitAber))

  glmFitWess <- glmer(case ~ connectivity + (1|id),
                      data = modelDataWess, family = binomial)

  summary(glmFitWess)

  # library(brms)
  #
  # brmsFit <- brm(case ~ connectivity + (1|id),
  #                data = modelData, family = bernoulli,
  #                iter = 1000, warmup = 250, thin = 1,
  #                chains = 4)
  #
  # summary(brmsFit)

  return(list(glmFitAber = glmFitAber, glmFitWess = glmFitWess))

}
