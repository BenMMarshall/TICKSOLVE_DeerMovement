#' Run iSSFs
#'
#' @name run_ssf_models
#' @description Run ssf using amt package
#' @return A list of model outputs.
#'
#' @export
run_ssf_models <- function(allSteps, ssfFormula){

  # targets::tar_load("tar_ssf_data")
  # allSteps <- tar_ssf_data

  ssfModelList <- vector("list", length = length(allSteps))
  names(ssfModelList) <- names(allSteps)
  for(id in names(allSteps)){
    # id <- names(allSteps)[1]
    focalData <- allSteps[[id]]$steps
    form <- ssfFormula
    # form <- ssfFormula

    ssfOUT <- amt::fit_issf(data = focalData,
                            formula = form,
                            model = TRUE)

    ssfModelList[[id]] <- ssfOUT
  }
  return(ssfModelList)

}
