#' Run basic RSFs
#'
#' @name run_rsf_models
#' @description Run basic RSF using glmer.
#' @return A list of model outputs from the glmer function.
#'
#' @export
run_rsf_models <- function(rsfDataList){

  rsfModelList <- vector("list", length = length(rsfDataList))
  names(rsffModelList) <- names(rsfDataList)
  for(id in names(rsfDataList)){

    form <- case_ ~ distance + landUse

    rsfOUT <- glmer(form,
                    family = binomial(),
                    data = rsfData,
                    nAGQ = 0) # nAGQ set to zero to speed up process

    rsfModelList[[id]] <- rsfOUT
  }

}
