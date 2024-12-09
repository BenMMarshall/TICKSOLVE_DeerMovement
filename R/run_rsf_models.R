#' Run basic RSFs
#'
#' @name run_rsf_models
#' @description Run basic RSF using glmer.
#' @return A list of model outputs from the glmer function.
#'
#' @export
run_rsf_models <- function(rsfDataList, rsfFormula){

  rsfModelList <- vector("list", length = length(rsfDataList))
  names(rsfModelList) <- names(rsfDataList)
  for(id in names(rsfDataList)){
    # id <- names(rsfDataList)[1]
    focalRsfData <- rbind(rsfDataList[[id]]$usedDeer %>%
            st_drop_geometry() %>%
            select(x, y, Animal_ID, distancePatch, landuse, case_),
          rsfDataList[[id]]$availPoints %>%
            select(x, y, Animal_ID, distancePatch, landuse, case_))

    form <- case_ ~ distancePatch + landuse + distancePatch:landuse
    # form <- rsfFormula

    # rsfOUT <- glmer(form,
    rsfOUT <- glm(form,
                  family = binomial(),
                  data = focalRsfData)
                  # nAGQ = 0) # nAGQ set to zero to speed up process, in glmer

    rsfModelList[[id]] <- rsfOUT
  }

  return(rsfModelList)

}
