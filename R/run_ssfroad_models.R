#' Run iSSFs
#'
#' @name run_ssfroad_models
#' @description Run ssf using amt package
#' @return A list of model outputs.
#'
#' @export
run_ssfroad_models <- function(ssfDataList){

  # targets::tar_load("tar_ssfroad_data")
  # ssfDataList <- tar_ssfroad_data

  ssfData <- do.call(rbind, lapply(ssfDataList, function(x){x$steps}))
  ssfData <- ssfData %>%
    select(Animal_ID, step_id_, case_,
           landuse, distanceWoodland,
           distanceHedges, roadSize,
           sl_, ta_) %>%
    mutate(roadTypeCrossing =
             case_when(
               str_detect(roadSize, "A roads") ~ "A_roads",
               str_detect(roadSize, "B roads") ~ "B_roads",
               str_detect(roadSize, "C roads") ~ "C_roads",
               TRUE ~ "No_crossing"
             ))

  # dummyLU <- sjmisc::to_dummy(x = ssfData %>%
  #                               select(landuse),
  #                             landuse)
  # names(dummyLU) <- gsub(" ", "_", levels(ssfData$landuse)[-7])
  #
  # ssfData <- ssfData %>%
  #   bind_cols(dummyLU)

  ssfModels <- ssf_ind(id = ssfData$Animal_ID,
                       data = ssfData,
                       form_ls = list(as.formula(case_ ~
                                                   # landuse +
                                                   distanceWoodland +
                                                   # distanceHedges +
                                                   roadTypeCrossing +
                                                   sl_ + log(sl_) + cos(ta_) +
                                                   # sl_:landuse +
                                                   # log(sl_):landuse +
                                                   strata(step_id_))),
                       cleanModel = FALSE, method = "exact")

  return(ssfModels)

}
