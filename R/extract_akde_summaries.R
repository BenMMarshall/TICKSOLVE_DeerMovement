#' Extract summary stats from AKDE and complete overall mean for Aberdeen deer
#'
#' @name extract_akde_summaries
#' @description a
#' @return a
#'
#' @export
extract_akde_summaries <- function(deerData, akdeLists){

  # library(dplyr)
  # library(ctmm)
  #
  # targets::tar_load("tar_akdeLists")
  # targets::tar_load("tar_deerData")
  # deerData <- tar_deerData
  # akdeLists <- tar_akdeLists

  allAreas <- do.call(rbind, akdeLists$area) %>%
    left_join(deerData %>%
                group_by(Animal_ID, region, Sex) %>%
                slice_head(n = 1) %>%
                dplyr::select(Animal_ID, region, Sex))

  aberDeer <- allAreas %>%
    filter(region == "Aberdeenshire") %>%
    pull(Animal_ID)

  aberMean <- mean(akdeLists$fits[aberDeer])
  summary(aberMean)

  # plot(akdeLists$vario$Fallow02_F)
  # summary(akdeLists$fits[["Fallow02_F"]])$DOF["area"]
  ESSDF <- do.call(rbind, lapply(names(akdeLists$fits), function(x){
    data.frame(
      Animal_ID = x,
      DOF_area = summary(akdeLists$fits[[x]])$DOF["area"])
  }))
  modelDF <- do.call(rbind, lapply(names(akdeLists$fits), function(x){
    data.frame(
      Animal_ID = x,
      bestModel = summary(akdeLists$fits[[x]])$name)
  }))

  allAKDESummary <- allAreas %>%
    left_join(ESSDF) %>%
    left_join(modelDF)

  write.csv(allAKDESummary,
            file = here::here("tables", "allAKDESummary.csv"), row.names = FALSE)

  # allAKDESummary %>%
  #   filter(region == "Aberdeenshire") %>%
  #   filter(level == 0.95) %>%
  #   summarise(mean95 = mean(est),
  #             min95 = min(est),
  #             max95 = max(est))

  write.csv(allAKDESummary %>%
              filter(region == "Aberdeenshire"),
            file = here::here("tables", "allAKDESummary_Aberdeen.csv"), row.names = FALSE)

  lapply(names(akdeLists$vario), function(x){
    png(filename = here::here("modelOutput", paste0("variogram_", x, ".png")))
    plot(akdeLists$vario[[x]])
    title(x)
    dev.off()
  })

  return(list(allAreas = allAKDESummary,
              ctmmMean = aberMean))
}
