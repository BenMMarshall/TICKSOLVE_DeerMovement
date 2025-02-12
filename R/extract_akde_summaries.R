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

  saveRDS(summary(aberMean),
          file = here::here("modelOutput", "aberdeenMeanAKDE.rds"))

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

  # extract longest axis of largest contiguous polygon ----------------------

  # akdeLists <- tar_akdeLists
  # Roe06_F

  longestAxisList <- vector("list", length = length(aberDeer))
  names(longestAxisList) <- aberDeer
  for(id in aberDeer){
    # id <- aberDeer[1]
    sfPolys <- akdeLists$sf[[id]] %>%
      filter(level == "99%", ci == "est")

    sfPolysSplit <- st_cast(sfPolys, "POLYGON") %>%
      mutate(polyID = row_number(),
             area = st_area(.))

    largestArea <- max(st_area(sfPolysSplit))

    corePoly <- sfPolysSplit %>%
      filter(area == largestArea)
    # plot(corePoly)

    corePoints <- corePoly %>%
      st_cast("POINT") %>%
      mutate(pointID = row_number())

    pointDistances <- corePoints %>%
      st_distance()

    # corePoints %>%
    #   filter(pointID %in% which(pointDistances == max(pointDistances), arr.ind = TRUE)[1,])

    # ggplot() +
    #   geom_sf(data = sfPolys %>% filter(level == "99%", ci == "est"), fill = "grey75") +
    #   geom_sf(data = corePoly, fill = "grey25") +
    #   geom_sf(data = corePoints %>%
    #             filter(pointID %in% which(pointDistances == max(pointDistances), arr.ind = TRUE)[1,]),
    #           colour = "grey5")

    longestAxisList[[id]] <- data.frame(
      Animal_ID = id,
      longestAxisRange_m = units::set_units(max(pointDistances), "m")
    )

  }
  longestAxisSummary <- do.call(rbind, longestAxisList)

  write.csv(longestAxisSummary,
            file = here::here("tables", "longestAxisSummary.csv"), row.names = FALSE)

  return(list(allAreas = allAKDESummary,
              ctmmMean = aberMean,
              longestAxisSummary = longestAxisSummary))
}
