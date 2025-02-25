#' Extract longest Axis Summary for fallow deer
#'
#' @name extract_akdeFallow_longest
#' @description a
#' @return a
#'
#' @export
extract_akdeFallow_longest <- function(akdeLists){

  # library(stringr)
  # library(sf)
  # library(dplyr)
  #
  # tar_akdeLists$sf$Fallow02_F %>%
  #   filter(level = )
  #
  # tar_akdeLists$sf$Fallow07_F

  # akdeLists <- tar_akdeLists

  fallowIDs <- names(akdeLists$sf)[str_detect(names(akdeLists$sf), "Fallow")]

  longestAxisList <- vector("list", length = length(fallowIDs))
  names(longestAxisList) <- fallowIDs
  for(id in fallowIDs){
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

  return(longestAxisSummary)

}
