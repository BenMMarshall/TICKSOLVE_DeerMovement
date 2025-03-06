#' Read HR database and summarise
#'
#' @name determine_rodent_HR
#' @description a
#' @return A value and two tables summarising studies effort
#'
#' @export
determine_rodent_HR <- function(){

  hrData <- read.csv(here("data", "Home Range Database", "HomeRangeData_2023_09_06.csv"))

  rodentHR <- hrData %>%
    filter(Species %in% occDataWessex$species)

  table(rodentHR$HR_Level)
  # Individual
  # Population (mean)
  # Population (median)

  rodentHR %>%
    filter(HR_Level == "Individual") %>%
    group_by(Species) %>%
    count()

  rodentIndiMeans <- rodentHR %>%
    filter(HR_Level == "Individual") %>%
    group_by(Study_ID, Species) %>%
    summarise(meanHR = mean(Home_Range_km2))

  meanRodentHR <- rodentHR %>%
    filter(!HR_Level == "Individual") %>%
    dplyr::select(Study_ID, Species, Home_Range_km2) %>%
    left_join(rodentIndiMeans) %>%
    summarise(meanHR = mean(Home_Range_km2))

  return(
    list(meanRodentHR = meanRodentHR,
         speciesCount_indi = rodentHR %>%
           filter(HR_Level == "Individual") %>%
           group_by(Species) %>%
           count(name = "measureCount_indi"),
         speciesCount_pop = rodentHR %>%
           filter(!HR_Level == "Individual") %>%
           group_by(Species) %>%
           count(name = "measureCount_pop")))

}
