#' Read deer movement data and metadata and combine
#'
#' @name read_deer_data
#' @description A function to read in raw data and do basic clean up and add converted 27700 coord system.
#' @return A dataframe with all the deer data processed, and an exported processed csv for review.
#'
#' @export
read_deer_data <- function(){

  deerData <- read.csv(here("data", "GPS data", "deer GPScollar ticksolve_cleaned.csv"))
  deerMetaData <- read.csv(here("data", "GPS data", "Deer metadata and schedules2.csv"))

  deerMetaData <- deerMetaData %>%
    select(Animal_ID, Sex, Collar.ID) %>%
    mutate(Animal_ID = paste0(Animal_ID, "_", Sex))

  deerData <- deerData %>%
    select(-Location, -X, -X.1, -Temperature, -Speed, -fHDOP, -nActivity, -X.2, -nAlt) %>%
    mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")) %>%
    group_by(Name) %>%
    arrange(datetime) %>%
    mutate(timelag = as.numeric(difftime(datetime, lag(datetime), units = "secs"))) %>%
    left_join(deerMetaData %>%
                select(Animal_ID, "Name" = Collar.ID, Sex)) %>%
    ungroup()

  deerData$Animal_ID <- gsub("^R", "Roe", deerData$Animal_ID)
  deerData$Animal_ID <- gsub("^F", "Fallow", deerData$Animal_ID)
  deerData$Animal_ID <- str_replace(deerData$Animal_ID, "[:digit:]{1,2}",
              str_pad(str_extract(deerData$Animal_ID, "[:digit:]{1,2}"), width = 2, side = "left", pad = "0"))

  ##########################################
  ### DEER MISALLOCATED TO NEW FOREST WHEN IN ABERDEEN
  ##########################################
  deerData %>%
    filter(region == "New Forest", Latitude > 54)
  ### T3HS-7359
  deerData[deerData$Name == "T3HS-7359",]$region <- "Aberdeenshire"

  ##########################################
  ##########################################

  #### tracking data is in WGS84, patches are in OSGB36
  sfDeer <- st_as_sf(deerData, coords = c("Longitude","Latitude"),
                     crs = 4326)
  sfDeer <- st_transform(sfDeer, 27700)

  sfDeer$x <- sf::st_coordinates(sfDeer)[,1]
  sfDeer$y <- sf::st_coordinates(sfDeer)[,2]
  sfDeer$epsg <- 27700

  sfDeer <- sfDeer %>%
    mutate(
      step = sqrt((x - lag(x))^2 + (y - lag(y))^2),
      speed_ms = (step)/timelag)

  write.csv(sfDeer %>%
              st_drop_geometry(), here("data", "deerMovementData_processed.csv"), row.names = FALSE)

  return(sfDeer %>%
           st_drop_geometry())

}
