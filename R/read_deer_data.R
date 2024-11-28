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

  # deerData %>%
  #   filter(Name == "T3HS-7351") %>%
  #   mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  #   filter(Date > as.Date("2023-03-30"), Date < as.Date("2023-04-02"))

  deerData <- deerData %>%
    select(-Location, -ID, -X, -X.1, -Temperature, -Speed, -fHDOP, -nActivity, -X.2, -nAlt) %>%
    mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
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
  sfDeer <- st_as_sf(deerData, coords = c("Longitude","Latitude"), remove = FALSE,
                     crs = 4326)
  sfDeer <- st_transform(sfDeer, 27700)

  # sfDeer$Longitude <- deerData$Longitude
  # sfDeer$Latitude <- deerData$Latitude
  sfDeer$x <- sf::st_coordinates(sfDeer)[,1]
  sfDeer$y <- sf::st_coordinates(sfDeer)[,2]
  sfDeer$epsg <- 27700

  # sfDeer <- sfDeer %>%
  #   group_by(Animal_ID) %>%
  #   arrange(datetime) %>%
  #   mutate(
  #     step = sqrt((x - lag(x))^2 + (y - lag(y))^2),
  #     speed_ms = (step)/timelag)

  deerTrackList <- vector("list", length = length(unique(sfDeer$Animal_ID)))
  names(deerTrackList) <- unique(sfDeer$Animal_ID)
  for(id in unique(sfDeer$Animal_ID)){
    # id <- "Roe14_M"
    # id <- "Fallow02_F"
    deerTrack <- make_track(sfDeer %>%
                              filter(Animal_ID == id),
                            .t = "datetime", .x = x, .y = y,
                            crs = 27700, id = Animal_ID,
                            all_cols = TRUE)

    deerTrack <- track_resample(deerTrack, rate = hours(3), tolerance = hours(1))
    # deerTrack %>%
    #   steps()

    deerTrack <- deerTrack %>%
      rename(x = x_, y = y_, datetime = t_) %>%
      dplyr::select(-burst_) %>%
      arrange(datetime) %>%
      mutate(timelag = as.numeric(difftime(datetime, lag(datetime), units = "secs"))) %>%
      mutate(
        step = sqrt((x - lag(x))^2 + (y - lag(y))^2),
        speed_ms = (step)/timelag)

    deerTrackList[[id]] <- deerTrack


  }
  allDeer <- do.call(rbind, deerTrackList)

  deerOUT <- allDeer %>%
    st_drop_geometry()

  # allDeer %>%
  #   filter(id == "Roe14_M") %>%
  #   filter(t_ > as.POSIXct("2023-03-06 12:39:01") - 60*60*6,
  #          t_ < as.POSIXct("2023-03-06 12:39:01") + 60*60*6)
  # allDeer %>%
  #   ggplot() +
  #   geom_point(aes(x = t_, y = Animal_ID, colour = timelag))
  # # Roe14_M remove too soon fix as.POSIXct("2023-03-06 12:39:01")
  # allDeer %>%
  #   filter(Animal_ID == "Roe14_M") %>%
  #   filter(datetime > as.POSIXct("2023-03-06 12:39:01") - 60*60*6,
  #          datetime < as.POSIXct("2023-03-06 12:39:01") + 60*60*6)
  #
  # allDeer %>%
  #   ggplot() +
  #   geom_point(aes(x = datetime, y = Animal_ID, colour = timelag < 12000 & timelag > 10000))
  #
  # fastdeer <- allDeer %>%
  #   filter(Animal_ID == "Roe14_M") %>%
  #   filter(datetime > as.POSIXct("2023-03-05 18:00:01") - 60*60*12,
  #          datetime < as.POSIXct("2023-03-05 18:00:01") + 60*60*12)
  #   filter(speed_ms > 4)

  # allDeer %>%
  #   filter(Animal_ID == "Roe14_M") %>%
  #   ggplot(aes(x = x_, y = y_, colour = speed_ms, shape = timelag < 60*60)) +
  #   geom_path(alpha = 0.25) +
  #   geom_point(alpha = 0.25)
  # allDeer %>%
  #   filter(Animal_ID == "Roe13_F") %>%
  #   ggplot(aes(x = x_, y = y_, colour = speed_ms, shape = timelag < 60*60)) +
  #   geom_path(alpha = 0.25) +
  #   geom_point(alpha = 0.25)
  # allDeer %>%
  #   filter(Animal_ID == "Roe09_M") %>%
  #   ggplot(aes(x = x_, y = y_, colour = speed_ms, shape = timelag < 60*60)) +
  #   geom_path(alpha = 0.25) +
  #   geom_point(alpha = 0.25)
  # allDeer %>%
  #   filter(Animal_ID == "Roe01_F") %>%
  #   ggplot(aes(x = x_, y = y_, colour = speed_ms, shape = timelag < 60*60)) +
  #   geom_path(alpha = 0.25) +
  #   geom_point(alpha = 0.25)

  write.csv(deerOUT, here("data", "deerMovementData_processed.csv"), row.names = FALSE)

  return(deerOUT)

}
