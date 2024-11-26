#' Generate summary of tracking data
#'
#' @name generate_tracking_table
#' @description Simple summary of tracking frequency and duration.
#' @return A dataframe of summarised tracking data. Also exports a csv table.
#'
#' @export
generate_tracking_table <- function(deerData){

  trackingSummary <- deerData %>%
    group_by(Animal_ID) %>%
    summarise(
      durationDays = round(as.numeric(max(datetime) - min(datetime)), digits = 2),
      nFixes = n(),
      fixesPerDay = round(n() / durationDays, digits = 2),
      meanTimeLagHours = paste0(round(mean(timelag/60/60, na.rm = TRUE), digits = 2),
                                " ±", round(sd(timelag/60/60, na.rm = TRUE), digits = 2)))

  write.csv(trackingSummary, here("tables", "trackingSummary.csv"), row.names = FALSE)

  return(trackingSummary)

}


