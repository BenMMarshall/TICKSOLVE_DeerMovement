#' Read rodent occ data
#'
#' @name read_cleanRodent_occData
#' @description Read in a clean occ data for UK rodents
#' @return A sf occ data
#'
#' @export
read_cleanRodent_occData <- function(sdmLayers){
  # targets::tar_load("tar_sdm_layers")
  # sdmLayers <- tar_sdm_layers
  occGBIFdata <- read.csv(here("data", "GBIF data", "rodentiaUK.csv"),
                          sep = "\t")

  occNBNdata <- read.csv(here("data", "NBN Atlas data", "Rodents",
                              "rodents-2025-02-25.csv")) %>%
    filter(!basisOfRecord == "PreservedSpecimen") %>%
    filter(coordinateUncertaintyInMeters <= 25)

  occData <- rbind(occGBIFdata %>%
                     dplyr::select(species, decimalLatitude, decimalLongitude),
                   occNBNdata %>%
                     dplyr::select(species = scientificName,
                                   decimalLatitude,
                                   decimalLongitude))

  flags <- clean_coordinates(x = occData,
                             lon = "decimalLongitude",
                             lat = "decimalLatitude",
                             species = "species",
                             tests = c("capitals", "centroids",
                                       "equal", "zeros",
                                       "outliers"),
                             capitals_rad = 10000,
                             centroids_rad = 1000,
                             outliers_mtp = 5,
                             outliers_method = "quantile",
                             outliers_size = 7)

  flags[!flags$.summary,]
  summary(flags)
  plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

  occData <- occData[flags$.summary,]

  occDataSF <- st_as_sf(occData,
                        coords = c("decimalLongitude", "decimalLatitude"),
                        crs = 4326,
                        remove = FALSE) %>%
    st_transform(27700)

  occDataWessex <- st_crop(occDataSF, st_bbox(terra::rast(sdmLayers$distanceWoodlandWessexLocation)))

  occDataWessex$resp <- 1

  return(occDataWessex)

}
