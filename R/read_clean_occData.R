#' Read fallow occ data
#'
#' @name read_clean_occData
#' @description Read in a clean occ data for UK fallow
#' @return A sf occ data
#'
#' @export
read_clean_occData <- function(sdmLayers){
  # targets::tar_load("tar_sdm_layers")
  # sdmLayers <- tar_sdm_layers
  occData <- read.csv(here("data", "GBIF data", "fallowUK.csv"),
                      sep = "\t")

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
