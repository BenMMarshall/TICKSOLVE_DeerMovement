#' Read fallow occ data
#'
#' @name read_cleanFallow_occData
#' @description Read in a clean occ data for UK fallow
#' @return A sf occ data
#'
#' @export
read_cleanFallow_occData <- function(sdmLayers){
  # targets::tar_load("tar_sdm_layers")
  # sdmLayers <- tar_sdm_layers
  occGBIFdata <- read.csv(here("data", "GBIF data", "fallowUK.csv"),
                      sep = "\t")

  occNBNdata <- read.csv(here("data", "NBN Atlas data", "Dama dama",
                           "damadama-2025-02-23.csv")) %>%
    filter(!Basis.of.record == "PreservedSpecimen") %>%
    filter(Coordinate.uncertainty..m. <= 25)

  occData <- rbind(occGBIFdata %>%
                     dplyr::select(species, decimalLatitude, decimalLongitude),
                   occNBNdata %>%
                     dplyr::select(species = Scientific.name, decimalLatitude = Latitude..WGS84., decimalLongitude = Longitude..WGS84.))

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

  # crop to GB area only - exclude NI
  gbGADM <- readRDS(here("data", "GIS data", "gadm", "gadm41_GBR_2_pk.rds"))
  gbGADM <- st_as_sf(gbGADM)
  gbGADM <- st_transform(gbGADM, 27700)
  gbOnly <- gbGADM %>%
    filter(!GID_1 == "GBR.2_1")
  gbUnion <- st_union(gbOnly)
  intersects <- st_intersects(occDataSF, gbUnion, sparse = FALSE)

  occDataGB <- occDataSF[intersects,]

  # occDataWessex <- st_crop(occDataSF, st_bbox(terra::rast(sdmLayers$distanceWoodlandWessexLocation)))

  occDataGB$resp <- 1

  return(occDataGB)

}
