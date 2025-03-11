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
  # targets::tar_source()
  occGBIFdata <- read.csv(here("data", "GBIF data", "rodentiaUK.csv"),
                          sep = "\t")

  rodentSpp <- c("Myodes glareolus", "Apodemus sylvaticus", "Sciurus carolinensis", "Sciurus vulgaris")
  rodentList <- vector("list", length(rodentSpp))
  names(rodentList) <- rodentSpp
  for(sp in rodentSpp){
    # sp <- rodentSpp[1]
    occGBIFdataSP <- occGBIFdata %>%
      filter(species == sp)

    occNBNdataSP <- read.csv(here("data", "NBN Atlas data", sp,
                                  paste0(sub(" ", "-", sp), "-2025-03-10.csv"))) %>%
      filter(!Basis.of.record == "PreservedSpecimen") %>%
      filter(Coordinate.uncertainty..m. <= 25)

    occDataSP <- rbind(occGBIFdataSP %>%
                       dplyr::select(species, decimalLatitude, decimalLongitude),
                     occNBNdataSP %>%
                       dplyr::select(species = Scientific.name,
                                     decimalLatitude = Latitude..WGS84.,
                                     decimalLongitude = Longitude..WGS84.))

    flags <- clean_coordinates(x = occDataSP,
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

    occDataSP <- occDataSP[flags$.summary,]
    rodentList[[sp]] <- occDataSP
  }
  occData <- do.call(rbind, rodentList)

  occDataSF <- st_as_sf(occData,
                        coords = c("decimalLongitude", "decimalLatitude"),
                        crs = 4326,
                        remove = FALSE) %>%
    st_transform(27700)

  # crop to GB area only - exclude NI
  geodata::gadm(country = "GB",
                path = here("data", "GIS data"),
                level = 2,
                version = "latest")
  gbGADM <- readRDS(here("data", "GIS data", "gadm", "gadm41_GBR_2_pk.rds"))
  gbGADM <- st_as_sf(gbGADM)
  gbGADM <- st_transform(gbGADM, 27700)
  gbOnly <- gbGADM %>%
    filter(!GID_1 == "GBR.2_1")
  gbUnion <- st_union(gbOnly)
  intersects <- st_intersects(occDataSF, gbUnion, sparse = FALSE)

  occDataGB <- occDataSF[intersects,]

  ggplot() +
    geom_sf(data = occDataGB)

  sdmStack <- read_stack_layers(layerLoc = here("data", "GIS data", "SDM Layers"))

  xyResults <- terra::extract(sdmStack$distanceWoodland, occDataGB,
                              cells = TRUE, bind = TRUE)

  occDataCells <- st_as_sf(xyResults)

  occDataDeDup <- occDataCells %>%
    filter(!duplicated(cell)) %>%
    dplyr::select(-distanceWoodland, -cell)

  # occDataWessex <- st_crop(occDataSF, st_bbox(terra::rast(sdmLayers$distanceWoodlandWessexLocation)))

  occDataDeDup$resp <- 1

  return(occDataDeDup)

}
