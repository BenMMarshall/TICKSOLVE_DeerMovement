#' Read forest patche spatial data
#'
#' @name read_patches_data
#' @description Read in the previously created patch information.
#' @return A two object list, one object for each study location.
#'
#' @export
read_patches_data <- function(){

  patchesWessex <- read_sf(here("data", "GIS data", "patchesWessex.geoJSON"))
  patchesAberdeen <- read_sf(here("data", "GIS data", "patchesAberdeen.geoJSON"))
  selectedPatchesAberdeen <- read_sf(here("data", "GIS data", "patchesAberdeen_selected.geoJSON"))

  metricsAberdeen <- read.csv(here("data", "GIS data", "aberdeen_site_metrics.csv"))

  selectedPatchesAberdeen <- selectedPatchesAberdeen %>%
    left_join(metricsAberdeen %>%
                rename(Ptch_ID = patch.id))

  # min(st_area(selectedPatchesAberdeen))
  # areaPatches <- st_area(patchesAberdeen)
  # areaPatches <- units::set_units(areaPatches, "m^2")
  # patchesAberdeen <- patchesAberdeen %>%
  #   mutate(area_m = as.numeric(areaPatches))
  # patchesAberdeen %>%
  #   filter(area_m > 170000) %>% dim()
  # # units::set_units(units::set_units(65, "ha"), "m^2")
  # patchesAberdeen %>%
  #   ggplot() +
  #   geom_sf(aes(), fill = "grey50") +
  #   geom_sf(data = selectedPatchesAberdeen, aes(fill = as.factor(Ptch_ID))) +
  #   geom_sf(data = patchesAberdeen %>%
  #             filter(area_m > 170000), aes(), fill = NA,
  #           colour = "red") +
  #   theme_bw()

  # patchList <- list("Aberdeen" = patchesAberdeen,
  #                   "Wessex" = patchesWessex)

  # for(pt in names(patchList)){
  #   # pt <- names(patchList)[1]
  #   patches <- patchList[[pt]]
  #
  #   extent_m <- ext(patches)
  #   # will result in a grid that has a 1 m x 1 m res
  #   xRes <- abs(extent_m[1] - extent_m[2])
  #   yRes <- abs(extent_m[3] - extent_m[4])
  #
  #   ggplot() +
  #     geom_sf(data = patches, aes(), fill = "grey25")
  #
  #   template <- rast(vect(patches), nrows = yRes, ncols = xRes)
  #
  #   print("Binary Raster...")
  #   binaryRaster <- rasterize(vect(patches),
  #                             template)
  #
  #
  #   print("Distance Raster...")
  #   distanceRaster <- terra::distance(binaryRaster)
  #
  #   assign(paste0("distance", pt), here("data", "GIS data", paste0("distance", pt, ".tif")))
  #
  #   writeRaster(distanceRaster, filename = here("data", "GIS data", paste0("distance", pt, ".tif")),
  #               overwrite = TRUE,  gdal = c("COMPRESS=LZW"))
  #
  #
  # }

  patchList <- list("Aberdeen" = patchesAberdeen,
                    "Wessex" = patchesWessex,
                    "AberdeenSelected" = selectedPatchesAberdeen
                    # "distanceAberdeen" = distanceAberdeen,
                    # "distanceWessex" = distanceWessex
                    )

  return(patchList)

}
