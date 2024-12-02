#' Generate landscape layers from patches etc
#'
#' @name prepare_landscape_layers
#' @description Creates extra landscape layers for models.
#' @return A list of file locations of rasters to read in.
#'
#' @export
prepare_landscape_layers <- function(deerData, patchList){

  # library(sf)
  # library(terra)
  # library(ggplot2)
  #
  # targets::tar_load("tar_deerData")
  # targets::tar_load("tar_patches")
  # patchList <- tar_patches

  for(pt in c("Aberdeen", "Wessex")){

    patches <- patchList[[pt]]

    extent_m <- ext(patches)
    # will result in a grid that has a 1 m x 1 m res
    xRes <- abs(extent_m[1] - extent_m[2])
    yRes <- abs(extent_m[3] - extent_m[4])

    # ggplot() +
    #   geom_sf(data = patches, aes(), fill = "grey25")

    template <- rast(vect(patches), nrows = yRes, ncols = xRes)

    print("Binary Raster...")
    binaryRaster <- rasterize(vect(patches),
                              template)


    print("Distance Raster...")
    distanceRaster <- terra::distance(binaryRaster)

    writeRaster(distanceRaster, filename = here("data", "GIS data", paste0("distance", pt, ".tif")),
                overwrite = TRUE,  gdal = c("COMPRESS=LZW"))

    assign(paste0("distance", pt), distanceRaster)

  }


}
