build_omniscape_layer <- function(predRasterLoc, patchList, #longestAxisSummary,
                                  blockSize, searchRadius, reRun){

  # targets::tar_load("tar_predPoisResist_fallow")
  # targets::tar_load("tar_patchList")
  # prelimAggFact = 10
  # predRasterLoc <- tar_predPoisResist_fallow
  # patchList <- tar_patchList

  # longestMean <- mean(longestAxisSummary$longestAxisRange_m)

  resistanceTerra <- terra::rast(predRasterLoc)

  focalPatches <- patchList$Wessex

  template <- terra::rast(resistanceTerra)

  binaryRaster <- terra::rasterize(vect(focalPatches %>% mutate(Ptch_ID = as.numeric(Ptch_ID))),
                                   template, field = "Ptch_ID")

  binaryRaster <- binaryRaster %>%
    mutate(Ptch_ID = ifelse(!is.na(Ptch_ID), 1, 0))

  resistanceOmniTerraLoc <- here::here("data", "GIS data", "resistanceOmniWessex.tif")
  sourceOmniTerraLoc <- here::here("data", "GIS data", "sourceOmniWessex.tif")

  # resistanceTerra <- raster::aggregate(resistanceTerra, fact = 10, fun = max)
  # binaryRaster <- raster::aggregate(binaryRaster, fact = 10, fun = max)

  terra::writeRaster(binaryRaster, sourceOmniTerraLoc,
                     overwrite = TRUE)
  terra::writeRaster(resistanceTerra, resistanceOmniTerraLoc,
                     overwrite = TRUE)
  #
  # fileList <- list(
  #   resistanceOmniTerraLoc = resistanceOmniTerraLoc,
  #   sourceOmniTerraLoc = sourceOmniTerraLoc
  # )
  # return(fileList)
  outFolders <- list.files(pattern = "omniscape_output", full.names = TRUE)
  if(reRun == TRUE | length(outFolders) == 0){

    if(length(outFolders) > 0){
      unlink(outFolders, recursive = TRUE)
    }

    resistR <- paste0('resistance, wkt, transform = Omniscape.read_raster("',
                      resistanceOmniTerraLoc, '", Float64)')
    sourceR <- paste0('source, wkt, transform = Omniscape.read_raster("',
                      sourceOmniTerraLoc, '", Float64)')

    configR <- paste0(
      'config = Dict{String, String}(',
      '"resistance_file"', ' => "', resistanceOmniTerraLoc, '",\n',
      '"source_file"', ' => "', sourceOmniTerraLoc, '",\n',
      '"radius"', ' => "', as.numeric(searchRadius), '",\n',
      '"block_size"', ' => "', as.numeric(blockSize), '",\n',
      # other settings in single string
      '"project_name" => "omniscape_output",
    "source_from_resistance" => "false",
    "calc_normalized_current" => "true",
    "calc_flow_potential" => "true",
    "parallelize" => "true",
    "parallel_batch_size" => "20",
    "write_raw_currmap" => "true")'
    )

    JuliaCall::julia_setup()
    JuliaCall::julia_install_package_if_needed("Omniscape")
    JuliaCall::julia_library("Omniscape")

    JuliaCall::julia_command("using Omniscape")
    JuliaCall::julia_command(resistR)
    JuliaCall::julia_command(sourceR)
    JuliaCall::julia_command(configR)

    # iniFile <- here::here("data", "GIS data", "omniscape", "omniscapeSettings.ini")
    # JuliaCall::julia_command('resistance, wkt, transform = Omniscape.read_raster("F:/Projects/TICKSOLVE_DeerMovement/data/GIS data/resistanceOmniWessex.tif", Float64)')
    # JuliaCall::julia_command('source, wkt, transform = Omniscape.read_raster("F:/Projects/TICKSOLVE_DeerMovement/data/GIS data/sourceOmniWessex.tif", Float64)')
    # JuliaCall::julia_command('config = Dict{String, String}(
    #   "resistance_file" => "F:/Projects/TICKSOLVE_DeerMovement/data/GIS data/resistanceOmniWessex.tif",
    #   "source_file" => "F:/Projects/TICKSOLVE_DeerMovement/data/GIS data/sourceOmniWessex.tif",
    #   "radius" => "250",
    #   "block_size" => "9",
    #   "project_name" => "omniscape_output",
    #   "source_from_resistance" => "false",
    #   "calc_normalized_current" => "true",
    #   "calc_flow_potential" => "true",
    #   "parallelize" => "true",
    #   "parallel_batch_size" => "20",
    #   "write_raw_currmap" => "true")')
    JuliaCall::julia_command('currmap, flow_pot, norm_current = run_omniscape(config,
                                                resistance,
                                                source_strength = source,
                                                wkt = wkt,
                                                geotransform = transform,
                                                write_outputs = true)')

  }

  omniscapeLocations <- list.files(here::here("omniscape_output"),
                                   pattern = "*?.tif$",
                                   full.names = TRUE)

  return(omniscapeLocations)

}
