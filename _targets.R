# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
dir.create("modelOutput", showWarnings = FALSE)

# Set target options:
tar_option_set(
  packages = c("tibble",
               "here",
               "dplyr",
               "stringr",
               "sf",
               "ctmm",
               "amt",
               "move",
               "lme4",
               "effects",
               "sjmisc",
               "boot",
               "sp",
               "raster",
               "gdistance",
               "INLA",
               "performance",
               "JuliaCall",
               "ggplot2",
               "ggtext",
               "ggridges",
               "patchwork",
               "terra",
               "tidyterra"), # Packages that your targets need for their tasks.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.

  controller = crew::crew_controller_local(workers = 3, seconds_idle = 60),
  error = "continue",
  #
  format = "qs" # Optionally set the default storage format. qs is fast.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# selectedPatches <- read.csv(file = here::here("data", "GIS data", "abdn_final_patches.csv"))
# selectedPatches <- selectedPatches$Patch_ID

# OPTIONS AND DECISIONS
# RSF
nAvailable <- 10
typeAvialable <- "random"
contourAvialable <- "95%"
rsfFormula <- case_ ~ distanceWoodland + landuse + distanceWoodland:landuse
# SSF
nAvailableSteps <- 10
slDistribution <- "gamma"
taDistribution <- "vonmises"
ssfFormula <- case_ ~ landuse +
  distanceWoodland +
  distanceHedges +
  distanceWoodland:landuse +
  roadCrossings +
  sl_ + log(sl_) + cos(ta_) +
  log(sl_):landuse +
  log(sl_):distanceWoodland +
  strata(step_id_)
# dbbmm
windowSize <- 29 #~ a week
marginSize <- 5 #~ a day
locationError <- 0.1

# connectivity passage settings
# connectSettings <- expand.grid(
#   THETA = c(0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001),
#   repeatsPerPair = 1,
#   patchDistance = c(1200)
# )
# patchDistance <- 250
connectSettings <- expand.grid(
  THETA = c(0.1, 0.001, 0.00001),
  repeatsPerPair = 1
)

aggFact <- 20
## derivied from the selected Aberdeen patch data
minPatchSize_m2 <- 170000

buffers <- c(0, 50, 100, 200, 500)

# Replace the target list below with your own:
coreTargetList <- list(
  tar_target(
    name = tar_deerData,
    command = read_deer_data()
  ),
  tar_target(
    name = tar_patchList,
    command = read_patches_data()
  ),
  tar_target(
    name = tar_landuseList,
    command = read_landuse_data(tar_deerData, tar_patchList, prelimAggFact = aggFact)
  ),
  tar_target(
    name = tar_trackingTables,
    command = generate_tracking_table(tar_deerData)
  ),
  tar_target(
    name = tar_trackingPlots,
    command = generate_tracking_plots(tar_deerData)
  ),
  # tar_target(
  #   name = tar_studyMaps,
  #   command = generate_study_maps(tar_deerData, tar_patchList, tar_landuseList)
  # )
  tar_target(
    name = tar_akdeLists,
    command = calculate_akdes(tar_deerData)
  ),
  tar_target(
    name = tar_akdeSummary,
    command = extract_akde_summaries(tar_deerData, tar_akdeLists)
  ),
  tar_target(
    name = tar_dbbmmList,
    command = calculate_dbbmms(tar_deerData, tar_landuseList,
                               window = windowSize,
                               margin = marginSize,
                               locationError = locationError)
  ),
  tar_target(
    name = tar_homeRange_sizePlot,
    command = plot_homeRange_sizes(tar_deerData, tar_akdeLists)
  ),
  tar_target(
    name = tar_overview_maps,
    command = generate_overview_maps(tar_deerData, tar_akdeLists, tar_landuseList, tar_patchList)
  ),
  # tar_target(
  #   name = tar_rsf_data,
  #   command = prepare_rsf_data(tar_deerData, tar_akdeLists, tar_landuseList, tar_patchList,
  #                              nAvail = nAvailable, typeAvial = typeAvialable, conAvail = contourAvialable)
  # ),
  # tar_target(
  #   name = tar_rsf_models,
  #   command = run_rsf_models(tar_rsf_data, rsfFormula = rsfFormula)
  # ),
  # tar_target(
  #   name = tar_rsf_outputs,
  #   command = extract_rsf_results(tar_deerData, tar_rsf_data, tar_rsf_models, nAvail = nAvailable)
  # ),
  tar_target(
    name = tar_ssf_data,
    command = prepare_ssf_data(tar_deerData, tar_landuseList, tar_patchList,
                               nAvail = nAvailable, slDist = slDistribution,
                               taDist = taDistribution)
  ),
  tar_target(
    name = tar_ssf_models,
    command = run_ssf_models(tar_ssf_data, ssfFormula = ssfFormula)
  ),
  tar_target(
    name = tar_ssf_plots,
    command = plot_ssf_coefs(tar_deerData, tar_ssf_models, REGION = "Aberdeenshire")
  ),
  tar_target(
    name = tar_predSSFResist_location,
    command = build_predResistance_layer(tar_ssf_data, tar_ssf_models,
                                         tar_landuseList, tar_patchList,
                                         tar_deerData, REGION = "Aberdeenshire", prelimAggFact = aggFact)
  ),
  tar_target(
    name = tar_pois_model,
    command = run_pois_model(tar_ssf_data)
  ),
  tar_target(
    name = tar_pois_plot,
    command = plot_pois_coefs(tar_pois_model)
  ),
  tar_target(
    name = tar_predPoisResist_location,
    command = build_predResistance_layer(tar_ssf_data, tar_pois_model,
                                         tar_landuseList, tar_patchList,
                                         tar_deerData, REGION = "Aberdeenshire", prelimAggFact = aggFact)
  ),
  tar_map(
    values = connectSettings,
    tar_target(
      name = tar_connectSSF_location,
      command = build_connect_layer(tar_predSSFResist_location, tar_patchList,
                                    tar_akdeSummary,
                                    REGION = "Aberdeenshire", prelimAggFact = aggFact,
                                    seed = 2025, THETA = THETA, repeatsPerPair = repeatsPerPair,
                                    MINPATCHSIZE = minPatchSize_m2)
    ),
    tar_target(
      name = tar_connectStanSSF_location,
      command = standardise_connect_layer(tar_connectSSF_location,
                                          REGION = "Aberdeenshire",
                                          THETA = THETA)
    ),
    tar_target(
      name = tar_connectSSF_dbbmmmse,
      command = calculate_dbbmm_mse(tar_deerData,
                                    tar_dbbmmList,
                                    tar_connectStanSSF_location,
                                    REGION = "Aberdeenshire",
                                    THETA = THETA)
    )
  ),
  tar_map(
    values = connectSettings,
    tar_target(
      name = tar_connectPois_location,
      command = build_connect_layer(tar_predPoisResist_location, tar_patchList,
                                    tar_akdeSummary,
                                    REGION = "Aberdeenshire", prelimAggFact = aggFact,
                                    seed = 2025, THETA = THETA, repeatsPerPair = repeatsPerPair,
                                    MINPATCHSIZE = minPatchSize_m2)
    ),
    tar_target(
      name = tar_connectStanPois_location,
      command = standardise_connect_layer(tar_connectPois_location,
                                          REGION = "Aberdeenshire",
                                          THETA = THETA)
    ),
    tar_target(
      name = tar_connectPois_dbbmmmse,
      command = calculate_dbbmm_mse(tar_deerData,
                                    tar_dbbmmList,
                                    tar_connectStanPois_location,
                                    REGION = "Aberdeenshire",
                                    THETA = THETA)
    )
  )
  # tar_target(
  #   name = tar_circuitscape_data,
  #   command = prepare_circuitscape_data(tar_predPoisResist_location, tar_patchList, REGION = "Aberdeenshire",
  #                                       prelimAggFact = aggFact,
  #                                       patchDistance = patchDistance)
  # ),
  # tar_target(
  #   name = tar_circuitscape_files,
  #   command = run_julia_circuitscape(model = "pois", tar_circuitscape_data)
  # )
)

connectTargetList <- list(
  tar_combine(
    tar_connectSSF_list,
    coreTargetList[[18]][grep("tar_connectStanSSF_location", names(coreTargetList[[18]]))],
    command = list(!!!.x)
  ),
  tar_combine(
    tar_mseSSF_df,
    coreTargetList[[18]][grep("SSF_dbbmmmse", names(coreTargetList[[18]]))],
    command = rbind(!!!.x)
  ),
  tar_target(
    tar_connectivitySSF_thetaMaps,
    map_connectivity_thetas(tar_connectSSF_list, tar_landuseList, tar_patchList, REGION = "Aberdeenshire")
  ),
  tar_target(
    tar_patchSSF_plot,
    plot_patch_connectivity(tar_mseSSF_df, tar_connectSSF_list, tar_patchList, REGION = "Aberdeenshire")
  ),
  tar_combine(
    tar_connectPois_list,
    coreTargetList[[19]][grep("tar_connectStanPois_location", names(coreTargetList[[19]]))],
    command = list(!!!.x)
  ),
  tar_combine(
    tar_msePois_df,
    coreTargetList[[19]][grep("Pois_dbbmmmse", names(coreTargetList[[19]]))],
    command = rbind(!!!.x)
  ),
  tar_target(
    tar_connectivityPois_thetaMaps,
    map_connectivity_thetas(tar_connectPois_list, tar_landuseList, tar_patchList, REGION = "Aberdeenshire")
  ),
  tar_target(
    tar_patchPois_plot,
    plot_patch_connectivity(tar_msePois_df, tar_connectPois_list, tar_patchList, REGION = "Aberdeenshire")
  ),
  tar_target(
    tar_patch_summaryPois,
    summarise_patch_connectivity(tar_msePois_df, tar_connectPois_list, tar_patchList, REGION = "Aberdeenshire",
                                 buffers = buffers)
  ),
  tar_target(
    tar_patchPois_summaryPlot,
    plot_patch_summary(tar_patch_summaryPois, tar_msePois_df, tar_connectPois_list, tar_patchList, REGION = "Aberdeenshire")
  ),
  tar_target(
    name = tar_predPoisResist_locationWessex,
    command = build_predResistance_layer(tar_ssf_data, tar_pois_model,
                                         tar_landuseList, tar_patchList,
                                         tar_deerData, REGION = "Wessex", prelimAggFact = aggFact)
  ),
  tar_target(
    name = tar_connectPois_locationWessex,
    command = build_connect_layer(tar_predPoisResist_locationWessex, tar_patchList,
                                  tar_akdeSummary,
                                  REGION = "Wessex", prelimAggFact = aggFact,
                                  seed = 2025, repeatsPerPair = NA,
                                  MSEdf = tar_msePois_df)
  ),
  tar_target(
    name = tar_validation_data,
    command = extract_connectivity_locations(connectRasterLocations = tar_connectPois_list,
                                             connectRasterLocationWessex = tar_connectPois_locationWessex,
                                             akdeLists = tar_akdeLists,
                                             deerData = tar_deerData,
                                             MSEdf = tar_msePois_df,
                                             nAvail = nAvailable,
                                             conAvail = contourAvialable,
                                             typeAvial = typeAvialable,
                                             seed = 2025)
  ),
  tar_target(
    name = tar_connectivityValue_models,
    command = test_validation_data(tar_validation_data)
  ),
  tar_target(
    name = tar_connectivityValue_plots,
    command = plot_connectivity_distributions(tar_validation_data, tar_connectivityValue_models)
  )
)

list(coreTargetList,
     connectTargetList)
