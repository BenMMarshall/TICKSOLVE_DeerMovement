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
               "IndRSA",
               "performance",
               "JuliaCall",
               "ggplot2",
               "ggtext",
               "ggridges",
               "ggrepel",
               "rnaturalearth",
               "patchwork",
               "terra",
               "tidyterra",
               "foreach",
               "doParallel",
               "CoordinateCleaner",
               "fasterRaster",
               "biomod2"
  ), # Packages that your targets need for their tasks.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.

  # controller = crew::crew_controller_local(workers = 3, seconds_idle = 60),
  # error = "continue",
  #
  format = "qs" # Optionally set the default storage format. qs is fast.
)

# Run the R scripts in the R/ folder with your custom functions:
targets::tar_source()

# selectedPatches <- read.csv(file = here::here("data", "GIS data", "abdn_final_patches.csv"))
# selectedPatches <- selectedPatches$Patch_ID

# OPTIONS AND DECISIONS
# RSF
nAvailable <- 10
typeAvialable <- "random"
contourAvialable <- "95%"
# rsfFormula <- case_ ~ distanceWoodland + landuse + distanceWoodland:landuse
# SSF
nAvailableSteps <- 10
slDistribution <- "gamma"
taDistribution <- "vonmises"
# ssfFormula <- case_ ~ landuse +
#   distanceWoodland +
#   distanceHedges +
#   roadCrossings +
#   sl_ + log(sl_) + cos(ta_) +
#   sl_:landuse +
#   log(sl_):landuse +
#   strata(step_id_)

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
  repeatsPerPair = 6
)

aggFact <- NA
## derived from the selected Aberdeen patch data
minPatchSize_m2 <- 5000
setCropArea <- 750
useCores <- 12

buffers <- c(0, 750)

inputList <- list(
  nAvailable = nAvailable,
  typeAvialable = typeAvialable,
  nAvailableSteps = nAvailableSteps,
  slDistribution = slDistribution,
  taDistribution = taDistribution,
  windowSize = windowSize,
  marginSize = marginSize,
  locationError = locationError,
  theta = connectSettings$THETA,
  repeatsPerPair = connectSettings$repeatsPerPair,
  minPatchSize_m2 = minPatchSize_m2,
  setCropArea = setCropArea,
  buffers = buffers
)

saveRDS(inputList, file = here::here("data", "inputList.rds"))

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
    name = tar_selectedPatchList,
    command = read_selectedPatches_data()
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
  tar_target(
    name = tar_akdeLists,
    command = calculate_akdes(tar_deerData)
  ),
  tar_target(
    name = tar_akdeSummary,
    command = extract_akde_summaries(tar_deerData, tar_akdeLists)
  ),
  # tar_target(
  #   name = tar_wrsfakde,
  #   command = run_wrsf_models(deerData = tar_deerData,
  #                             akdeLists = tar_akdeLists,
  #                             landuseList = tar_landuseList,
  #                             REGION = "Aberdeenshire",
  #                             error = 0.01)
  # ),
  # tar_target(
  #   name = tar_wrsf_effects,
  #   command = plot_wrsf_effects(tar_wrsfakde)
  # ),
  tar_target(
    name = tar_distancePatch_plot,
    command = plot_distance_to_patch(tar_deerData, tar_patchList, tar_akdeSummary)
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
    command = plot_homeRange_sizes(tar_deerData, tar_akdeLists#, tar_wrsfakde
                                   )
  ),
  tar_target(
    name = tar_variograms,
    command = plot_variograms(tar_akdeLists)
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
                               nAvail = nAvailableSteps, slDist = slDistribution,
                               taDist = taDistribution)
  ),
  tar_target(
    name = tar_pois_model,
    command = run_pois_model(tar_ssf_data)
  ),
  tar_target(
    name = tar_pois_plot,
    command = export_pois_coefs(tar_pois_model)
  ),
  tar_target(
    name = tar_predPoisResist_location,
    command = build_predResistance_layer(tar_ssf_data, tar_pois_model,
                                         tar_landuseList, tar_patchList,
                                         tar_deerData, REGION = "Aberdeenshire", prelimAggFact = aggFact)
  ),
  tar_target(
    name = tar_poisResist_map,
    command = plot_conductance_map(tar_predPoisResist_location, tar_patchList,
                                   REGION = "Aberdeenshire")
  ),
  # tar_map(
  #   values = connectSettings,
  #   tar_target(
  #     name = tar_connectSSF_location,
  #     command = build_connect_layer(tar_predSSFResist_location, tar_patchList,
  #                                   tar_akdeSummary,
  #                                   REGION = "Aberdeenshire", prelimAggFact = aggFact,
  #                                   seed = 2025, THETA = THETA, repeatsPerPair = repeatsPerPair,
  #                                   MINPATCHSIZE = minPatchSize_m2, cropArea = setCropArea, cores = useCores)
  #   ),
  #   tar_target(
  #     name = tar_connectStanSSF_location,
  #     command = standardise_connect_layer(tar_connectSSF_location,
  #                                         REGION = "Aberdeenshire",
  #                                         THETA = THETA)
  #   ),
  #   tar_target(
  #     name = tar_connectSSF_dbbmmmse,
  #     command = calculate_dbbmm_mse(tar_deerData,
  #                                   tar_dbbmmList,
  #                                   tar_connectStanSSF_location,
  #                                   REGION = "Aberdeenshire",
  #                                   THETA = THETA)
  #   )
  # ),
  tar_map(
    values = connectSettings,
    tar_target(
      name = tar_connectPois_location,
      command = build_connect_layer(tar_predPoisResist_location, tar_patchList,
                                    tar_akdeSummary,
                                    REGION = "Aberdeenshire", prelimAggFact = aggFact,
                                    seed = 2025, THETA = THETA, repeatsPerPair = repeatsPerPair,
                                    MINPATCHSIZE = minPatchSize_m2, cropArea = setCropArea, cores = useCores)
    ),
    tar_target(
      name = tar_connectStanPois_location,
      command = standardise_connect_layer(tar_connectPois_location,
                                          REGION = "Aberdeenshire",
                                          THETA = THETA, MSEdf = NULL)
    ),
    tar_target(
      name = tar_connectPois_dbbmmmse,
      command = calculate_dbbmm_mse(tar_deerData,
                                    tar_dbbmmList,
                                    tar_connectStanPois_location,
                                    REGION = "Aberdeenshire",
                                    THETA = THETA)
    )
  ),
  tar_target(
    name = tar_ssf_models,
    command = run_ssf_models(tar_ssf_data)
  ),
  tar_target(
    name = tar_ssf_extracts,
    command = extract_ssf_coefs(tar_ssf_models)
  ),
  tar_target(
    name = tar_ssf_plots,
    command = plot_ssf_coefs(tar_ssf_extracts)
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
  # tar_combine(
  #   tar_connectSSF_list,
  #   coreTargetList[[19]][grep("tar_connectStanSSF_location", names(coreTargetList[[19]]))],
  #   command = list(!!!.x)
  # ),
  # tar_combine(
  #   tar_mseSSF_df,
  #   coreTargetList[[19]][grep("SSF_dbbmmmse", names(coreTargetList[[19]]))],
  #   command = rbind(!!!.x)
  # ),
  # tar_target(
  #   tar_connectivitySSF_thetaMaps,
  #   map_connectivity_thetas(tar_connectSSF_list, tar_landuseList, tar_patchList, REGION = "Aberdeenshire")
  # ),
  # tar_target(
  #   tar_patchSSF_plot,
  #   plot_patch_connectivity(tar_mseSSF_df, tar_connectSSF_list, tar_patchList, REGION = "Aberdeenshire")
  # ),
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
    tar_patchPois_MMMplot,
    plot_patch_MMMconnectivity(tar_msePois_df, tar_connectPois_list, tar_patchList, tar_selectedPatchList, REGION = "Aberdeenshire")
  ),
  tar_target(
    tar_patch_summaryPois_aberdeen,
    extract_patch_connectivity(tar_msePois_df, tar_connectPois_list, tar_patchList, tar_selectedPatchList,
                               REGION = "Aberdeenshire",
                               buffers = buffers)
  ),
  tar_target(
    tar_patchPois_plot,
    plot_patch_connectivity(tar_msePois_df, tar_connectPois_list, tar_patchList, tar_selectedPatchList, REGION = "Aberdeenshire",
                            tar_patch_summaryPois_aberdeen)
  ),
  tar_target(
    tar_funcStruc_plot,
    plot_funcStruc_comparison(tar_patch_summaryPois_aberdeen)
  ),
  # tar_target(
  #   tar_patchPois_summaryPlot,
  #   plot_patch_summary(tar_patch_summaryPois, tar_msePois_df, tar_connectPois_list, tar_patchList, tar_selectedPatchList,
  #                      REGION = "Aberdeenshire")
  # ),
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
                                  seed = 2025, repeatsPerPair = connectSettings$repeatsPerPair[1],
                                  MSEdf = tar_msePois_df,
                                  MINPATCHSIZE = minPatchSize_m2, cropArea = setCropArea, cores = useCores)
  ),
  tar_target(
    name = tar_connectStanPois_locationWessex,
    command = standardise_connect_layer(tar_connectPois_locationWessex,
                                        REGION = "Wessex",
                                        THETA = NULL, MSEdf = tar_msePois_df)
  ),
  tar_target(
    tar_patch_summaryPois_wessex,
    extract_patch_connectivity(tar_msePois_df,
                               tar_connectStanPois_locationWessex,
                               tar_patchList,
                               tar_selectedPatchList,
                               REGION = "Wessex",
                               buffers = buffers)
  ),
  tar_target(
    name = tar_validation_data,
    command = extract_connectivity_locations(connectRasterLocations = tar_connectPois_list,
                                             connectRasterLocationWessex = tar_connectStanPois_locationWessex,
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
  ),
  tar_target(
    name = tar_package_text,
    command = create_package_txt(excludes = c("bestNormalize", "JuliaCall", "recurse", "roxygen2", "suncalc", "survival"))
  ),
  tar_target(
    name = tar_render_movement,
    command = render_rmd(fileIN = here::here("notebook", "manuscript", "deerMovementManuscript.Rmd"),
                         fileOUT = here::here("notebook", "manuscript", "deerMovementManuscript.html"),
                         tar_pois_plot,
                         tar_variograms,
                         tar_homeRange_sizePlot),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    name = tar_location_plot,
    command = plot_study_locations(
      deerData = tar_deerData,
      landuseList = tar_landuseList,
      patchList = tar_patchList
    )
  ),
  tar_target(
    name = tar_render_connectivity,
    command = render_rmd(fileIN = here::here("notebook", "manuscript", "deerConnectivityManuscript.Rmd"),
                         fileOUT = here::here("notebook", "manuscript", "deerConnectivityManuscript.pdf"),
                         tar_location_plot,
                         # tar_patchPois_summaryPlot,
                         tar_connectivityValue_plots,
                         tar_package_text,
                         tar_poisResist_map,
                         tar_connectivityPois_thetaMaps),
    cue = tar_cue(mode = "always")
  )
)

# SDM pipeline ------------------------------------------------------------

aggFact_SDM <- NA
# block size for omniscape. 5000m each side for min patch size. 5000/25 = 200, so perhaps a
# bs of 7 would be suitable. 7x7=49. Would be ~ four sources per smallest patch. - seems too high
bs_fallow <- 31
sr_fallow <- 1500
bs_rodent <- 15
sr_rodent <- 150

coreSDMList <- list(
  tar_target(
    name = tar_sdm_layers,
    command = prepare_sdm_layer(prelimAggFact = aggFact_SDM)
  ),
  # fallow occ ----
  tar_target(
    name = tar_occData_fallow,
    command = read_cleanFallow_occData(tar_sdm_layers)
  ),
  tar_target(
    name = tar_pseudoAbs_fallow,
    command = create_psuedo_abs(tar_occData_fallow,
                                hfBiasLayer = here("data", "Human Footprint", "hfp2022.tif"),
                                nPointMultiplier = 3, nReps = 1,
                                envLayers = read_stack_layers(layerLoc = here("data", "GIS data", "SDM Layers"),
                                                              tar_sdm_layers))
  ),
  # rodent occ ----
  tar_target(
    name = tar_occData_rodent,
    command = read_cleanRodent_occData(tar_sdm_layers)
  ),
  tar_target(
    name = tar_pseudoAbs_rodent,
    command = create_psuedo_abs(tar_occData_rodent,
                                hfBiasLayer = here("data", "Human Footprint", "hfp2022.tif"),
                                nPointMultiplier = 3, nReps = 1,
                                envLayers = read_stack_layers(layerLoc = here("data", "GIS data", "SDM Layers"),
                                                              tar_sdm_layers))
  ),
  # fallow SDM ----
  tar_target(
    name = tar_biomodData_fallow,
    command = BIOMOD_FormatingData(resp.var = tar_pseudoAbs_fallow$sp,
                                   expl.var = tar_pseudoAbs_fallow$env,
                                   resp.xy = tar_pseudoAbs_fallow$xy,
                                   resp.name = "Dama.dama",
                                   #     # advice from biomod2’s team:
                                   # # - random selection of PA when high specificity is valued over high sensitivity
                                   # # - number of PA = 3 times the number of presences
                                   # # - 10 repetitions
                                   # PA.strategy = "random",
                                   # PA.nb.rep = 2,
                                   # PA.nb.absences = 1000,
                                   filter.raster = TRUE,
                                   # PA.nb.rep = 2,
                                   PA.strategy = "user.defined",
                                   PA.user.table = tar_pseudoAbs_fallow$pa.tab
    )
  ),
  tar_target(
    name = tar_biomodModels_fallow,
    command = BIOMOD_Modeling(bm.format = tar_biomodData_fallow,
                              modeling.id = "AllModels",
                              models = c("ANN",
                                         "GBM",
                                         "GLM",
                                         # "MAXNET",
                                         # "XGBOOST",
                                         "RF"
                              ),
                              CV.strategy = "user.defined",
                              CV.user.table = bm_CrossValidation(bm.format = tar_biomodData_fallow,
                                                                 strategy = "strat",
                                                                 k = 4,
                                                                 balance = "presences",
                                                                 strat = "both"),
                              # CV.strategy = "random",
                              # CV.nb.rep = 2,
                              # CV.perc = 0.8,
                              OPT.strategy = "bigboss",
                              var.import = 3,
                              metric.eval = c("TSS","ROC"),
                              seed.val = 2025,
                              nb.cpu = 6)
  ),
  tar_target(
    name = tar_biomodEns_fallow,
    command = BIOMOD_EnsembleModeling(bm.mod = tar_biomodModels_fallow,
                                      models.chosen = "all",
                                      em.by = "all",
                                      em.algo = c("EMmean", "EMcv", "EMci",
                                                  "EMmedian", "EMca",
                                                  "EMwmean"),
                                      metric.select = c("TSS"),
                                      metric.select.thresh = c(0.25),
                                      metric.eval = c("TSS", "ROC"),
                                      var.import = 3,
                                      EMci.alpha = 0.05,
                                      EMwmean.decay = "proportional",
                                      seed.val = 2025,
                                      nb.cpu = 6)
  ),
  tar_target(
    name = tar_biomodForecast_fallow,
    command = BIOMOD_EnsembleForecasting(bm.em = tar_biomodEns_fallow,
                                         proj.name = "CurrentEM_fallow",
                                         new.env = read_stack_layers(layerLoc = here("data", "GIS data", "SDM Layers"),
                                                                     tar_sdm_layers) %>%
                                           crop(tar_patchList$Wessex),
                                         models.chosen = get_built_models(tar_biomodEns_fallow)[
                                           stringr::str_detect(get_built_models(tar_biomodEns_fallow), "EMwmeanByTSS")],
                                         metric.binary = "all",
                                         metric.filter = "all",
                                         output.format = ".tif",
                                         nb.cpu = 6)
  ),
  tar_target(
    name = tar_projLayer_fallow,
    command = save_proj_layer(tar_biomodForecast_fallow)
  ),
  # rodent SDM ----
  tar_target(
    name = tar_biomodData_rodent,
    command = BIOMOD_FormatingData(resp.var = tar_pseudoAbs_rodent$sp,
                                   expl.var = tar_pseudoAbs_rodent$env,
                                   resp.xy = tar_pseudoAbs_rodent$xy,
                                   resp.name = "Rodent",
                                   #     # advice from biomod2’s team:
                                   # # - random selection of PA when high specificity is valued over high sensitivity
                                   # # - number of PA = 3 times the number of presences
                                   # # - 10 repetitions
                                   # PA.strategy = "random",
                                   # PA.nb.rep = 2,
                                   # PA.nb.absences = 1000,
                                   filter.raster = TRUE,
                                   # PA.nb.rep = 2,
                                   PA.strategy = "user.defined",
                                   PA.user.table = tar_pseudoAbs_rodent$pa.tab
    )
  ),
  tar_target(
    name = tar_biomodModels_rodent,
    command = BIOMOD_Modeling(bm.format = tar_biomodData_rodent,
                              modeling.id = "AllModels",
                              models = c("ANN",
                                         "GBM",
                                         "GLM",
                                         # "MAXNET",
                                         # "XGBOOST",
                                         "RF"
                              ),
                              CV.strategy = "user.defined",
                              CV.user.table = bm_CrossValidation(bm.format = tar_biomodData_rodent,
                                                                 strategy = "strat",
                                                                 k = 4,
                                                                 balance = "presences",
                                                                 strat = "both"),
                              # CV.strategy = "random",
                              # CV.nb.rep = 2,
                              # CV.perc = 0.8,
                              OPT.strategy = "bigboss",
                              var.import = 3,
                              metric.eval = c("TSS","ROC"),
                              seed.val = 2025,
                              nb.cpu = 6)
  ),
  tar_target(
    name = tar_biomodEns_rodent,
    command = BIOMOD_EnsembleModeling(bm.mod = tar_biomodModels_rodent,
                                      models.chosen = "all",
                                      em.by = "all",
                                      em.algo = c("EMmean", #"EMcv", "EMci",
                                                  "EMmedian", #"EMca",
                                                  "EMwmean"),
                                      metric.select = c("TSS"),
                                      metric.select.thresh = c(0.25),
                                      metric.eval = c("TSS", "ROC"),
                                      var.import = 3,
                                      EMci.alpha = 0.05,
                                      EMwmean.decay = "proportional",
                                      seed.val = 2025,
                                      nb.cpu = 6)
  ),
  tar_target(
    name = tar_biomodForecast_rodent_wessex,
    command = BIOMOD_EnsembleForecasting(bm.em = tar_biomodEns_rodent,
                                         proj.name = "CurrentEM_rodent_wessex",
                                         new.env = read_stack_layers(layerLoc = here("data", "GIS data", "SDM Layers"),
                                                                     tar_sdm_layers) %>%
                                           crop(tar_patchList$Wessex),
                                         models.chosen = get_built_models(tar_biomodEns_rodent)[
                                           stringr::str_detect(get_built_models(tar_biomodEns_rodent), "EMwmeanByTSS")],
                                         metric.binary = "all",
                                         metric.filter = "all",
                                         output.format = ".tif",
                                         nb.cpu = 6)
  ),
  tar_target(
    name = tar_projLayer_rodent_wessex,
    command = save_proj_layer(tar_biomodForecast_rodent_wessex)
  ),
  tar_target(
    name = tar_biomodForecast_rodent_aberdeen,
    command = BIOMOD_EnsembleForecasting(bm.em = tar_biomodEns_rodent,
                                         proj.name = "CurrentEM_rodent_aberdeen",
                                         new.env = read_stack_layers(layerLoc = here("data", "GIS data", "SDM Layers"),
                                                                     tar_sdm_layers) %>%
                                           crop(tar_patchList$Aberdeen),
                                         models.chosen = get_built_models(tar_biomodEns_rodent)[
                                           stringr::str_detect(get_built_models(tar_biomodEns_rodent), "EMwmeanByTSS")],
                                         metric.binary = "all",
                                         metric.filter = "all",
                                         output.format = ".tif",
                                         nb.cpu = 6)
  ),
  tar_target(
    name = tar_projLayer_rodent_aberdeen,
    command = save_proj_layer(tar_biomodForecast_rodent_aberdeen)
  ),
  # fallow Poisson ----
  tar_target(
    name = tar_ssfFallow_data,
    command = prepare_ssfFallow_data(tar_deerData, tar_landuseList, tar_patchList, cores = 12,
                                     nAvail = nAvailable, slDist = slDistribution,
                                     taDist = taDistribution)
  ),
  tar_target(
    name = tar_poisFallow_model,
    command = run_poisFallow_model(tar_ssfFallow_data)
  ),
  tar_target(
    name = tar_predPoisResist_fallow,
    command = build_predResistanceFallow_layer(tar_ssfFallow_data, tar_poisFallow_model,
                                               tar_projLayer_fallow,
                                               prelimAggFact = aggFact_SDM)
  ),
  tar_target(
    name = tar_longestFallow,
    command = extract_akdeFallow_longest(tar_akdeLists)
  ),
  # fallow omniscape ----
  tar_target(
    name = tar_omniLayers_fallow,
    command = build_omniscape_layer(tar_predPoisResist_fallow, tar_patchList$Wessex, #tar_longestFallow,
                                    blockSize = bs_fallow, searchRadius = sr_fallow, reRun = FALSE,
                                    projName = "omniscape_output_fallow")
  ),
  # fallow plots ----
  tar_target(
    name = tar_biomodSingle_plots_fallow,
    command = plot_biomodEval_single(tar_biomodModels_fallow)
  ),
  tar_target(
    name = tar_biomodEns_plots_fallow,
    command = plot_biomodEval_ensemble(tar_biomodEns_fallow, tar_patchList)
  ),
  tar_target(
    name = tar_HFOcc_plots_fallow,
    command = plot_occSDMOmni_inOut_HFOcc(species = "fallow",
                                          hfBiasLayer = here("data", "Human Footprint", "hfp2022.tif"),
                                          tar_pseudoAbs_fallow)
  ),
  tar_target(
    name = tar_SDMin_plots_fallow,
    command = plot_occSDMOmni_inOut_SDMin(species = "fallow",
                                          sdmLayers = read_stack_layers(layerLoc = here("data", "GIS data", "SDM Layers")))
  ),
  tar_target(
    name = tar_omniIN_plots_fallow,
    command = plot_occSDMOmni_inOut_omniIN(species = "fallow",
                                           tar_projLayer_fallow,
                                           tar_predPoisResist_fallow)
  ),
  tar_target(
    name = tar_omniOUT_plots_fallow,
    command = plot_occSDMOmni_inOut_omniOUT(species = "fallow",
                                            tar_omniLayers_fallow,
                                            tar_selectedPatchList)
  ),
  # rodent omniscape ----
  tar_target(
    name = tar_predPoisResist_rodent_wessex,
    command = build_predResistanceRodent_layer(tar_projLayer_rodent_wessex,
                                               prelimAggFact = aggFact_SDM)
  ),
  tar_target(
    name = tar_omniLayers_rodent_wessex,
    command = build_omniscape_layer(tar_predPoisResist_rodent_wessex, tar_patchList$Wessex, #tar_longestrodent,
                                    blockSize = bs_rodent, searchRadius = sr_rodent, reRun = TRUE,
                                    projName = "omniscape_output_rodent_wessex")
  ),
  tar_target(
    name = tar_predPoisResist_rodent_aberdeen,
    command = build_predResistanceRodent_layer(tar_projLayer_rodent_aberdeen,
                                               prelimAggFact = aggFact_SDM)
  ),
  tar_target(
    name = tar_omniLayers_rodent_aberdeen,
    command = build_omniscape_layer(tar_predPoisResist_rodent_aberdeen, tar_patchList$Aberdeen, #tar_longestrodent,
                                    blockSize = bs_rodent, searchRadius = sr_rodent, reRun = TRUE,
                                    projName = "omniscape_output_rodent_aberdeen")
  ),
  # rodent plots ----
  tar_target(
    name = tar_biomodSingle_plots_rodent,
    command = plot_biomodEval_single(tar_biomodModels_rodent)
  ),
  tar_target(
    name = tar_biomodEns_plots_rodent,
    command = plot_biomodEval_ensemble(tar_biomodEns_rodent, tar_patchList)
  ),
  tar_target(
    name = tar_HFOcc_plots_rodent,
    command = plot_occSDMOmni_inOut_HFOcc(species = "rodent",
                                          hfBiasLayer = here("data", "Human Footprint", "hfp2022.tif"),
                                          tar_pseudoAbs_rodent)
  ),
  tar_target(
    name = tar_SDMin_plots_rodent,
    command = plot_occSDMOmni_inOut_SDMin(species = "rodent",
                                          sdmLayers = read_stack_layers(layerLoc = here("data", "GIS data", "SDM Layers")))
  ),
  tar_target(
    name = tar_omniIN_plots_rodent_wessex,
    command = plot_occSDMOmni_inOut_omniIN(species = "rodent",
                                           tar_projLayer_rodent_wessex,
                                           tar_predPoisResist_rodent_wessex)
  ),
  tar_target(
    name = tar_omniOUT_plots_rodent_wessex,
    command = plot_occSDMOmni_inOut_omniOUT(species = "rodent",
                                            tar_omniLayers_rodent_wessex,
                                            tar_selectedPatchList)
  ),
  tar_target(
    name = tar_omniIN_plots_rodent_aberdeen,
    command = plot_occSDMOmni_inOut_omniIN(species = "rodent",
                                           tar_projLayer_rodent_aberdeen,
                                           tar_predPoisResist_rodent_aberdeen)
  ),
  tar_target(
    name = tar_omniOUT_plots_rodent_aberdeen,
    command = plot_occSDMOmni_inOut_omniOUT(species = "rodent",
                                            tar_omniLayers_rodent_aberdeen,
                                            tar_selectedPatchList)
  ),
  # Extract values patch-wise -----------------------------------------------
  tar_target(
    name = tar_patch_omniSummary_rodent_wessex,
    command = extract_patch_omniConnectivity(tar_omniLayers_rodent_wessex, tar_selectedPatchList,
                                             buffers = buffers)
  ),
  tar_target(
    name = tar_patch_omniSummary_rodent_aberdeen,
    command = extract_patch_omniConnectivity(tar_omniLayers_rodent_aberdeen, tar_selectedPatchList,
                                             buffers = buffers)
  ),
  tar_target(
    name = tar_patch_omniSummary_fallow,
    command = extract_patch_omniConnectivity(tar_omniLayers_fallow, tar_selectedPatchList,
                                             buffers = buffers)
  )
)


# WREN landscape offshoot -------------------------------------------------

wrenList_simplified <- list(
  tar_target(
    name = tar_patchList_WREN,
    command = read_patches_data_WREN()
  ),
  tar_target(
    name = tar_landuseList_WREN,
    command = read_landuse_data_WREN(tar_patchList_WREN, prelimAggFact = NA)
  ),
  tar_target(
    name = tar_predPoisResist_locationWREN,
    command = build_predResistance_layer_WREN(tar_ssf_data, tar_pois_model,
                                         tar_landuseList_WREN, tar_patchList_WREN,
                                         tar_deerData, prelimAggFact = NA)
  ),
  tar_target(
    name = tar_connectPois_locationWREN,
    command = build_connect_layer_WREN(tar_predPoisResist_locationWREN, tar_patchList_WREN,
                                       tar_akdeSummary,
                                       REGION = "WREN", prelimAggFact = aggFact,
                                       seed = 2025, repeatsPerPair = connectSettings$repeatsPerPair[1],
                                       MSEdf = tar_msePois_df,
                                       MINPATCHSIZE = minPatchSize_m2,
                                       cropArea = setCropArea, cores = useCores)
  ),
  tar_target(
    name = tar_connectStanPois_locationWREN,
    command = standardise_connect_layer(tar_connectPois_locationWREN,
                                        REGION = "WREN",
                                        THETA = NULL, MSEdf = tar_msePois_df)
  ),
  tar_target(
    tar_patch_summaryPois_WREN,
    extract_patch_connectivity_WREN(tar_msePois_df,
                                    tar_connectStanPois_locationWREN,
                                    tar_patchList_WREN,
                                    REGION = "WREN",
                                    buffers = buffers)
  )
)

list(coreTargetList,
     connectTargetList,
     coreSDMList,
     wrenList_simplified)
