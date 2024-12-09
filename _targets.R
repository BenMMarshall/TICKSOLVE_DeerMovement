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
               "performance",
               "ggplot2",
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

  # controller = crew::crew_controller_local(workers = 2, seconds_idle = 60),

  #
  format = "qs" # Optionally set the default storage format. qs is fast.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# OPTIONS AND DECISIONS
# RSF
nAvailable <- 10
typeAvialable <- "random"
contourAvialable <- "99%"
rsfFormula <- case_ ~ distanceWoodland + landuse + distancePatch:landuse
# SSF
nAvailableSteps <- 10
slDistribution <- "gamma"
taDistribution <- "vonmises"
ssfFormula <- case_ ~ distanceWoodland + landuse + distanceWoodland:landuse +
  roadCrossings +
  sl_ + log(sl_) + cos(ta_) + log(sl_):landuse + log(sl_):distanceWoodland +
  strata(step_id_)
# dbbmm
windowSize <- 29 #~ a week
marginSize <- 5 #~ a day
locationError <- 0.1

# Replace the target list below with your own:
list(
  tar_target(
    name = tar_deerData,
    command = read_deer_data()
  ),
  tar_target(
    name = tar_landuseList,
    command = read_landuse_data(tar_deerData)
  ),
  tar_target(
    name = tar_patchList,
    command = read_patches_data()
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
  tar_target(
    name = tar_rsf_data,
    command = prepare_rsf_data(tar_deerData, tar_akdeLists, tar_landuseList, tar_patchList,
                               nAvail = nAvailable, typeAvial = typeAvialable, conAvail = contourAvialable)
  ),
  tar_target(
    name = tar_rsf_models,
    command = run_rsf_models(tar_rsf_data, rsfFormula = rsfFormula)
  ),
  tar_target(
    name = tar_rsf_outputs,
    command = extract_rsf_results(tar_deerData, tar_rsf_data, tar_rsf_models, nAvail = nAvailable)
  ),
  tar_target(
    name = tar_ssf_data,
    command = prepare_ssf_data(tar_deerData, tar_landuseList, tar_patchList,
                                  nAvail = nAvailable, slDist = slDistribution,
                                  taDist = taDistribution)
  ),
  tar_target(
    name = tar_ssf_models,
    command = run_ssf_models(tar_ssf_data, ssfFormula = ssfFormula)
  )
)
