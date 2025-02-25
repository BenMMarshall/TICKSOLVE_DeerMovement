#' Create a plot all aspects of omni pipeline
#'
#' @name plot_occSDMOmni_inOut
#' @description abc
#' @return abc
#'
#' @export
plot_occSDMOmni_inOut <- function(...){

  # targets::tar_source()
  # targets::tar_load("tar_projLayer_fallow")
  # targets::tar_load("tar_predPoisResist_fallow")
  # targets::tar_load("tar_omniLayers")
  # targets::tar_load("tar_pseudoAbs_fallow")
  # library(patchwork)

  # pseudoAbs_fallow <- tar_pseudoAbs_fallow
  # projLayer_fallow <- tar_projLayer_fallow
  # predPoisResist_fallow <-tar_predPoisResist_fallow
  # omniLayers <- tar_omniLayers
  sourceOmniTerra <- rast(here::here("data", "GIS data", "sourceOmniWessex.tif"))

  hfData <- rast(here("data", "Human Footprint", "hfp2022.tif"))
  UKbbox <- st_bbox(c(xmin = -900000, xmax = 200000, ymin = 5500000, ymax = 7000000))
  hfDataCrop <- terra::crop(hfData, UKbbox)
  hfDataBNG <- terra::project(hfDataCrop, crs("epsg:27700"))
  hfDataBNG <- terra::crop(hfDataBNG, st_bbox(as.data.frame(pseudoAbs_fallow$xy) %>%
                                                st_as_sf(coords = c("x", "y"), crs = 27700)))
  # plot(hfDataBNG)
  rescale <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}
  hfDataBNG <- hfDataBNG %>%
    mutate(hfp2022 = rescale(hfp2022))

  projLayer_fallow <- rast(projLayer_fallow)
  predPoisResist_fallow <-rast(predPoisResist_fallow)
  omniLayers <- rast(omniLayers)

  sdmLayers <- read_stack_layers(layerLoc = here("data", "GIS data", "SDM Layers"))

  occDataPlottable <- data.frame(
    x = pseudoAbs_fallow$xy[,"x"],
    y = pseudoAbs_fallow$xy[,"y"],
    resp = pseudoAbs_fallow$sp) %>%
    mutate(resp = ifelse(is.na(resp), "Pseudo-absence", "Occurrence"))

  paletteList <- load_deer_palette()

  ggplotThemeCombo <-
    theme_bw() +
    theme(
      text = element_text(colour = paletteList$baseGrey),
      line = element_line(colour = paletteList$baseGrey),
      plot.title = element_text(face = 2),
      axis.title = element_text(face = 2),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(angle = 0, face = 4, hjust = 0, vjust = 1)
      # legend.position = "none"
    )


  # Occ data plots ----------------------------------------------------------

  hf_plot <- ggplot() +
    geom_spatraster(data = hfDataBNG) +
    facet_wrap(vars(lyr)) +
    labs(title = "Human footprint 2022 bias layer", fill = "Human footprint (scaled)")  +
    scale_fill_gradient(high = scales::muted(paletteList$highSigLowSigNoSig[1]),
                        low = "#ffffff",
                        na.value = paletteList$baseGrey) +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo

  occPoint_plot <- ggplot() +
    geom_point(data = occDataPlottable %>%
                 arrange(desc(resp)),
               aes(x = x, y = y, colour = resp, alpha = resp)) +
    labs(title = "Fallow deer occurrence data", colour = "Point classification",
         alpha = "Point classification") +
    scale_colour_manual(values = unname(paletteList$deerSpeciesPal[c(1,3)])) +
    scale_alpha_manual(values = c(1, 0.25)) +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo +
    theme(axis.title = element_blank())

  plot_layout(hf_plot / occPoint_plot)
  ggsave(filename = here("figures", "fallow_occurrenceData_plot.png"),
         width = 240, height = 300, units = "mm")

  # SDM inputs --------------------------------------------------------------

  sdmInput_plot <- ggplot() +
    geom_spatraster(data = sdmLayers) +
    facet_wrap(vars(lyr)) +
    labs(title = "SDM input layers", fill = "Distance|presence (scaled)")  +
    scale_fill_gradient(high = scales::muted(paletteList$highSigLowSigNoSig[1]),
                        low = "#ffffff",
                        na.value = paletteList$baseGrey) +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo

  ggsave(filename = here("figures", "fallow_sdmInputs_plot.png"),
         plot = sdmInput_plot,
         width = 350, height = 200, units = "mm")

  # Omni inputs -------------------------------------------------------------

  projectedLayer_plot <- ggplot() +
    geom_spatraster(data = projLayer_fallow) +
    facet_wrap(vars(lyr)) +
    labs(title = "Projected ensemble model preference", fill = "Probability of occurrence")  +
    scale_fill_gradient(high = scales::muted(paletteList$highSigLowSigNoSig[1]),
                        low = "#ffffff",
                        na.value = paletteList$baseGrey) +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo

  resistanceLayer_plot <- ggplot() +
    geom_spatraster(data = predPoisResist_fallow, aes(fill = resistance)) +
    scale_fill_gradient(high = scales::muted(paletteList$highSigLowSigNoSig[1]),
                        low = "#ffffff",
                        na.value = paletteList$baseGrey) +
    labs(title = "Poisson adjusted resistance map", fill = "Resistance")  +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo

  sourceLayer_plot <- ggplot() +
    geom_spatraster(data = sourceOmniTerraLoc, aes(fill = Ptch_ID)) +
    scale_fill_gradient(high = scales::muted(paletteList$highSigLowSigNoSig[1]),
                        low = "#ffffff",
                        na.value = paletteList$baseGrey) +
    labs(title = "Current source areas", fill = "Source")  +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo

  plot_layout(projectedLayer_plot / resistanceLayer_plot / sourceLayer_plot)
  ggsave(filename = here("figures", "fallow_omniscapeInputs_plot.png"),
         width = 160, height = 300, units = "mm")

  # Omni outputs ------------------------------------------------------------

  cumCurr_plot <- ggplot() +
    geom_spatraster(data = omniLayers, aes(fill = cum_currmap)) +
    scale_fill_gradient(high = scales::muted(paletteList$highSigLowSigNoSig[1]),
                        low = "#ffffff",
                        na.value = paletteList$baseGrey) +
    labs(title = "Cumulative current map", fill = "Cumulative current")  +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo

  potCurr_plot <- ggplot() +
    geom_spatraster(data = omniLayers, aes(fill = flow_potential)) +
    scale_fill_gradient(high = scales::muted(paletteList$highSigLowSigNoSig[1]),
                        low = "#ffffff",
                        na.value = paletteList$baseGrey) +
    labs(title = "Flow potential map", fill = "Flow potential")  +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo

  normCurr_plot <- ggplot() +
    geom_spatraster(data = omniLayers, aes(fill = normalized_cum_currmap)) +
    scale_fill_gradient(high = scales::muted(paletteList$highSigLowSigNoSig[1]),
                        low = "#ffffff",
                        na.value = paletteList$baseGrey) +
    labs(title = "Normalised cumulative current map", fill = "Normalised cumulative current") +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo

  plot_layout(cumCurr_plot / potCurr_plot / normCurr_plot)
  ggsave(filename = here("figures", "fallow_omniscapeOutputs_plot.png"),
         width = 160, height = 300, units = "mm")

  return(list(
    hf_plot = hf_plot,
    occPoint_plot = occPoint_plot,
    sdmInput_plot = sdmInput_plot,
    projectedLayer_plot = projectedLayer_plot,
    resistanceLayer_plot = resistanceLayer_plot,
    sourceLayer_plot = sourceLayer_plot,
    cumCurr_plot = cumCurr_plot,
    potCurr_plot = potCurr_plot,
    normCurr_plot = normCurr_plot))

}
