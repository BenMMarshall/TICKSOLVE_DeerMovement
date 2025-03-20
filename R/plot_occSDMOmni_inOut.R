#' Create a plot all aspects of omni pipeline
#'
#' @name plot_occSDMOmni_inOut
#' @description abc
#' @return abc
#'
#' @export
plot_occSDMOmni_inOut_HFOcc <- function(species,
                                        hfBiasLayer = here("data", "Human Footprint", "hfp2022.tif"),
                                        pseudoAbs,
                                        ...){

  # targets::tar_source()
  # targets::tar_load("tar_projLayer_rodent")
  # targets::tar_load("tar_predPoisResist_rodent")
  # targets::tar_load("tar_omniLayers_rodent")
  # targets::tar_load("tar_pseudoAbs_rodent")
  # library(patchwork)

  # pseudoAbs <- tar_pseudoAbs_rodent
  # projLayer <- tar_projLayer_rodent
  # predPoisResist <-tar_predPoisResist_rodent
  # omniLayers <- tar_omniLayers_rodent
  # sdmLayers <- read_stack_layers(layerLoc = here("data", "GIS data", "SDM Layers"))

  hfData <- rast(hfBiasLayer)
  UKbbox <- st_bbox(c(xmin = -900000, xmax = 200000, ymin = 5500000, ymax = 7000000))
  hfDataCrop <- terra::crop(hfData, UKbbox)
  hfDataBNG <- terra::project(hfDataCrop, crs("epsg:27700"))
  pseudobbox <- st_bbox(data.frame(x = pseudoAbs$xy[,"x"],
                                   y = pseudoAbs$xy[,"y"]) %>%
                          st_as_sf(coords = c("x", "y"), crs = 27700))
  hfDataBNG <- terra::crop(hfDataBNG, pseudobbox)
  # plot(hfDataBNG)
  rescale <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}
  hfDataBNG <- hfDataBNG %>%
    mutate(hfp2022 = rescale(hfp2022))


  occDataPlottable <- data.frame(
    x = pseudoAbs$xy[,"x"],
    y = pseudoAbs$xy[,"y"],
    resp = pseudoAbs$sp) %>%
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
               aes(x = x, y = y, colour = resp, alpha = resp),
               size = 0.5) +
    labs(title = "Occurrence data", colour = "Point classification",
         alpha = "Point classification") +
    scale_colour_manual(values = unname(paletteList$deerSpeciesPal[c(1,3)])) +
    scale_alpha_manual(values = c(1, 0.25)) +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo +
    theme(axis.title = element_blank())

  plot_layout(hf_plot + occPoint_plot)
  ggsave(filename = here("figures", paste0(species, "_occurrenceData_plot.png")),
         width = 300, height = 240, units = "mm")

  return(list(
    hf_plot = hf_plot,
    occPoint_plot = occPoint_plot))

}

#' @export
plot_occSDMOmni_inOut_SDMin <- function(species,
                                        sdmLayers,
                                        ...){

  # targets::tar_load("tar_pseudoAbs_rodent")
  # pseudoAbs <- tar_pseudoAbs_rodent

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

  # SDM inputs --------------------------------------------------------------

  sdmInput_plot <- ggplot() +
    geom_spatraster(data = sdmLayers) +
    facet_wrap(vars(lyr)) +
    labs(title = "SDM input layers", fill = "Distance")  +
    scale_fill_gradient(high = scales::muted(paletteList$highSigLowSigNoSig[1]),
                        low = "#ffffff",
                        na.value = paletteList$baseGrey) +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo

  ggsave(filename = here("figures", paste0(species, "_sdmInputs_plot.png")),
         plot = sdmInput_plot,
         width = 350, height = 200, units = "mm")

  return(list(
    sdmInput_plot = sdmInput_plot))

}

#' @export
plot_occSDMOmni_inOut_omniIN <- function(species,
                                         projLayer,
                                         predPoisResist,
                                         ...){

  sourceOmniTerra <- rast(here::here("data", "GIS data", "sourceOmniWessex.tif"))
  projLayer <- rast(projLayer)
  predPoisResist <- rast(predPoisResist)

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

  # Omni inputs -------------------------------------------------------------

  projectedLayer_plot <- ggplot() +
    geom_spatraster(data = projLayer) +
    facet_wrap(vars(lyr)) +
    labs(title = "Projected ensemble model preference", fill = "Probability of occurrence")  +
    scale_fill_gradient(high = scales::muted(paletteList$highSigLowSigNoSig[1]),
                        low = "#ffffff",
                        na.value = paletteList$baseGrey) +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo

  # if(species == "fallow"){
  resistanceLayer_plot <- ggplot() +
    geom_spatraster(data = predPoisResist, aes(fill = resistance)) +
    scale_fill_gradient(high = scales::muted(paletteList$highSigLowSigNoSig[1]),
                        low = "#ffffff",
                        na.value = paletteList$baseGrey) +
    labs(title = "Resistance map", fill = "Resistance")  +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo
  # }

  sourceLayer_plot <- ggplot() +
    geom_spatraster(data = sourceOmniTerra, aes(fill = Ptch_ID)) +
    scale_fill_gradient(high = scales::muted(paletteList$highSigLowSigNoSig[1]),
                        low = "#ffffff",
                        na.value = paletteList$baseGrey) +
    labs(title = "Current source areas", fill = "Source")  +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo

  # if(species == "fallow"){
  plot_layout(projectedLayer_plot / resistanceLayer_plot / sourceLayer_plot)
  # } else {
  #   plot_layout(projectedLayer_plot / sourceLayer_plot)
  # }
  ggsave(filename = here("figures", paste0(species, "_omniscapeInputs_plot.png")),
         width = 160, height = 300, units = "mm")

  return(list(
    projectedLayer_plot = projectedLayer_plot,
    resistanceLayer_plot = resistanceLayer_plot,
    sourceLayer_plot = sourceLayer_plot))

}

#' @export
plot_occSDMOmni_inOut_omniOUT <- function(species,
                                          omniLayers,
                                          selectedPatchList,
                                          ...){
  # targets::tar_load("tar_selectedPatchList")
  # targets::tar_load("tar_omniLayers_fallow")
  # omniLayers <- tar_omniLayers_fallow
  # selectedPatchList <- tar_selectedPatchList

  focalPatches <- selectedPatchList$Wessex
  omniLayers <- rast(omniLayers)

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
    scale_fill_gradient2(high = scales::muted(paletteList$highSigLowSigNoSig[1]),
                         mid = "#ffffff",
                         low = "#202020",
                         midpoint = 1,
                        na.value = paletteList$baseGrey, trans = "sqrt") +
    labs(title = "Normalised cumulative current map", fill = "Normalised cumulative current") +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo

  plot_layout(cumCurr_plot / potCurr_plot / normCurr_plot)
  ggsave(filename = here("figures", paste0(species, "_omniscapeOutputs_plot.png")),
         width = 160, height = 300, units = "mm")

  normCurrPatches_plot <- ggplot() +
    geom_spatraster(data = omniLayers, aes(fill = normalized_cum_currmap)) +
    geom_sf(data = focalPatches, fill = NA, colour = "black") +
    scale_fill_gradient2(high = scales::muted(paletteList$highSigLowSigNoSig[1]),
                         mid = "#ffffff",
                         low = "#202020",
                         midpoint = 1,
                        na.value = paletteList$baseGrey, trans = "sqrt") +
    labs(title = "Normalised cumulative current map with sampled patches", fill = "Normalised cumulative current") +
    coord_sf(expand = 0, datum = st_crs(27700)) +
    ggplotThemeCombo

  ggsave(filename = here("figures", paste0(species, "_omniscapeNormCurrPatch_plot.png")),
         width = 250, height = 250, units = "mm")

  return(list(
    cumCurr_plot = cumCurr_plot,
    potCurr_plot = potCurr_plot,
    normCurr_plot = normCurr_plot,
    normCurrPatches_plot = normCurrPatches_plot))

}

