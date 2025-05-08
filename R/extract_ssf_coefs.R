#' Pull out indi coefs and IndRSA metrics
#'
#' @name extract_ssf_coefs
#' @description abc
#' @return abc
#'
#' @export
extract_ssf_coefs <- function(ssfModels){

  # targets::tar_load("tar_ssf_models")
  # ssfModels <- tar_ssf_models
  # targets::tar_source()

  paletteList <- load_deer_palette()
  sigColourDF <- data.frame(colour = paletteList$highSigLowSigNoSig,
                            sig = names(paletteList$highSigLowSigNoSig))

  indiCoef <- ind_coef(1, ssfModels, cutoff = -1)
  indiSE <- ind_se(1, ssfModels, cutoff = -1)

  ssfPopAvg <- pop_avg(m = 1, mod_ls = ssfModels, cutoff = -1)

  popAvg <- as.data.frame(ssfPopAvg[[1]])
  popAvg$variable <- rownames(popAvg)
  popAvg$Animal_ID <- "<b>Population</b>"
  popAvg <- popAvg %>%
    mutate(sig = ifelse(LCI > 0, "Significant +", ifelse(UCI < 0, "Significant -", "Not Significant")),
           variableType = case_when(
             grepl("sl_", variable) ~ "Step",
             grepl("distance", variable) ~ "Distance",
             grepl("landuse", variable) ~ "Landuse",
             TRUE ~ "Other"
           ),
           variable = case_when(
             str_detect(variable, "\\.") ~ str_replace_all(gsub("landuse", "", variable), "\\.", " "),
             str_detect(variable, "distance") ~ str_replace_all(variable, "distance", "Distance to "),
             variable == "roadCrossingsTRUE" ~ "Road Crossing",
             TRUE ~ gsub("landuse", "", variable)
           )) %>%
    filter(!variable == "landuseBarren") %>%
    left_join(sigColourDF, by = "sig") %>%
    mutate(Animal_ID_colour = glue::glue("<i style='color:{colour}'>{Animal_ID}</i>")) %>%
    mutate(Animal_ID_colour = factor(Animal_ID_colour, levels = c(unique(Animal_ID_colour))))

  write.csv(popAvg, file = here::here("tables", "ssf_popAvgCoef.csv"), row.names = FALSE)

  indiDataframe <- indiCoef %>%
    as.data.frame() %>%
    # select(-ID, -Freq, -landuseBarren) %>%
    select(-ID, -Freq) %>%
    pivot_longer(where(is.numeric), names_to = "variable", values_to = "est") %>%
    left_join(indiSE %>%
                as.data.frame() %>%
                # select(-ID, -Freq, -landuseBarren) %>%
                select(-ID, -Freq) %>%
                pivot_longer(where(is.numeric), names_to = "variable", values_to = "se")) %>%
    rename("Animal_ID" = name) %>%
    mutate(sig = ifelse(est-se > 0, "Significant +", ifelse(est+se < 0, "Significant -", "Not Significant")),
           variableType = case_when(
             grepl("sl_", variable) ~ "Step",
             grepl("distance", variable) ~ "Distance",
             grepl("landuse", variable) ~ "Landuse",
             TRUE ~ "Other"
           ),
           variable = case_when(
             str_detect(variable, "\\.") ~ str_replace_all(gsub("landuse", "", variable), "\\.", " "),
             str_detect(variable, "distance") ~ str_replace_all(variable, "distance", "Distance to "),
             variable == "roadCrossingsTRUE" ~ "Road Crossing",
             TRUE ~ gsub("landuse", "", variable)
           )) %>%
    left_join(sigColourDF, by = "sig") %>%
    mutate(Animal_ID_colour = glue::glue("<i style='color:{colour}'>{Animal_ID}</i>")) %>%
    arrange(desc(Animal_ID)) %>%
    mutate(Animal_ID_colour = factor(Animal_ID_colour, levels = c(unique(popAvg$Animal_ID_colour), unique(Animal_ID_colour))))

  write.csv(indiDataframe, file = here::here("tables", "ssf_indiCoef.csv"), row.names = FALSE)

  simuCoefs <- simu_coefs(coef = indiCoef, se = indiSE, n = 10000)

  simuSD <- simu_sd(simuCoefs)
  colnames(simuSD) <- names(indiCoef)
  apply(simuSD, 2, quantile, na.rm=T) #Show variation around estimate of each covariate
  simuSDmeans <- colMeans(simuSD) #Calculate average heterogeneity for each covariate
  simuSDmeans <- data.frame(
    variable = names(simuSDmeans),
    meanSD = simuSDmeans) %>%
    filter(!is.na(simuSDmeans)) %>%
    mutate(valueMeasure = "Heterogeneity",
           variable = case_when(
             str_detect(variable, "\\.") ~ str_replace_all(gsub("landuse", "", variable), "\\.", " "),
             str_detect(variable, "distance") ~ str_replace_all(variable, "distance", "Distance to "),
             variable == "roadCrossingsTRUE" ~ "Road Crossing",
             TRUE ~ gsub("landuse", "", variable)
           ))

  simuSD <- simuSD %>%
    as.data.frame() %>%
    # select(-name, -ID, -Freq, -landuseBarren) %>%
    select(-name, -ID, -Freq) %>%
    pivot_longer(everything(), values_to = "value") %>%
    rename("variable" = name) %>%
    mutate(variableType = case_when(
      variable %in% c("distanceWoodland", "distanceHedges") ~ "Distance",
      variable %in% c("roadCrossingsTRUE") ~ "Other",
      variable %in% c("sl_", "cos.ta_.", "log.sl_.") ~ "Core movement",
      str_detect(variable, "\\:sl_") ~ "Step interactions (sl_)",
      str_detect(variable, "\\:log.sl_.") ~ "Step interactions (log_sl)",
      TRUE ~ "Landuse"
    ),
    valueMeasure = "Heterogeneity",
    variable = case_when(
      str_detect(variable, "\\.") ~ str_replace_all(gsub("landuse", "", variable), "\\.", " "),
      str_detect(variable, "distance") ~ str_replace_all(variable, "distance", "Distance to "),
      variable == "roadCrossingsTRUE" ~ "Road Crossing",
      TRUE ~ gsub("landuse", "", variable)
    )) %>%
    arrange(desc(variable)) %>%
    mutate(variable = factor(variable, levels = unique(variable)))

  simuSPE <- simu_spe(simuCoefs)
  colnames(simuSPE) <- names(indiCoef)
  apply(simuSPE, 2, quantile, na.rm=T) #Show variation around estimate of each covariate
  simuSPEmeans <- colMeans(simuSPE) #Calculate average specialization for each covariate
  simuSPEmeans <- data.frame(
    variable = names(simuSPEmeans),
    meanSPE = simuSPEmeans) %>%
    filter(!is.na(simuSPEmeans)) %>%
    mutate(valueMeasure = "Specialisation",
           variable = case_when(
             str_detect(variable, "\\.") ~ str_replace_all(gsub("landuse", "", variable), "\\.", " "),
             str_detect(variable, "distance") ~ str_replace_all(variable, "distance", "Distance to "),
             variable == "roadCrossingsTRUE" ~ "Road Crossing",
             TRUE ~ gsub("landuse", "", variable)
           ))

  simuSPE <- simuSPE %>%
    as.data.frame() %>%
    # select(-name, -ID, -Freq, -landuseBarren) %>%
    select(-name, -ID, -Freq) %>%
    pivot_longer(everything(), values_to = "value") %>%
    rename("variable" = name) %>%
    mutate(variableType = case_when(
      variable %in% c("distanceWoodland", "distanceHedges") ~ "Distance",
      variable %in% c("roadCrossingsTRUE") ~ "Other",
      variable %in% c("sl_", "cos.ta_.", "log.sl_.") ~ "Core movement",
      str_detect(variable, "\\:sl_") ~ "Step interactions (sl_)",
      str_detect(variable, "\\:log.sl_.") ~ "Step interactions (log_sl)",
      TRUE ~ "Landuse"
    ),
    valueMeasure = "Specialisation",
    variable = case_when(
      str_detect(variable, "\\.") ~ str_replace_all(gsub("landuse", "", variable), "\\.", " "),
      str_detect(variable, "distance") ~ str_replace_all(variable, "distance", "Distance to "),
      variable == "roadCrossingsTRUE" ~ "Road Crossing",
      TRUE ~ gsub("landuse", "", variable)
    )) %>%
    arrange(desc(variable)) %>%
    mutate(variable = factor(variable, levels = unique(variable)))

  write.csv(simuSDmeans %>%
              full_join(simuSPEmeans),
            file = here::here("tables", "ssf_meanSD_SPE.csv"), row.names = FALSE)

  outputList <- list(
    indiDataframe = indiDataframe,
    popAvg = popAvg,
    simuSD = simuSD,
    simuSPE = simuSPE,
    simuSDmeans = simuSDmeans,
    simuSPEmeans = simuSPEmeans
  )

  return(outputList)

}
