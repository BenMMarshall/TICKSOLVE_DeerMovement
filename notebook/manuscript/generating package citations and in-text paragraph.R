library(grateful)
library(stringr)
library(dplyr)

packages <- grateful::scan_packages()
packages <- packages[!packages$pkg %in% c("bestNormalize", "JuliaCall", "recurse",
                                          "roxygen2", "suncalc", "survival"),]

packages

packageCitations <- grateful::get_pkgs_info(packages$pkg,
                                            out.dir = here::here("notebook", "manuscript"),
                                            bib.file = "packages_refs",
                                            include.RStudio = TRUE)

packagesDataframe <- packageCitations %>%
  rowwise() %>%
  mutate(inTextString =
           paste0(pkg, " (v.", version, ") [@", paste0(citekeys, collapse = ";@"), "]")
  ) %>%
  select(-citekeys)
# add missing tidyterra intext bit
packagesDataframe[packagesDataframe$pkg == "tidyterra",]$inTextString <-
  sub("@", "@R-tidyterra", packagesDataframe[packagesDataframe$pkg == "tidyterra",]$inTextString)

packagesDataframe <- packagesDataframe %>%
  rbind(data.frame(
    pkg = "R Studio",
    version = rstudioapi::versionInfo()$long_version,
    citekeys = "rstudio"
  ) %>%
    mutate(inTextString =
             paste0(pkg, " (v.", version, ") [@", paste0(citekeys, collapse = ";"), "]")
    ) %>%
    select(-citekeys)
  )

# remove some of the INLA citations
packagesDataframe$inTextString[packagesDataframe$pkg == "INLA"] <-
  gsub("INLA2009a;|INLA2011c;|;INLA2017e;INLA2018f;INLA2016g;INLA2017h;INLA2018i", "", packagesDataframe$inTextString[packagesDataframe$pkg == "INLA"])

packagesDataframe <- packagesDataframe %>%
  ungroup() %>%
  # filter(!pkg == "base") %>%
  mutate(purpose = case_when(
    pkg %in% c("amt", "ctmm", "move") ~ "For analysis of animal movement data we used",
    pkg %in% c("gdistance", "raster", "sf", "sp", "terra", "tidyterra") ~ "To manipulate and manage spatial data we used",
    pkg %in% c("ggdist", "ggridges", "ggtext", "patchwork", "scales") ~ "For visualisation we used the following as expansions from the tidyverse suite of packages:",
    pkg %in% c("tidyverse", "units", "sjmisc", "glue") ~ "For general data manipulation we used",
    pkg %in% c("targets", "tarchetypes", "here") ~ "For project and code management we used",
    pkg %in% c("lme4", "INLA", "performance", "effects") ~ "To run models and explore model outputs we used",
    pkg %in% c("bookdown", "rmarkdown") ~ "To generate typeset outputs we used",
    pkg %in% c("base", "R Studio") ~ "For all analysis we used",
    TRUE ~ "Other pacakges we used were"
  )) %>%
  mutate(order = case_when(
    pkg %in% c("amt", "ctmm", "move") ~ 2,
    pkg %in% c("gdistance", "raster", "sf", "sp", "terra", "tidyterra") ~ 3,
    pkg %in% c("ggdist", "ggridges", "ggtext", "patchwork", "scales") ~ 5,
    pkg %in% c("tidyverse", "units", "sjmisc", "glue") ~ 6,
    pkg %in% c("targets", "tarchetypes", "here") ~ 7,
    pkg %in% c("lme4", "INLA", "performance", "effects") ~ 4,
    pkg %in% c("bookdown", "rmarkdown") ~ 8,
    pkg %in% c("base", "R Studio") ~ 1,
    TRUE ~ 9
  )) %>%
  arrange(order)

packagesDataframe[packagesDataframe$pkg == "base",]$inTextString <-
  sub("^base", "R", packagesDataframe[packagesDataframe$pkg == "base",]$inTextString)

packagesDataframe <- packagesDataframe %>%
  group_by(purpose) %>%
  summarise(inText = paste0(paste0(inTextString[1:n()-1], collapse = ", "), ", and ",
                            inTextString[n()])) %>%
  ungroup() %>%
  mutate(inTextFull = paste0(purpose, " ", inText, "."))

packageParagraph <- paste0(packagesDataframe$inTextFull, collapse = " ")

writeLines(packageParagraph, here::here("notebook", "manuscript", "packageParagraph.txt"))

