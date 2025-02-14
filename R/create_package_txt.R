#' Use grateful to generate a paragraph of R packages
#'
#' @name create_package_txt
#' @description Uses grateful to generate a paragraph of R packages that can be used in a RMD document.
#' @param excludes character string of packages to exclude from the paragraph.
#' @return A txt file output and returns that string of text.
#'
#' @export
create_package_txt <- function(excludes = NULL, rstudioVersion = "2024.12.0+467"){

  # library(grateful)
  # library(stringr)
  # library(dplyr)

  dir.create(here::here("notebook", "manuscript"), showWarnings = FALSE)

  packages <- grateful::scan_packages()

  if(!is.null(excludes)){
    packages <- packages[!packages$pkg %in% excludes,]
  }

  # will make a .bib file that will be used for the full reference list
  packageCitations <- grateful::get_pkgs_info(packages$pkg,
                                              out.dir = here::here("notebook", "manuscript"),
                                              bib.file = "packages_refs",
                                              include.RStudio = TRUE)

  packagesDataframe <- packageCitations %>%
    rowwise() %>%
    mutate(inTextString =
             paste0(pkg, " (v.", version, ") [@", paste0(citekeys, collapse = ";@"), "]")
    ) %>%
    dplyr::select(-citekeys)

  # if(any(packagesDataframe$pkg == "tidyterra")){
  #   # tidyterra is for some reason missing a citekey to generate anything in-text,
  #   # we can just manually add that here.
  #   packagesDataframe[packagesDataframe$pkg == "tidyterra",]$inTextString <-
  #     sub("@", "@R-tidyterra", packagesDataframe[packagesDataframe$pkg == "tidyterra",]$inTextString)
  # }

  # issues with rstudio in targets pipeline, deal with it as anothter step
  packagesDataframe <- packagesDataframe %>%
    rbind(data.frame(
      pkg = "R Studio",
      version = rstudioVersion,
      citekeys = "rstudio"
    ) %>%
      mutate(inTextString =
               paste0(pkg, " (v.", version, ") [@", paste0(citekeys, collapse = ";"), "]")
      ) %>%
      dplyr::select(-citekeys)
    )

  writeLines("@Manual{rstudio,
    title = {{RStudio}: Integrated Development Environment for R},
    author = {{Posit team}},
    organization = {Posit Software, PBC},
    address = {Boston, MA},
    year = {2024},
    url = {http://www.posit.co/},
  }", here::here("notebook", "manuscript", "packagesRstudio_refs.bib"))

  if(any(packagesDataframe$pkg == "INLA")){
    # INLA has huge amounts of cites, we'll remove some of the INLA citations to
    # focus on the most relevant. INLA citation docs suggest citing some of the ones
    # they provide so I think this is ok.
    packagesDataframe$inTextString[packagesDataframe$pkg == "INLA"] <-
      gsub("@INLA2009a;|@INLA2011c;|;@INLA2017e;@INLA2018f;@INLA2016g;@INLA2017h;@INLA2018i",
           "",
           packagesDataframe$inTextString[packagesDataframe$pkg == "INLA"])
  }

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
      TRUE ~ "Other packages we used were"
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

  # make sure base is written as R in text
  packagesDataframe[packagesDataframe$pkg == "base",]$inTextString <-
    sub("^base", "R", packagesDataframe[packagesDataframe$pkg == "base",]$inTextString)

  # make the last package in a section have an "and" before it
  packagesDataframe <- packagesDataframe %>%
    group_by(purpose) %>%
    summarise(inText = paste0(paste0(inTextString[1:n()-1], collapse = ", "), ", and ",
                              inTextString[n()])) %>%
    ungroup() %>%
    mutate(inTextFull = paste0(purpose, " ", inText, "."))

  # merge into a single line
  packageParagraph <- paste0(packagesDataframe$inTextFull, collapse = " ")
  # export as a txt file
  writeLines(packageParagraph, here::here("notebook", "manuscript", "packageParagraph.txt"))

  ## add the below code chunk in RMD to read in and place the txt paragraph
  # ```{r packageText, child="packageParagraph.txt", eval=TRUE}

  # ```

}
