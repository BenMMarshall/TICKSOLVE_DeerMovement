---
title: "Roe Deer Movement"
author1: "Benjamin Michael Marshall*"
author2: "---**"
affiliation1: "---"
affiliation2: "---"
corresponding1: "benjaminmichaelmarshall@gmail.com"
corresponding2: "---"
date: "2025-02-05"
output:
  bookdown::pdf_document2:
    template: main.tex
    keep_tex: true
    highlight: monochrome
    fig_caption: true
    dev: pdf
    extra_dependencies: ["float"]
  bookdown::html_document2:
    theme: yeti
    highlight: monochrome
    fig_caption: true
link-citations: yes
linkcolor: gray
bibliography: [deerMovement_refs.bib, packages_refs.bib]
# csl: peerj.csl
editor_options: 
  markdown: 
    wrap: sentence
abstract: abstract text
keywords: Movement ecology, step selection function, poisson, habitat preference, habitat selection, animal movement, roe deer
---

# Introduction

# Methods

## Study Area

<!-- resampling of data, overview of data quantity -->
<!-- removed first week of data -->

## Home Range Esimation

We estimated roe deer home range using autocorrelated kernel density estimators [@ctmm; @Calabrese2016; @Fleming2015; @Fleming2017].
This process consisted of fitting a number of continuous time movement models to an individuals movement data, selecting the best fitting movement model, and extracting a suitable range contour from the resulting utilisation distribution.
We fit the following models (following the default process provided by the ctmm package): OU, OUF, IID etc here.
We used AIC to determine the best fitting movement model on an individual basis, and used that single best fitting model for all further estimations.

Before committing to the estimations of home range size we examined whether the roe deer exhibited stable ranges through the visual inspection of variograms.
A stable range should be reveal by a clear asymptote in the variogram.
We paired these visual inspections with a judgement of effective sample size to help gasuge our confident in the area estimates (DEFINTION OF ESS HERE WOULD BE GOOD)  <!--CITE-->.
All our individuals showed effective sample sizes _____, leading us to be confident in overall home range estimates.

[PMREL goes in here somewhere] [@silva_autocorrelationinformed_2022]

Having determine the data suitability for home range estimations, we extracted the 95% and 99% contours from the weighted AKDE estimate, alongside 95% CI surrounding that contour.
We selected 95% as a balance between a generous estimate of home range, while also avoiding the undue influence of the most extremely outlying movements.
To generate an overall home range estimate for UK roe deer, we averaged all home ranges using the weighted mean function provided by the ctmm package [@ctmm; @silva_autocorrelationinformed_2022].
This way the home range mean is weighted by the confidence (i.e., ESS) surrounding each home range.

We retained 99% estimates for guiding the between patch conenctivity to maximise the repeated between patch modelling, ie connecting as many patches as would be likely for an individual roe deer in the course of their life.
We calculated the widest dimension of each home range polygon (or largest polygon if the home range area was non-contiguous), and halved that to serve as a proxy for the distance deer could travel between patches.
A mean of this longest dimension was used to limit which patches could be considered likely to be travelled between by deer.
To confirm this approach, we determined the distance from patch for every deer location that fell outside a patch.
We examined the distribution of these distance values that revealed that 95% of all deer movements fell within the mean of half longest dimension of the 99% home range area.

## Habitat Selection

<!-- To determine habitat selection of roe deer we focused on the Aberdeen sub-sample as those individuals were tracked for longer periods of time providing the opportunity for a stable overall population estimate.  -->
We determined selection using two methods: an integrated step selection function and a poisson model (Muff et al paper CITE).

Both methods require a comparison of use (i.e., GPS locations of the deer) to available points (i.e., randomly generated locations the deer could that travelled).
For each confirmed deer location we generated __ random alternative locations they could have travelled to.
The location of these random locations was governed by two distributions.
A gamma distribution that random step lengths were drawn from, and a von Mises distribution that random turn directions were drawn from.
Both distribution where calibrated (e.g., shape, size, mu, and kappa) by the underlying movement data.
The same data was used in both modelling approaches.

Once all random locations had been generated we extracted a suite of environmental conditions at all those locations.
First was the land use type.
Using the 25m resolution.
[NEED SOMETHING ON RESOLUTION]
We used the land use data provided by UKCEH (CITE AND DETAILS HERE).
We recategoriesd the UKCEH land use categories into __ more general categories that reduced instances of limited interaction with the deer movement data thereby aiding habitat selection model convergence and avoided extreme, unstable selection estimates.
<!-- also made the results compatible with efforts to model deer tick dymanics -->
We also acquired woody linear feature (i.e., hedgerows) data from UKCEH (CITE AND DETAILS).
We converted the polygon spatial data into a raster, where 1 == hedgerow, and used that rasterisation to generate a distance to hedgerow raster for the entire study landscape.
We conducted the same process to create a distance to woodland raster, where we calculated the distance from any area the UKCEH land use data classed as deciduous or coniferous woodland.
These distance rasters allowed for easy extraction of the distance to the nearest hedgerow and woodland for all locations.
We acquired road data from OS map open GOV licensed (CITE HERE with DETAILS). 
We created a binary variable describing crossing events for all steps, with all steps that crossed one or more of the roads being classed as 1.
This binary variable allowed us to estimate the likelihood deer cross a road and therefore the level of barrier roads present.

<!-- Integrated Step Selection Functions -->
For the iSSFs we ran a single model for each individual deer.
<!-- make sure to mention whehther it was all deer or just aberdeen deer - SURELY SHOUDL ONLY BE ABERDEEN SO WE CAN USE WESSEX AS A VALID INDEPENDENT VALIDATION SET -->
The model formulae consisted of land use (__ category variable, with decidious woodland placed as the reference category), distance to woodland (continous in m), distance to hedgerow (continuous in m), road crossing (binary).
In addition to these selection focused predictors, we several movement predictors including step length, turn angle, log step length, cos turn angle, as well as the interaction between step length and land use, step length and distance to woodland.
<!-- make sure this all matches up with what is actually run -->
<!-- Poisson Model -->
The Poisson model was a single population level model that included all the individuals data.
The formula was largely the same as the iSSF, except for the removal of the movement predictors and interactions, and the addition of fixed gaussian process to allow for selection effect to vary by individual. <!-- check termiloogy here  -->
Previous work has demonstrate that the inclusions of those aid bias reduction in iSSFs but can lead to increased bias and more variable estimates when placed in the Poisson model approach.
<!-- no movement aspects in formula, can cite Muff et al and Marshall et al (preprint), suggesting that its leads to more vairable estimates  -->

## Translating Preferences into Connectivity

Once we had estimates of selection and effect for all the environmental aspects of interest, we spatially mapped those estimated covariates back onto the landscape, resulting in a map of resistance.
For the iSSF the covariates we used consisted of the median estimate for each predictor, and as some required a required a step length and turn angle, we elected to use the mean of both to generate all resistance maps.
For both methods we ignored the uncertainty surrounding the selection estimates, instead relying on just the point estimates for the resistance mapping.
<!-- invert logit to standardise them and flip them to movement ease -->

<!-- Random Shortest Paths -->
We used two methods to simulate potential connectivity across the landscapes.
The first was random shortest paths, that consists of generating random walks between locations.
This method was used to show the connectivity of areas used by RIENDEER(?), and offers a mechanism for calibrating the connectivity maps to both the habitat selection and movement path characteristics.
<!-- CITE THE REINDEER MOVEMENT PAPER -->
We used habitat patches as the sources of our random locations.
Each patch had __ start and end locations generated within it, we then ran walks from these locations to all other patch locations within 750 m of that patch.
<!-- excluded patches smaller than the smallest selected patch area -->
We elected not to generate start or end points in patches less than 10ha in areas, following <!-- cite -->. 
This was due to these patches being unlikely to host deer populations and the reduction in origin patches aided computational costs.
We selected 750 m as that represented the mean longest axis of the 99% HR estimate of the deer (excluding outlying non-contiguous portions of the area polygons).
Once every walk was complete, the resulting rasters describing the likelihood of a deer crossing a cell are complied into a single landscape raster describing connectivity and standardised between 0 and 1 (where 0 is low connectivity and 1 is high connectivity).
A key consideration in these walks is how random the paths are.
We elected to run walks at __ different levels of randomness (theta; with 1 being close to a least cost path, and ____ being the most random and diffuse walks).
To determine what level of randomness best reflected the realised movements of the deer, we compared the resulting connectivity maps to dynamic Brownian Bridge Movement Models.
Dynamic Brownian Bridge Movement Models run a series of random walks between defined start and end points, from the summation of these walks you can extract a rasterised occurrence distribution.
Critically the dBBMM walks are calibrated to the movement capacity of the animal through rolling window (i.e., a number of data points) that summarises the movement rate during that time.
Additionally within that window, a margin (a subset of data points) is used to detect any sudden changes in movement capacity that may be reflect of behavioural/movement mode changes.
Therefore the dBBMMs provide a estimate of how diffuse the movements could be between known locations.
We ran dBBMMs for all roe deer with a window size of __ and a margin of __, that provided estimates of motion variance on roughly a weekly sliding window with the allowance of sudden motion variance changes day to day (margin).
We compared the dBBMMs to the connectivity maps constructed with varying levels of randomness, and used mean squared least [EXTACT TERM HERE] to determine which level of randomness best fit the movement data.
<!-- Circuitscape -->
<!-- The second method we used to create connectivity maps was circuitscape. -->
<!-- Explainer why circuitscape is good to use in two sentences here -->
<!-- Circuitscape involve treating the landscape as wired circuit with differing levels of resistance between nodes (inputs and grounds). -->
<!-- In much the same way as the random shortest paths, we selected pairs of nodes (habitat patches) within ___ m to run circuitscape between. -->

## Validation

We examined whether the connectivity maps generated matched the observed movements of roe deer using a logistic regression.
The model was supplied with the known locations of deer as well as ___ randomly generated points across the landscape, all of which had associated connectivity values.
We formulated the model to predict whether a point was used or random based on the connectivity values, and we included a random effect for deer ID.
The expectation was that the model coefficients would indicate that deer locations were positively associated with higher connectivity values.


<!-- ## Projecting to Wessex -->
<!-- We elected to retain the Wessex Roe Deer data for model validation, as compared to Aberdeen the tracking was conducted for a shorter period with fewer individuals. -->
<!-- We compared the known locations of deer to the connectivity maps to test their ability to predict deer movements. -->
<!-- precision recall?? might not be possible as no binary -->
<!-- Root Mean Squared Error (RSME) -->
<!-- logistic regression -->

# Results

# Discussion

## Conlcusions

# Acknowledgements


# Software availablity


For all analysis we used R (v.4.2.2) [@base], and R Studio (v.2024.09.1+394) [@rstudio]. For analysis of animal movement data we used amt (v.0.2.2.0) [@amt], ctmm (v.1.2.0) [@ctmm], and move (v.4.2.4) [@move]. For general data manipulation we used glue (v.1.7.0) [@glue], sjmisc (v.2.8.10) [@sjmisc], tidyverse (v.2.0.0) [@tidyverse], and units (v.0.8.5) [@units]. For project and code management we used here (v.1.0.1) [@here], tarchetypes (v.0.9.0) [@tarchetypes], and targets (v.1.9.1) [@targets]. For visualisation we used the following as expansions from the tidyverse suite of packages: ggdist (v.3.3.2) [@ggdist2024a;@ggdist2024b], ggridges (v.0.5.6) [@ggridges], ggtext (v.0.1.2) [@ggtext], patchwork (v.1.2.0) [@patchwork], and scales (v.1.3.0) [@scales]. Other pacakges we used were boot (v.1.3.28) [@boot2021;@boot1997], crew (v.0.10.2) [@crew], and usethis (v.2.2.3) [@usethis]. To generate typeset outputs we used bookdown (v.0.41) [@bookdown2024;@bookdown2016], and rmarkdown (v.2.28) [@rmarkdown2024;@rmarkdown2018;@rmarkdown2020]. To manipulate and manage spatial data we used gdistance (v.1.6.4) [@gdistance], raster (v.3.6.26) [@raster], sf (v.1.0.16) [@sf2023;@sf2018], sp (v.2.1.4) [@sp2005;@sp2013], terra (v.1.7.78) [@terra], and tidyterra (v.0.6.0) [@R-tidyterra]. To run models and explore model outputs we used effects (v.4.2.2) [@effects2019;@effects2018;@effects2003;@effects2009], INLA (v.23.4.24) [@@INLA2013b;@@INLA2015d;@INLA2017e;@INLA2018f;@INLA2016g;@INLA2017h;@INLA2018i], lme4 (v.1.1.36) [@lme4], and performance (v.0.13.0) [@performance].

<!-- CIRCUITSCAPE -->
<!-- circuitscape, juliacall -->

# Data availabilty

# Author Contributions

# Supplementary Material

\clearpage

# References
