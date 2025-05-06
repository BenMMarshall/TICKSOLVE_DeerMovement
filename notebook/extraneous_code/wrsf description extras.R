## Habitat Adjusted Home Ranges

To supplement our initial estimates of home range size, we generated estimates that take into account aspects of the discovered habitat selection.
To accomplish this, we ran weighted resource selection functions [wRSF; @alston_mitigating_2023].
This approach uses autocorrelation-adjusted weights to estimate the availability of habitat types.
We passed these models the main co-variates the population Poisson model identified as important: distance to woodland (continuous), Tall Grassland (binary), and Open Shrubland (binary).
Once we had estimates of selection, we adjusted our original best perfomring autocorrelated kernel density estimates using the wRSF models.
We also estimated a mean selection for each variable, using the weighted ctmm mean functionality.

The habitat selection adjusted home ranges were on average `r round(HRadjDiff$meanDiff, digits = 2)` ±SD`r round(HRadjDiff$sdDiff, digits = 2)` ha larger non-adjusted AKDE ranges.
The largest difference was only `r round(HRadjDiff$maxDiff, digits = 2)` ha.

Weighted resource selection models largely supported and agreed with the Poisson model estimates (Fig. \@ref(fig:wrsfEffects)).
There was a clear effect of distance from woodland at the population level, even if it was universally significant at an individual level.
Only two individuals show an opposite effect, suggesting a (non-significant) selection for areas farther from woodland.
Similarly, we see clear agreement between the approach for the effects of Tall Grassland.
The effects of Open Shurbland and Tall Grassland were more mixed, with roughly equal number of individuals selected for as against both habitat types.
For Open Shurbland the overall mean population selection disagreed with Poisson model, suggesting overall avoidance.
We suspect this lack of agreement is due to a single individual's (Roe 12) strong avoidance.
Tall Grassland by contrast, did agree with the Poisson model on a population level, but the mix of individuals selecting for and against it leaves the overall conclusion less clear.
