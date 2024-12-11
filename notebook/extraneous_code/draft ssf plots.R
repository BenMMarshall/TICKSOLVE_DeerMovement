
https://conservancy.umn.edu/server/api/core/bitstreams/90c4b460-93ee-42e2-96dd-f695b633d9b0/content

# targets::tar_load("tar_ssf_data")
targets::tar_load("tar_ssf_models")

focalModel <- tar_ssf_models[[1]]

## Interpreting Habitat-Selection Parameters

# We begin by exploring the coefficients in the fitted model using the `summary()` function:

summary(m1)

# For now, we will just focus on the habitat-selection coefficients, i.e., `popden`, `elevation`, and the two `landuseC` coefficients. The interpretation of the parameters is very similar to the RSF interpretation (Supplementary Appendix A) but at a local scale with habitat availability determined by the selection-free movement kernel. We can exponentiate habitat-selection parameters in a fitted step-selection model to compare the relative rates of use of two locations that differ by 1 unit of the explanatory variable but are otherwise equivalent - i.e., they should be equally accessible and have identical values for all other explanatory variables.

### Calculating Relative Selection Strength (RSS) for Two Locations

# Calculating the relative use of location $s_1$ versus location $s_2$ is fairly straightforward when $s_1$ and $s_2$ share the same values for all but one covariate (see e.g., Supplementary Appendix A). For more complex scenarios,  we have implemented a function in `amt` that will calculate the log-relative intensity [referred to as the log-Relative Selection Strength, or log-RSS, by @avgar2017relative]  by leveraging the generic `predict()` function available for many ordinary model classes (e.g., `lm()` or `glm()`) models in `R`. Regardless of whether there are interactions or polynomial terms, the function will correctly calculate log-RSS for you. If you prefer to quantify relative-use (i.e., RSS), you can simply exponentiate the results.

# Here we demonstrate the use  of`log_rss()` with the simple example presented in equation 8 of the main text. Suppose we have two locations with the following covariates:

# *  $s_1$: `elevation_end = 3`, `popden_end = 1.5`, `landuseC_end = "wet"`
# *  $s_2$: `elevation_end = 2`, `popden_end = 1.5`, `landuseC_end = "wet"`

# We want to calculate the log-RSS for a step ending in $s_1$ versus a step ending in $s_2$, assuming $s_1$ and $s_2$ are equally accessible to the animal.

# The function `log_rss()` expects the two locations to be formatted as separate `data.frame` objects. Each `data.frame` must include all covariates used to fit the model. Furthermore, `factor` variables should have the same `levels` as the original data.

# # data.frame for s1
# s1 <- data.frame(
#   elevation_end = 3,
#   popden_end = 1.5,
#   landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
#   sl_ = 100,
#   log_sl_ = log(100),
#   cos_ta_ = 1)
#
# # data.frame for s2
# s2 <- data.frame(
#   elevation_end = 2,
#   popden_end = 1.5,
#   landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
#   sl_ = 100,
#   log_sl_ = log(100),
#   cos_ta_ = 1)
# ```
#
# Now that we have specified each location as a `data.frame`, we can pass them along to `log_rss()` for the calculation. The function will return an object of class `log_rss`, which is also more generally a `list`. The `list` element `"df"` contains a `data.frame` which contains the log-RSS calculation and could easily be used to make a plot when considering relative selection strength across a range of environmental characteristics.
#
# ```{r basic logrss 1}
# lr1 <- log_rss(m1, x1 = s1, x2 = s2)
# lr1$df
# ```

# As we will see below, this function is designed to be able to consider several locations as `x1`, relative to a single location in `x2`. Because the `[["df"]]` element of the `log_rss` object is designed for plotting, it includes the value of all the covariates at location `x1` by appending `_x1` to the end of the predictor names. The `log_rss` object also contains the original `x1` and `x2` as elements, along with the model formula.

# We can see that the log-RSS for this example is `r lr1$df$log_rss`, which is equivalent to $\beta_{elevation}$ since we have considered the simple case in which $s_1$ and $s_2$ differ by only a single unit of elevation. Now that we know how the function works, we can consider more complex cases.

# Suppose we are interested in making a figure showing the relative selection strength our fisher using a range of elevations. The function `log_rss()` can accept a `data.frame` with many rows as as its first argument, `x1`, to go with a single comparison location, `x2` (i.e., `x2` must only have 1 row).

# Make a new data.frame for s1
s1 <- data.frame(
  elevation_end = seq(from = -2, to = 2, length.out = 200),
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s2
s2 <- data.frame(
  elevation_end = 0, # mean of elev, since we scaled and centered
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# Calculate log-RSS
lr2 <- log_rss(m1, s1, s2)

# Plot using ggplot2
ggplot(lr2$df, aes(x = elevation_end_x1, y = log_rss)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  xlab("Elevation (SD)") +
  ylab("log-RSS vs Mean Elevation") +
  theme_bw()

# The line depicts the log-RSS between each value of $s_1$ relative to $s_2$. Note that these locations differ only in their values of `elevation`, and the log-RSS is equal to 0 when the two locations are identical (i.e., when `elevation` = 0). If you prefer to plot RSS to log-RSS, simply exponentiate the y-axis (and move your horizontal line for no selection to $exp(0) = 1$).

# Plot using ggplot2
ggplot(lr2$df, aes(x = elevation_end_x1, y = exp(log_rss))) +
  geom_line(size = 1) +
  geom_hline(yintercept = exp(0), linetype = "dashed", color = "gray30") +
  xlab("Elevation (SD)") +
  ylab("RSS vs Mean Elevation") +
  theme_bw()

### Bootstrap Confidence Intervals

# The RSS plots above do not give any indication of model uncertainty. One
# method for constructing confidence intervals associated with the RSS
# predictions is via non-parametric bootstrapping [@fieberg2020resampling]. When
# using the argument `ci="boot"`  to the `log_rss()` function, the function
# repeatedly resamples strata with replacement, refits the model, and calculates
# log-RSS. Quantiles of the bootstrap log-RSS statistics are used to  construct,
# by default, a 95% confidence interval around the original log-RSS calculation.
# The size of the confidence interval and number of bootstrap iterations can be
# controlled by the user by changing the `ci_level` and `n_boot` arguments. `r
# colorize("Warning: this code can take some time to execute.", "red")`
# Calculate log-RSS
lr2_ci_boot <- log_rss(m1, s1, s2, ci = "boot", ci_level = 0.95, n_boot = 1000)

# Check the header of the data.frame
head(lr2_ci_boot$df)

# You can see that the resulting `lr2_ci_boot`  data.frame now has columns `lwr`
# and `upr`, indicating the lower and upper bounds of the confidence interval.
# We can now add this confidence envelope to our plot.

ggplot(lr2_ci_boot$df, aes(x = elevation_end_x1, y = log_rss)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              linetype = "dashed",
              color = "black", fill = "gray80", alpha = 0.5) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  xlab("Elevation (SD)") +
  ylab("log-RSS vs Mean Elevation") +
  theme_bw()

### Large-Sample Confidence Intervals

# To avoid the computational time required to calculate bootstrap confidence
# intervals, you may decide that using a large-sample-based confidence interval
# is appropriate. We can use the standard errors from our fitted model to
# estimate these confidence intervals based on a normal approximation to the
# sampling distribution of $\hat{\beta}$.

lr2_ci_se <- log_rss(m1, s1, s2, ci = "se", ci_level = 0.95)

# Check the header of the data.frame
head(lr2_ci_se$df)
# We see, in this case, that the two confidence intervals (bootstrap, normal-based) lead to similar conclusions.

# Add "type" label to both predictions data.frames
lr2_ci_boot$df$type <- "Bootstrap"
lr2_ci_se$df$type <- "SE"

# Combine two prediction data.frames
lr2_ci_both <- rbind(lr2_ci_boot$df, lr2_ci_se$df)

# Plot
ggplot(lr2_ci_both, aes(x = elevation_end_x1, y = log_rss,
                        group = factor(type),
                        fill = factor(type))) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              linetype = "dashed", color = "black", alpha = 0.25) +
  geom_line(size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  scale_fill_manual(name = "CI Type", breaks = c("Bootstrap", "SE"),
                    values = c("blue", "orange")) +
  xlab("Elevation (SD)") +
  ylab("log-RSS vs Mean Elevation") +
  theme_bw()

## Interpreting Movement Parameters

# Previously, we viewed tentative estimates of parameters describing the
# distribution of step-lengths and turn-angles.  When estimating these
# parameters, however, we did not account or adjust for the influence of habitat
# selection. Including movement characteristics (`sl_`, `log_sl_`, and
# `cos_ta_`) in our iSSF allows us to update our estimates of these parameters,
# which describe selection-free movement distributions. Mathematical formulas
# required to perform these updates are given in Supplementary Appendix C. Here,
# we will see how to update the distributions using the functions from `amt`.

### Update Step-Length Distribution

# In our model, we included parameters for the step length (`sl_`) and its
# natural logarithm (`log_sl_`). We included these parameters to update the
# scale and shape parameters of our tentative gamma distribution, respectively.
# Different terms will need to be included in the iSSF, depending on the assumed
# step-length distribution (for details, see Supplementary Appendix C).

# For our basic model -- where the movement parameters are *not* interacting
# with any covariates -- `amt` has a simple function to update the step-length
# distribution.

# Update the distribution
updated_sl <- update_sl_distr(m1)

# Check the class
class(updated_sl)

# Print
print(updated_sl)

# We can see that the updated distribution object has classes that are designed for working with step-length/turn-angle distributions in `amt`. In the most general sense, it is a `list`, so you can access its named components either with `$name` or `[["name"]]`, as you would with any other list. But it is also of class `amt_distr`, a general class for working with distributions in `amt`; class `sl_distr`, a more specific class for working with step-length distributions; and class `gamma_distr`, an even more specific class for working with gamma distributions. We can see that the object has a name (`"gamma"`) and the appropriate parameters for the gamma distribution -- shape (`$params$shape`) and scale (`$params$scale`).

# We can compare the tentative and updated distributions to see how accounting for habitat-selection changes our estimates of the selection-free step-length distribution.

# data.frame for plotting
plot_sl <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_sl$x <- seq(from = 0, to = 400, length.out = 100)

# y-axis is the probability density under the given gamma distribution
# For the tentative distribution
plot_sl$tentative <- dgamma(
  x = plot_sl$x,
  shape = m1$sl_$params$shape,
  scale = m1$sl_$params$scale)

# For the updated distribution
plot_sl$updated <- dgamma(
  x = plot_sl$x,
  shape = updated_sl$params$shape,
  scale = updated_sl$params$scale)

# Pivot from wide data to long data
plot_sl <- plot_sl %>%
  pivot_longer(cols = -x)

# Plot
ggplot(plot_sl, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  scale_color_manual(name = "Distribution",
                     breaks = c("tentative", "updated"),
                     values = c("blue", "orange")) +
  theme_bw()

# In this case, our estimates do not change very much. We might have anticipated this result because the coefficients associated with `sl_` and `log_sl_` were not statistically significant in our fitted model (`m1`).

### Update Turn-Angle Distribution

# In our model, we included cosine of the turn angle (`cos_ta_`) to update the concentration parameter of our tentative von Mises distribution. Again, see Supplementary Appendix C for details of updating turn-angle distributions. Here, we will see how to use `amt` to update our tentative parameters and retrieve the selection-free turn-angle distribution.

# We can use the `update_ta_distr` function in `amt` to update our turn-angle distribution:

# Update the distribution
updated_ta <- update_ta_distr(m1)

# Check the class
class(updated_ta)

# Print
print(updated_ta)

# Again, we see that the updated distribution object has several classes associated with it; it is a `list` and also of class `amt_distr`. This time it is also of class `ta_distr`, indicating that it is a turn-angle distribution, and more specifically it is a `vonmises_distr`. We can also access the appropriate parameters for the von Mises distribution -- concentration (`$params$kappa`) and mean (`$params$mu` -- often fixed to 0 under the assumption that animals are no more likely to turn left than right).

# We can again compare the tentative and updated distributions to see how accounting for habitat-selection changes our estimates of the selection-free turn-angle distribution.

# data.frame for plotting
plot_ta <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_ta$x <- seq(from = -1 * pi, to = pi, length.out = 100)

# y-axis is the probability density under the given von Mises distribution
# For the tentative distribution
plot_ta$tentative <- circular::dvonmises(
  x = plot_ta$x,
  mu = m1$ta_$params$mu,
  kappa = m1$ta_$params$kappa)

# For the updated distribution
plot_ta$updated <- circular::dvonmises(
  x = plot_ta$x,
  mu = updated_ta$params$mu,
  kappa = updated_ta$params$kappa)

# Pivot from wide data to long data
plot_ta <- plot_ta %>%
  pivot_longer(cols = -x)

# Plot
ggplot(plot_ta, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  coord_cartesian(ylim = c(0, 0.25)) +
  xlab("Relative Turn Angle (radians)") +
  ylab("Probability Density") +
  scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi),
                     labels = c(expression(-pi, -pi/2, 0, pi/2, pi))) +
  scale_color_manual(name = "Distribution",
                     breaks = c("tentative", "updated"),
                     values = c("blue", "orange")) +
  theme_bw()

# Again, we see that our estimates do not change very much, which is not surprising given that the coefficient associated with `cos_ta_` was not statistically significant in our fitted model (`m1`).

### Calculating RSS for Movements

# Suppose we are interested in how likely our fisher is to take a 50 m step versus a 100 m step. We can propose two hypothetical locations, $s_1$ and $s_2$, which have exactly the same habitat attributes but differ in their distance from the fisher's starting location.

# We do not want to use `log_rss()` for this purpose, because the fitted movement parameters in our iSSF represent updates to the tentative step-length distribution and not the distribution itself. Instead, we want to use the updated PDF of the step-length distribution directly. We can calculate the likelihood under the selection-free step-length distribution of taking a 50 m step and the likelihood of taking a 100 m step. Then we divide for an estimate of how much more likely a 50 m step is than a 100 m step.

like50 <- dgamma(50,
                 shape = updated_sl$params$shape,
                 scale = updated_sl$params$scale)
like100 <- dgamma(100,
                  shape = updated_sl$params$shape,
                  scale = updated_sl$params$scale)
like50/like100

# Thus, we conclude Lupe is nearly 3 times more likely to take the shorter (50 m versus 100 m) step.

# More Complex Models

## Non-Linear Relationships: Adding Quadratic Terms

# Suppose, now, that we hypothesize that our fisher selects for intermediate elevations and avoids locations with extremely low or high elevations. We could use splines or polynomials to capture this non-linear effect. Here, we consider adding a quadratic term for elevation in our model.

m2 <- ssf_dat %>%
  fit_issf(case_ ~ popden_end + elevation_end + I(elevation_end^2) +
             landuseC_end + sl_ + log_sl_ + cos_ta_ +
             strata(step_id_), model = TRUE)


# In our basic model, we could interpret the fitted $\beta_{elevation}$ as capturing the log relative selection strength (or log-RSS) associated with a one unit change in elevation. With the addition of the quadratic term, the log-RSS for one unit change in elevation is no longer constant. <!---The effect is greater the farther we are from the vertex of the parabola we just fit. JF: Doesn't this depend on where the peak is and whether the parabola is facing up/down? Assuming so, I would keep this text more general... BJS: well, it is generally true that the slope is 0 at the vertex and increases in magnitude (negative on one side, positive on the other, depending on up/down), but I agree that this is probably a little too much detail.---> To illustrate this point, we compare the log-RSS for a one unit change in elevation for our two models (`m1` has just a linear term for elevation; `m2` has a quadratic term for elevation), using two different reference values for `elevation`:

# -  $s_1$: `elevation_end = 3`, `popden_end = 1.5`, `landuseC_end = "wet"`
# -  $s_2$: `elevation_end = 2`, `popden_end = 1.5`, `landuseC_end = "wet"`
# -  $s_3$: `elevation_end = -1`, `popden_end = 1.5`, `landuseC_end = "wet"`
# -  $s_4$: `elevation_end = -2`, `popden_end = 1.5`, `landuseC_end = "wet"`

# We can see that $s_1$ and $s_2$ are separated by 1 unit of elevation, and $s_3$ and $s_4$ are also separated by 1 unit of elevation. Let's compare the two sets of locations.

# data.frame for s1
s1 <- data.frame(
  elevation_end = 3, # mean of elev, since we scaled and centered
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s2
s2 <- data.frame(
  elevation_end = 2, # mean of elev, since we scaled and centered
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s3
s3 <- data.frame(
  elevation_end = -1, # mean of elev, since we scaled and centered
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s4
s4 <- data.frame(
  elevation_end = -2, # mean of elev, since we scaled and centered
  popden_end = 1.5,
  landuseC_end = factor("wet",  levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# Calculate log-RSS under m1
lr_m1_s1s2 <- log_rss(m1, s1, s2)
lr_m1_s3s4 <- log_rss(m1, s3, s4)

# Calculate log-RSS under m2
lr_m2_s1s2 <- log_rss(m2, s1, s2)
lr_m2_s3s4 <- log_rss(m2, s3, s4)

# Compare
data.frame(
  "model" = c("linear", "quadratic"),
  "logRSS_s1_s2" = round(c(lr_m1_s1s2$df$log_rss, lr_m2_s1s2$df$log_rss), 3),
  "logRSS_s3_s4" = round(c(lr_m1_s3s4$df$log_rss, lr_m2_s3s4$df$log_rss), 3))


# Notice that the reference value for `elevation_end` (`x1`) does not impact the log-RSS associated with a 1 unit change when the model is linear. If the model includes the quadratic term, however, then the reference `elevation` value is important.

# If we plot a range of log-RSS predictions, we can see the slope slowly leveling off as the value of `elevation` increases.

# Make a new data.frame for s1
s1 <- data.frame(
  elevation_end = seq(from = -2, to = 2, length.out = 100),
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s2
s2 <- data.frame(
  elevation_end = 0, # mean of elev, since we scaled and centered
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# Calculate log-RSS
lr_m2 <- log_rss(m2, s1, s2)

# Plot using ggplot2
ggplot(lr_m2$df, aes(x = elevation_end_x1, y = log_rss)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  xlab("Elevation (SD)") +
  ylab("log-RSS vs Mean Elevation") +
  theme_bw()

# Our hypothesis that Lupe selects intermediate elevations doesn't seem to have support here (at least over the range of elevations within Lupe's MCP), and the non-linear effect that is estimated is rather muted. Nevertheless, quadratic terms are often useful for predictors that may represent *conditions* [see main text; @Matthiopoulos2015].

## Movement and Habitat Interactions

# In all of our previous models, we have assumed that a single step-length distribution and a single turn-angle distribution was sufficient for modeling selection-free movements. However, considering interactions between movement properties can provide valuable insight into an animal's motivations for moving [e.g., @dickie2020corridors]. In this section, we will demonstrate how to fit and interpret models that include interactions between  movement characteristics and  categorical or continuous explanatory variables capturing environmental characteristics.

### Interactions Between Movement Characteristics and Categorical Variables

# Suppose we hypothesize that our fisher moves differently depending on the land-use class that it is in at the start of the movement step. We can fit a model where all of the movement characteristics (`sl_`, `log_sl_`, an d`cos_ta_`) interact with `landuseC` as follows:

m3 <- ssf_dat %>%
  fit_issf(case_ ~ popden_end + elevation_end +
             landuseC_end +
             sl_ + log_sl_ + cos_ta_ +
             landuseC_start:(sl_ + log_sl_ + cos_ta_) +
             strata(step_id_), model = TRUE)

# Under this model, Lupe's selection-free movement kernel, $\phi(s,s';\gamma(\Delta t,t,X(s,t)))$, depends on the value of `landuseC` at the start of the movement step and Lupe's step-selection function, $w(X(s,t+\Delta t);\beta(\Delta t))$, depends on the value of`landuseC` at the end of the movement step.
#
# Let's look at how the step-length distributions vary depending on the land-use class. Earlier, we saw simple `amt` functions to update step-length and turn-angle distributions from a fitted model. However, these easy-to-use functions  cannot accommodate more complex models that include interactions between habitat covariates and movement characteristics. Instead, we will have to use a separate set of functions that require manual specification of the appropriate $\beta$'s (see `?update_distr_man`).
#
# Before we demonstrate these functions, let's take a look at the parameters in the fitted model. For updating the step-length distribution, pay attention to those parameters that interact with `sl_` or `log_sl_`.

summary(m3)

# Recall that one of the `landuseC` categories is treated as a reference category (in this case, `"forest"`). We update the parameters of the step-length distribution for the reference category with the main effect parameters (i.e., `sl_` and `log_sl_`). For the other categories, we need to add the appropriate interaction terms to these parameters.

# Forest step-length distribution
forest_sl <- update_gamma(
  dist = m3$sl_,
  beta_sl = m3$model$coefficients["sl_"],
  beta_log_sl = m3$model$coefficients["log_sl_"])

# Grass step-length distribution
grass_sl <- update_gamma(
  dist = m3$sl_,
  beta_sl = m3$model$coefficients["sl_"] +
    m3$model$coefficients["sl_:landuseC_startgrass"],
  beta_log_sl = m3$model$coefficients["log_sl_"] +
    m3$model$coefficients["log_sl_:landuseC_startgrass"])

# Wet step-length distribution
wet_sl <- update_gamma(
  dist = m3$sl_,
  beta_sl = m3$model$coefficients["sl_"] +
    m3$model$coefficients["sl_:landuseC_startwet"],
  beta_log_sl = m3$model$coefficients["log_sl_"] +
    m3$model$coefficients["log_sl_:landuseC_startwet"])

We can follow a similar process with the turn-angle distribution.

# Forest turn-angle distribution
forest_ta <- update_vonmises(
  dist = m3$ta_, beta_cos_ta = m3$model$coefficients["cos_ta_"])

# Grass turn-angle distribution
grass_ta <- update_vonmises(
  dist = m3$ta_,
  beta_cos_ta = m3$model$coefficients["cos_ta_"] +
    m3$model$coefficients["cos_ta_:landuseC_startgrass"])

# Wet turn-angle distribution
wet_ta <- update_vonmises(
  dist = m3$ta_,
  beta_cos_ta = m3$model$coefficients["cos_ta_"] +
    m3$model$coefficients["cos_ta_:landuseC_startwet"])

# Now, we can plot the original and updated distributions for each `landuseC`.

# data.frame for plotting
plot_sl <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_sl$x <- seq(from = 0, to = 400, length.out = 100)

# y-axis is the probability density under the given gamma distribution
# Forest
plot_sl$forest <- dgamma(x = plot_sl$x,
                         shape = forest_sl$params$shape,
                         scale = forest_sl$params$scale)
# Grass
plot_sl$grass <- dgamma(x = plot_sl$x,
                        shape = grass_sl$params$shape,
                        scale = grass_sl$params$scale)
# Wet
plot_sl$wet <- dgamma(x = plot_sl$x,
                      shape = wet_sl$params$shape,
                      scale = wet_sl$params$scale)

# Pivot from wide to long data
plot_sl <- plot_sl %>%
  pivot_longer(cols = -x)

# Plot
p1<-ggplot(plot_sl, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  scale_color_manual(name = "Land-use",
                     breaks = c("forest", "grass", "wet"),
                     values = c("forestgreen", "wheat", "blue")) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  theme_bw()


#data.frame for plotting
plot_ta <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_ta$x <- seq(from = -pi, to = pi, length.out = 100)

# y-axis is the probability density under the given gamma distribution
# Forest
plot_ta$forest <- circular::dvonmises(x = plot_ta$x,
                                      kappa = forest_ta$params$kappa,
                                      mu = 0)
# Grass
plot_ta$grass <- circular::dvonmises(x = plot_ta$x,
                                     kappa = grass_ta$params$kappa,
                                     mu = 0)
# Wet
plot_ta$wet <- circular::dvonmises(x = plot_ta$x,
                                   kappa = wet_ta$params$kappa,
                                   mu = 0)

# Pivot from wide to long data
plot_ta <- plot_ta %>%
  pivot_longer(cols = -x)

# Plot
p2 <- ggplot(plot_ta, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  scale_color_manual(name = "Land-use",
                     breaks = c("forest", "grass", "wet"),
                     values = c("forestgreen", "wheat", "blue")) +
  scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi),
                     labels = c(expression(-pi, -pi/2, 0, pi/2, pi))) +
  xlab("Turn Angle (radians)") +
  ylab("Probability Density") +
  theme_bw()

combined <- p1 + p2 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

ggsave("figures/Figure4.png", width = 19, units = "cm", height = 10,
       dpi = 600)

# We see that Lupe tends to take larger, more directed steps when in `grass` and slower and more tortuous steps in `wet` habitat. One possibility is that Lupe tends to travel through `grass` and forage in `forest` and `wet` habitats.

### Continuous Movement Predictors

# We can follow a similar process for continuous movement predictors. Suppose we want to know if Lupe moves differently depending on the elevation she finds herself in. We can fit the following model:

m4 <- ssf_dat %>%
  fit_issf(case_ ~ popden_end + landuseC_end +
             elevation_end +
             sl_ + log_sl_ + cos_ta_ +
             elevation_start:(sl_ + log_sl_ + cos_ta_) +
             strata(step_id_), model = TRUE)

# Again, lets inspect the fitted model

summary(m4)

# We can see that the interactions between the step-length parameters and `elevation` are not detectable, so we shouldn't expect much of an effect of `elevation` on the step-length distribution. However, the relationship with the turn-angle parameter (`cos_ta_`) is stronger, so we might see an effect there. Let's update our step-length and turn-angle distributions and see how they look.
#
# Because we have included interactions between movement characteristics and a continuous covariate, `elevation`, our model allows for an infinite number of selection-free movement kernels -- i.e., Lupe's selection-free movement kernel will depend on the specific value of `elevation` associated with her current location.  For example,  the scale parameter of the gamma distribution,  $\beta_{l}$, is a function of elevation:
#
#   $$\beta_l = \beta_{sl\_} + \beta_{elevation:sl\_} \times elevation$$
#
#   To help visualize the effect of `elevation` on movement, it can be helpful to plot selection-free movement kernels for a range of `elevation` values. Below, we pick three values of `elevation` (-2, 0, 2) which we label ("low", "medium", and "high"), respectively. Remember that we scaled and centered our covariates so these values correspond to the mean and $\pm 2$ standard deviations away from the mean. We'll create an updated gamma step-length distribution for each of these three values of `elevation`:

# Low elevation step-length distribution
low_sl <- update_gamma(
  dist = m4$sl_,
  beta_sl = m4$model$coefficients["sl_"] +
    -2 * m4$model$coefficients["sl_:elevation_start"],
  beta_log_sl = m4$model$coefficients["log_sl_"] +
    -2 * m4$model$coefficients["log_sl_:elevation_start"])

# Medium elevation step-length distribution
med_sl <- update_gamma(
  dist = m4$sl_,
  beta_sl = m4$model$coefficients["sl_"] +
    0 * m4$model$coefficients["sl_:elevation_start"],
  beta_log_sl = m4$model$coefficients["log_sl_"] +
    0 * m4$model$coefficients["log_sl_:elevation_start"])

# Wet step-length distribution
hi_sl <- update_gamma(
  dist = m4$sl_,
  beta_sl = m4$model$coefficients["sl_"] +
    2 * m4$model$coefficients["sl_:elevation_start"],
  beta_log_sl = m4$model$coefficients["log_sl_"] +
    2 * m4$model$coefficients["log_sl_:elevation_start"])

Similarly, we calculate updated turn-angle distributions for each of these 3 values of `elevation`.

# low turn-angle distribution
low_ta <- update_vonmises(
  dist = m4$ta_,
  beta_cos_ta = m4$model$coefficients["cos_ta_"] +
    -2 * m4$model$coefficients["cos_ta_:elevation_start"])

# med turn-angle distribution
med_ta <- update_vonmises(
  dist = m4$ta_,
  beta_cos_ta = m4$model$coefficients["cos_ta_"] +
    0 * m4$model$coefficients["cos_ta_:elevation_start"])

# hi turn-angle distribution
hi_ta <- update_vonmises(
  dist = m4$ta_,
  beta_cos_ta = m4$model$coefficients["cos_ta_"] +
    2 * m4$model$coefficients["cos_ta_:elevation_start"])

# Now, lets plot the results:

#data.frame for plotting
plot_sl <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_sl$x <- seq(from = 0, to = 400, length.out = 100)

# y-axis is the probability density under the given gamma distribution
# Forest
plot_sl$low <- dgamma(x = plot_sl$x,
                      shape = low_sl$params$shape,
                      scale = low_sl$params$scale)
# Grass
plot_sl$medium <- dgamma(x = plot_sl$x,
                         shape = med_sl$params$shape,
                         scale = med_sl$params$scale)
# Wet
plot_sl$high <- dgamma(x = plot_sl$x,
                       shape = hi_sl$params$shape,
                       scale = hi_sl$params$scale)

# Pivot from wide to long data
plot_sl <- plot_sl %>%
  pivot_longer(cols = -x)

# Plot
p1 <- ggplot(plot_sl, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  scale_color_manual(name = "Elevation",
                     breaks = c("low", "medium", "high"),
                     values = c("navyblue", "gray50", "firebrick")) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  theme_bw()

#data.frame for plotting
plot_ta <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_ta$x <- seq(from = -pi, to = pi, length.out = 100)

# y-axis is the probability density under the given gamma distribution
# low
plot_ta$low <- circular::dvonmises(x = plot_ta$x,
                                   kappa = low_ta$params$kappa,
                                   mu = 0)
# med
plot_ta$medium <- circular::dvonmises(x = plot_ta$x,
                                      kappa = med_ta$params$kappa,
                                      mu = 0)
# hi
plot_ta$high <- circular::dvonmises(x = plot_ta$x,
                                    kappa = hi_ta$params$kappa,
                                    mu = 0)

# Pivot from wide to long data
plot_ta <- plot_ta %>%
  pivot_longer(cols = -x)

# Plot
p2 <- ggplot(plot_ta, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  scale_color_manual(name = "Elevation",
                     breaks = c("low", "medium", "high"),
                     values = c("navyblue", "gray50", "firebrick")) +
  scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi),
                     labels = c(expression(-pi, -pi/2, 0, pi/2, pi))) +
  xlab("Turn Angle (radians)") +
  ylab("Probability Density") +
  theme_bw()
combined <- p1 + p2 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

# The step-length distributions are very similar, which we expected based on the non-significant  coefficients associated with the interactions between `elevation` and (`sl_` and `log_sl_`) in our fitted model. We see bigger differences in the turn-angle distributions, as we expected based on the statistical significance of the `elevation` and `cos_ta_` interactions. Our fisher has more directed movements at low elevation and less directed movements at high elevation.

# Conclusion

# We have demonstrated how we can fit a rich set of models that capture the effects of environmental predictors on animal movement  and habitat selection using the `amt` package in R.
