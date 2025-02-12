
library(ggplot2)
library(ggridges)

tar_load("tar_deerData")

hist(tar_deerData$step)

# https://treethinkers.org/tutorials/the-gamma-distribution/
# log_sl shape - decrease
# when shape == 1 gamma becomes exponential
# sl_ scale - increase
# when rate gets smaller we see more of a tail - scale in invert of rate
# so as scale gets bigger rate gets smaller
# so scale gets bigger there is more of a tail
1/0.5
1/0.75

x <- 10000
data.frame(
  param = c(rep("base_sc1sh2", x),
            rep("sc1sh1.25", x), rep("sc4sh2", x), rep("com_sc4sh1.25", x),
            rep("sc8sh1.12", x)),
  value = c(rgamma(n = x, scale = 1, shape = 2),
            rgamma(n = x, scale = 1, shape = 1.25),
            rgamma(n = x, scale = 4, shape = 2),
            rgamma(n = x, scale = 4, shape = 1.25),
            rgamma(n = x, scale = 8, shape = 1.12)
            )) %>%
  ggplot() +
  geom_density_ridges(aes(x = value, y = param)) #+
  coord_cartesian(xlim = c(NA, 10))
