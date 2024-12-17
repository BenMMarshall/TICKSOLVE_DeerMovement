
targets::tar_load("tar_connectSSF_list")
targets::tar_load("tar_patchList")

patchList <- tar_patchList
connectTerra <- terra::rast(tar_connectSSF_list$tar_connectSSF_location_0.001_1_1200)

focalPatches <- patchList$Aberdeen %>%
  filter(!duplicated(Ptch_ID))

names(connectTerra) <- "connectivity"

colorspace::lighten("#DCBD0A", amount = 0.9)

(connectRasterPlot <- ggplot() +
  geom_spatraster(data = connectTerra, aes()) +
  # geom_sf(data = focalPatches, alpha = 0.1) +
  # geom_sf(data = focalRoads, colour = "#000000", alpha = 0.25) +
  scale_fill_gradient(high = scales::muted("#B54D17"),
                       low = "#ffffff",
                       na.value = "#333333") +
  labs(fill = "Connectivity", title = "By Cell Connectivity") +
  coord_sf(xlim = c(max(c(ext(focalPatches)[1],
                          ext(connectTerra)[1])),
                    min(c(ext(focalPatches)[2],
                          ext(connectTerra)[2]))),
           ylim = c(max(c(ext(focalPatches)[3],
                          ext(connectTerra)[3])),
                    min(c(ext(focalPatches)[4],
                          ext(connectTerra)[4]))),
           expand = 0, crs = st_crs(27700)) +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    plot.title = element_text(face = 2),
    axis.title = element_text(face = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(angle = 0, face = 4, hjust = 0, vjust = 1)
    # legend.position = "none"
  ))

patchMeanScore <- terra::extract(connectTerra, focalPatches, fun = mean,
                                 bind = TRUE) %>%
  rename(meanConnectivity = connectivity)

patchMeanScore <- terra::extract(connectTerra, patchMeanScore, fun = max,
                                 bind = TRUE) %>%
  rename(maxConnectivity = connectivity)
patchMeanScore <- terra::extract(connectTerra, patchMeanScore, fun = median,
                                 bind = TRUE) %>%
  rename(medianConnectivity = connectivity)

meanConnectPatchPlot <- ggplot() +
  geom_spatraster(data = connectTerra, aes(fill  = connectivity), alpha = 0.5) +
  geom_sf(data = patchMeanScore, aes(fill = meanConnectivity),
          colour = "#333333") +
  scale_fill_gradient(high = scales::muted("#B54D17"),
                      low = "#ffffff",
                      na.value = "#333333") +
  labs(fill = "Connectivity", title = "Mean Connectivity") +
  # geom_sf(data = focalRoads, colour = "#000000", alpha = 0.25) +
  coord_sf(xlim = c(max(c(ext(focalPatches)[1],
                          ext(connectTerra)[1])),
                    min(c(ext(focalPatches)[2],
                          ext(connectTerra)[2]))),
           ylim = c(max(c(ext(focalPatches)[3],
                          ext(connectTerra)[3])),
                    min(c(ext(focalPatches)[4],
                          ext(connectTerra)[4]))),
           expand = 0, crs = st_crs(27700)) +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    plot.title = element_text(face = 2),
    axis.title = element_text(face = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(angle = 0, face = 4, hjust = 0, vjust = 1)
    # legend.position = "none"
  )

maxConnectPatchPlot <- ggplot() +
  geom_spatraster(data = connectTerra, aes(), alpha = 0.5) +
  geom_sf(data = patchMeanScore, aes(fill = maxConnectivity),
          colour = "#333333") +
  scale_fill_gradient(high = scales::muted("#B54D17"),
                      low = "#ffffff",
                      na.value = "#333333") +
  labs(fill = "Connectivity", title = "Max Connectivity") +
  # geom_sf(data = focalRoads, colour = "#000000", alpha = 0.25) +
  coord_sf(xlim = c(max(c(ext(focalPatches)[1],
                          ext(connectTerra)[1])),
                    min(c(ext(focalPatches)[2],
                          ext(connectTerra)[2]))),
           ylim = c(max(c(ext(focalPatches)[3],
                          ext(connectTerra)[3])),
                    min(c(ext(focalPatches)[4],
                          ext(connectTerra)[4]))),
           expand = 0, crs = st_crs(27700)) +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    plot.title = element_text(face = 2),
    axis.title = element_text(face = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(angle = 0, face = 4, hjust = 0, vjust = 1)
    # legend.position = "none"
  )

medianConnectPatchPlot <- ggplot() +
  geom_spatraster(data = connectTerra, aes(), alpha = 0.5) +
  geom_sf(data = patchMeanScore, aes(fill = medianConnectivity),
          colour = "#333333") +
  scale_fill_gradient(high = scales::muted("#B54D17"),
                      low = "#ffffff",
                      na.value = "#333333") +
  labs(fill = "Connectivity", title = "Median Connectivity") +
  # geom_sf(data = focalRoads, colour = "#000000", alpha = 0.25) +
  coord_sf(xlim = c(max(c(ext(focalPatches)[1],
                          ext(connectTerra)[1])),
                    min(c(ext(focalPatches)[2],
                          ext(connectTerra)[2]))),
           ylim = c(max(c(ext(focalPatches)[3],
                          ext(connectTerra)[3])),
                    min(c(ext(focalPatches)[4],
                          ext(connectTerra)[4]))),
           expand = 0, crs = st_crs(27700)) +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    plot.title = element_text(face = 2),
    axis.title = element_text(face = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(angle = 0, face = 4, hjust = 0, vjust = 1)
    # legend.position = "none"
  )

(connectRasterPlot + meanConnectPatchPlot +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())) /
  (maxConnectPatchPlot + medianConnectPatchPlot +
     theme(axis.text.y = element_blank(),
           axis.ticks.y = element_blank())) +
  plot_layout(guides = "collect")
