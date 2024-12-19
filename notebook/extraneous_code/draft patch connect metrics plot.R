
targets::tar_load("tar_connectSSF_list")
targets::tar_load("tar_msePois_df")
targets::tar_load("tar_patchList")
# THETA <- 0.1

patchList <- tar_patchList
mseList <- tar_msePois_df
connectTerraAddr <- tar_connectSSF_list
# connectTerra <- terra::rast(tar_connectSSF_list[[1]])

meanMSE <- mseList %>%
  group_by(theta) %>%
  summarise(meanMSE = mean(mse))

bestTheta <- meanMSE[meanMSE$meanMSE == min(meanMSE$meanMSE),]$theta

connectTerraAddr <- connectTerraAddr[str_detect(names(connectTerraAddr), sub("e-", "e.", as.character(bestTheta)))]
connectTerra <- terra::rast(connectTerraAddr[[1]])

paletteListpaletteListpaletteList <- load_deer_palette()


focalPatches <- patchList$Aberdeen %>%
  filter(!duplicated(Ptch_ID))

names(connectTerra) <- "connectivity"

(connectRasterPlot <- ggplot() +
  geom_spatraster(data = connectTerra, aes()) +
  # geom_sf(data = focalPatches, alpha = 0.1) +
  # geom_sf(data = focalRoads, colour = "#000000", alpha = 0.25) +
  scale_fill_gradient(high = scales::muted("#B54D17"),
                       low = "#ffffff",
                       na.value = paletteList$baseGrey) +
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
          colour = paletteList$baseGrey) +
  scale_fill_gradient(high = scales::muted("#B54D17"),
                      low = "#ffffff",
                      na.value = paletteList$baseGrey) +
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

maxConnectPatchPlot <- ggplot() +
  geom_spatraster(data = connectTerra, aes(), alpha = 0.5) +
  geom_sf(data = patchMeanScore, aes(fill = maxConnectivity),
          colour = paletteList$baseGrey) +
  scale_fill_gradient(high = scales::muted("#B54D17"),
                      low = "#ffffff",
                      na.value = paletteList$baseGrey) +
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

medianConnectPatchPlot <- ggplot() +
  geom_spatraster(data = connectTerra, aes(), alpha = 0.5) +
  geom_sf(data = patchMeanScore, aes(fill = medianConnectivity),
          colour = paletteList$baseGrey) +
  scale_fill_gradient(high = scales::muted("#B54D17"),
                      low = "#ffffff",
                      na.value = paletteList$baseGrey) +
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

(connectRasterPlot + meanConnectPatchPlot +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())) /
  (maxConnectPatchPlot + medianConnectPatchPlot +
     theme(axis.text.y = element_blank(),
           axis.ticks.y = element_blank())) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = paste0(REGION, " Landscape connectivity and patches"),
    subtitle = paste0("theta = ", THETA, ", max patch distance = ", patchDistance,
                      ", repeats per pair = ", repeatsPerPair, ", MSE = ", format(meanMSE, digits = 4))) &
  theme(plot.title = element_text(hjust = 0, face = 2),
        plot.subtitle = element_text(hjust = 0, face = 3),
        text = element_text(colour = paletteList$baseGrey))

