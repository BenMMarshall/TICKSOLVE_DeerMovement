

targets::tar_load("tar_predPoisResist_location")
targets::tar_load("tar_patchList")
patchList <- tar_patchList
resistRasterLocations <- tar_predPoisResist_location
resistTerra <- terra::rast(resistRasterLocations)
targets::tar_source()
REGION <- "Aberdeenshire"
method <- "pois"

focalPatches <- patchList[[sub("shire", "", REGION)]] %>%
  filter(!duplicated(Ptch_ID))

ggplot() +
  geom_spatraster(data = resistTerra, aes()) +
  scale_fill_gradient(high = scales::muted("#B54D17"),
                      low = "#ffffff",
                      na.value = paletteList$baseGrey) +
  labs(fill = "Connectivity", title = "Connectivity") +
  coord_sf(xlim = c(max(c(ext(focalPatches)[1],
                          ext(resistTerra)[1])),
                    min(c(ext(focalPatches)[2],
                          ext(resistTerra)[2]))),
           ylim = c(max(c(ext(focalPatches)[3],
                          ext(resistTerra)[3])),
                    min(c(ext(focalPatches)[4],
                          ext(resistTerra)[4]))),
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

ggsave(filename = here::here("figures", paste0("modelPredictionMap_", method, "_", REGION, ".pdf")),
       width = 220, height = 180, units = "mm")
