
poisModelData |>
  filter(case_) |>
  mutate(hour = lubridate::hour(t1_)) |>
  group_by(roadTypeCrossing, hour) |>
  summarise(crossingCount = n()) |>
  filter(!roadTypeCrossing == "No_crossing") |>
  ggplot() +
  geom_col(aes(x = hour, y = crossingCount, fill = roadTypeCrossing)) +
  scale_fill_manual(values = unname(paletteList$highMidLowPal)) +
  theme_minimal() +
  labs(x = "Hour", y = "Crossing count", fill = "Road type")

ggsave(
  # plot = coefCombo,
  filename = here::here("figures", "roadcrossingcount.png"),
  width = 210, height = 160, units = "mm", dpi = 300,
  device = ragg::agg_png
)
