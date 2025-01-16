
targets::tar_load("tar_circuitscape_files")

cirTerra <- rast(tar_circuitscape_files[str_detect(tar_circuitscape_files, "_cum_")])

plot(cirTerra)

for(f in tar_circuitscape_files){
  plot(rast(f))
}


focalPatches %>%
  filter(NumID %in% c(rowInclusionList[[1]][,1], rowInclusionList[[1]][,2])) %>%
  ggplot() +
  geom_sf(aes(fill = NumID))


ggplot() +
  geom_spatraster(data = binaryRaster %>%
                    filter(NumID %in% c(rowInclusionList[[1]][,1], rowInclusionList[[1]][,2])),
                  aes(fill = NumID))
