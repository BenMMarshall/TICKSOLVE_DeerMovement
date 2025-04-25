
library(here)
library(dplyr)
library(raster)
library(terra)
library(tidyterra)
library(ctmm)

targets::tar_load("tar_akdeLists")
targets::tar_load("tar_landuseList")
targets::tar_load("tar_deerData")

akdeLists <- tar_akdeLists
landuseList <- tar_landuseList
deerData <- tar_deerData
REGION = "Aberdeenshire"
if(REGION == "Aberdeenshire"){
  focalDistanceWoodland <- terra::rast(landuseList$Aberdeen$distanceWoodland)
  focalDistanceHedges <- terra::rast(landuseList$Aberdeen$distanceHedges)
  focalLand <- terra::rast(landuseList$Aberdeen$landuse)
}
focalLand <- focalLand %>%
  tidyterra::select(landuse)
focalDistanceWoodland <- project(focalDistanceWoodland, y = crs("epsg:4326"))
focalDistanceHedges <- project(focalDistanceHedges, y = crs("epsg:4326"))
focalLand <- project(focalLand, y = crs("epsg:4326"))

unique(focalLand$landuse)
focalLand <- focalLand %>%
  mutate(
    Deciduous_Broadleaf_Forest = ifelse(landuse == "Deciduous Broadleaf Forest", 1, 0),
   Evergreen_Needleleaf_Forest = ifelse(landuse == "Evergreen Needleleaf Forest", 1, 0),
                      Cropland = ifelse(landuse == "Cropland", 1, 0),
                Tall_Grassland = ifelse(landuse == "Tall Grassland", 1, 0),
               Short_Grassland = ifelse(landuse == "Short Grassland", 1, 0),
                Open_Shrubland = ifelse(landuse == "Open Shrubland", 1, 0),
                        Barren = ifelse(landuse == "Barren", 1, 0),
             Permanent_Wetland = ifelse(landuse == "Permanent Wetland", 1, 0),
             Human_Settlements = ifelse(landuse == "Human Settlements", 1, 0),
                         Other = ifelse(landuse == "Other", 1, 0)
  ) %>%
  tidyterra::select(-landuse)

binaryList <- terra::split(focalLand, names(focalLand))
names(binaryList) <- names(focalLand)

rasterList <- list(
  distanceWoodland = raster::raster(focalDistanceWoodland),
  # distanceHedges = raster::raster(focalDistanceHedges),
  # Deciduous_Broadleaf_Forest = raster::raster(binaryList$Deciduous_Broadleaf_Forest),
  # Evergreen_Needleleaf_Forest = raster::raster(binaryList$Evergreen_Needleleaf_Forest),
  # Cropland = raster::raster(binaryList$Cropland),
  Tall_Grassland = raster::raster(terra::project(binaryList$Tall_Grassland, terra::crs(focalDistanceWoodland))),
  # Short_Grassland = raster::raster(binaryList$Short_Grassland),
  Open_Shrubland = raster::raster(terra::project(binaryList$Open_Shrubland, terra::crs(focalDistanceWoodland)))#,
  # Barren = raster::raster(binaryList$Barren),
  # Permanent_Wetland = raster::raster(binaryList$Permanent_Wetland),
  # Human_Settlements = raster::raster(binaryList$Human_Settlements)
)

focalDeer <- deerData %>%
  filter(region == REGION)
focalDeerIDs <- unique(focalDeer$Animal_ID)
# x <- focalDeerIDs[2]

# integrator="MonteCarlo"
# Time difference of -21.48579 mins
# integrator="Riemann"

listWRSF <- lapply(names(akdeLists$tele)[names(akdeLists$tele) %in% focalDeerIDs], function(x){
  print(x)
  st <- Sys.time()
  wrsfOUT <- rsf.fit(data = akdeLists$tele[[x]],
                     UD = akdeLists$akde[[x]],
                     R = rasterList,
                     debias = FALSE,
                     error = 0.001,
                     integrator = "Riemann",
                     max.mem = "20 Gb")
  ed <- Sys.time()
  return(wrsfOUT)
})
names(listWRSF) <- names(teleObj)


generate_ctmm_wrsfAKDE <- function(teleObj, ud, wrsf, landscapeFileList, error){

  rasterList <- lapply(landscapeFileList, raster)
  names(rasterList) <- paste0("raster", seq_along(rasterList))

  RAKDE <- ctmm::akde(data = teleObj,
                      CTMM = wrsf,
                      R = rasterList,
                      weights = TRUE,
                      error = error)

  return(RAKDE)

}


wAKDE <- ctmm::akde(data = akdeLists$tele[[x]],
                    CTMM = wrsfOUT,
                    R = rasterList,
                    weights = TRUE,
                    error = 0.001)


plot(wAKDE, level.UD = c(0.95))
plot(akdeLists$akde[[x]], level.UD = c(0.95))
plot(rasterList$Deciduous_Broadleaf_Forest)
plot(akdeLists$tele[[x]], add = TRUE)
summary(wAKDE)

extent(focalDistanceWoodland)
akdeLists$tele[[x]]

polyData_naive <- as.sf(akdeLists$akde[[x]], UD = 0.95) %>%
  mutate(ci = factor(stringr::str_extract(name, "low|est|high"),
                     levels = c("high", "est", "low")),
         type = "naive",
         udLevel = 0.95,
         id = stringr::word(name, 1, 1),
         species = substr(id, 1, 4)) %>%
  arrange(ci)
polyData_wrsf <- as.sf(wAKDE, UD = 0.95) %>%
  mutate(ci = factor(stringr::str_extract(name, "low|est|high"),
                     levels = c("high", "est", "low")),
         type = "wrsf",
         udLevel = 0.95,
         id = stringr::word(name, 1, 1),
         species = substr(id, 1, 4)) %>%
  arrange(ci)

library(ggplot2)

# ggplot() +
#   geom_spatraster(data = binaryList$Deciduous_Broadleaf_Forest) +
#   geom_sf(data = polyData_naive, aes(), alpha = 1, colour = "red") +
#   coord_sf(crs = sf::st_crs(polyData_naive),
#            xlim = sf::st_bbox(polyData_naive)[c(1,3)],
#            ylim = sf::st_bbox(polyData_naive)[c(2,4)]
#            )

akdePolygons <- rbind(polyData_naive, polyData_wrsf)

ggplot(akdePolygons) +
  # geom_spatraster(data = binaryList$Deciduous_Broadleaf_Forest %>%
  #                   mutate(layer = factor(Deciduous_Broadleaf_Forest)),
  #                   mutate(layer = factor(Deciduous_Broadleaf_Forest)),
  #                 aes(fill = layer)) +
  geom_spatraster(data = focalDistanceWoodland,
                  aes(fill = distanceWoodland)) +
  geom_sf(aes(colour = ci, linetype = ci), fill = NA, linewidth = 0.5) +
  # geom_sf(data = movementDataAll, colour = "#873e23", size = 0.75) +
  facet_grid(col = vars(id), row = vars(type)) +
  coord_sf(crs = sf::st_crs(akdePolygons),
           xlim = sf::st_bbox(akdePolygons)[c(1,3)],
           ylim = sf::st_bbox(akdePolygons)[c(2,4)]
  ) +
  scale_fill_gradient(limits = c(0,500)) +
  # scale_fill_manual(values = c("0" = "white", "1" = "grey25")) +
  scale_linetype_manual(values = c("high" = "dashed", "est" = "solid", "low" = "dotted")) +
  scale_colour_manual(values = c("high" = "#033e76", "est" = "#e28743", "low" = "#60c0d8")) +
  theme_bw() +
  theme(text = element_text(colour = "grey15"),
        line = element_line(colour = "grey15"),
        strip.background = element_blank(),
        strip.text = element_text(face = 4, colour = "grey15",
                                  hjust = 0))
