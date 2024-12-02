# targets::tar_load("tar_rsf_models")
# targets::tar_load("tar_deerData")
# targets::tar_load("tar_rsf_data")
#
# nAvail <- 10
#
# rsfModelList <- tar_rsf_models
# rsfDataList <- tar_rsf_data
# deerData <- tar_deerData
# library(performance)
# lapply(rsfModelList, summary)
# lapply(rsfModelList, r2)
# performance::check_model(focalModel)

modelMetrics <- do.call(rbind, lapply(names(rsfModelList), function(x){
  normalRes <- performance::check_residuals(rsfModelList[[x]])
  out <- data.frame("NormalResidualsPvalue" = normalRes,
                    "NormalResidualsTestBinary" = ifelse(normalRes > 0.05, "normal", "non-normal"),
                    "R2_tjur" = performance::r2(rsfModelList[[x]]),
                    "Animal_ID" = x)
  row.names(out) <- NULL
  return(out)
}))

write.csv(modelMetrics, file = here("modelOutput", "rsfIndi_modelMetrics.csv"), row.names = FALSE)

predictedDistanceList <- vector("list", length = length(rsfModelList))
names(predictedDistanceList) <- names(rsfModelList)
predictedLanduseList <- vector("list", length = length(rsfModelList))
names(predictedLanduseList) <- names(rsfModelList)
predictedInteractionsList <- vector("list", length = length(rsfModelList))
names(predictedInteractionsList) <- names(rsfModelList)
for(id in names(rsfModelList)){

  focalModel <- rsfModelList[[id]]
  focalRsfData <- rsfDataList[[id]]

  performance::check_model(focalModel)

  ggsave(filename = here("modelOutput", paste0("rsfIndi_modelPerformance", id, ".png")),
         width = 240, height = 220, units = "mm", dpi = 300)

  newDataDistance <- data.frame(distancePatch = seq(0, 2000, 1),
                                landuse = "Woodland")

  distancePrediction <- predict(object = focalModel,
                                newdata = newDataDistance,
                                type = "response", # does the same as Plogis does
                                se.fit = TRUE)
  # newData$distancePredictionLink <- predict(object = focalModel,
  #                                       newdata = newData,
  #                                       type = "link")
  newDataDistance$distancePrediction <- distancePrediction$fit
  newDataDistance$distancePrediction_se <- distancePrediction$se.fit
  newDataDistance$Animal_ID <- id
  predictedDistanceList[[id]] <- newDataDistance

  landuses <- sub("^landuse", "", names(coef(focalModel))[!names(coef(focalModel)) %in% c("(Intercept)", "distancePatch")])
  landuses <- landuses[str_detect(landuses, "\\:", negate = TRUE)]
  newDataLanduse <- data.frame(
    distancePatch = mean(focalRsfData$usedDeer$distancePatch, na.rm = TRUE),
    landuse = landuses
    # landuse = rep(landuses, each = 100) # frequentist doesn't need repeats
  )

  # interceptPrediction <- predict(object = focalModel,
  #                                # newdata = newDataLanduse,
  #                                se.fit = TRUE, type = "response",
  #                                terms = "Intercept")
  # data.frame(
  #   landuse = "Woodland (Intercept)",
  #   landusePrediction = interceptPrediction$fit,
  #   landusePrediction_se = interceptPrediction$se.fit,
  #   Animal_ID = id
  # )
  # woodlandPatchProb <- plogis(focalModel$coefficients["(Intercept)"])

  landusePrediction <- predict(object = focalModel,
                               newdata = newDataLanduse,
                               type = "response",
                               se.fit = TRUE)

  newDataLanduse$landusePrediction <- landusePrediction$fit
  newDataLanduse$landusePrediction_se <- landusePrediction$se.fit
  newDataLanduse$Animal_ID <- id

  modelSummary <- summary(focalModel)
  newDataLanduse <- newDataLanduse %>%
    left_join(data.frame(
      landuse = sub("^landuse", "", names(modelSummary$coefficients[,4])),
      sig = ifelse(
        modelSummary$coefficients[,4] < 0.01, "p<0.01",
        ifelse(modelSummary$coefficients[,4] < 0.05, "p<0.05", "p>=0.05")))
    )

  predictedLanduseList[[id]] <- newDataLanduse

  interactionPreds <- effect(term = "distancePatch*landuse",
                             mod = focalModel, xlevels = 50,
                             KR = F)
  interactionPreds <- as.data.frame(interactionPreds)
  interactionPreds$Animal_ID <- id

  predictedInteractionsList[[id]] <- interactionPreds

}
predictedDistanceData <- do.call(rbind, predictedDistanceList)
predictedLanduseData <- do.call(rbind, predictedLanduseList)
predictedInteractionData <- do.call(rbind, predictedInteractionsList)

rsfDeerData <- do.call(rbind, lapply(rsfDataList, function(x){
  focalRsfData <- rbind(x$usedDeer %>%
                          st_drop_geometry() %>%
                          select(x, y, Animal_ID, distancePatch, landuse, case_),
                        x$availPoints %>%
                          select(x, y, Animal_ID, distancePatch, landuse, case_) %>%
                          sample_n(n()/nAvail)
  )
  return(focalRsfData)
}))

(distancePop <- predictedDistanceData %>%
  ggplot() +
  # geom_point(data = rsfDeerData,
  #            aes(x = distancePatch, y = ifelse(case_ < 0.5, -0.105, 1.105), colour = Animal_ID),
  #            position = position_jitter(width = 0, height = 0.095),
  #            size = 0.2, alpha = 0.2) +
  # geom_ribbon(aes(x = distancePatch,
  #                 ymin = distancePrediction - 1.96 * distancePrediction_se,
  #                 ymax = distancePrediction + 1.96 * distancePrediction_se,
  #                 fill = Animal_ID),
  #             alpha = 0.15) +
  geom_line(aes(x = distancePatch, y = distancePrediction, colour = Animal_ID),
            linewidth = 0.25) +
  # scale_x_continuous(limits = c(0, max(rsfDeerData$distancePatch, na.rm = TRUE))) +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  coord_cartesian(clip = "on",
                  xlim = c(0, max(rsfDeerData$distancePatch, na.rm = TRUE))
                  # ylim = c(-0.2, 1.2)
                  ) +
  labs(title = "Probability of use",
       x = "Distance from patch (m)",
       y = "Predicted probability of use compare to availability",
       colour = "Animal ID") +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    axis.title = element_text(face = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
    # legend.position = "none"
  ))

(landusePop <- predictedLanduseData %>%
  ggplot() +
  # geom_point(data = rsfDeerData,
  #            aes(x = landuse, y = ifelse(case_ < 0.5, -0.105, 1.105), colour = Animal_ID),
  #            position = position_jitter(width = 0.2, height = 0.1),
  #            size = 0.2, alpha = 0.2) +
  geom_errorbar(aes(x = Animal_ID,
                    ymin = landusePrediction - 1.96 * landusePrediction_se,
                    ymax = landusePrediction + 1.96 * landusePrediction_se,
                    colour = Animal_ID),
                alpha = 1, width = 0.1) +
  geom_point(aes(x = Animal_ID, y = landusePrediction, shape = sig, colour = Animal_ID),
             size = 2) +
  coord_flip(ylim = c(0, 0.5)) +
  facet_grid(rows = vars(landuse), space ="free", drop = TRUE, scales = "free_y",
             switch = "y") +
  labs(title = "Land use effect on probability of use",
       x = "Land use",
       y = "Predicted probability of use compare to availability\n(at mean distance from patch)",
       colour = "Animal ID", shape = "Significance") +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    axis.title = element_text(face = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
    # legend.position = "none"
  ) +
    guides(colour = guide_none())
    )

(interaction <- ggplot(predictedInteractionData) +
  geom_line(aes(x = distancePatch, y = fit, colour = Animal_ID),
            linewidth = 0.25) +
  facet_wrap(vars(landuse)) +
  labs(title = "Interaction between distance from patch and land use",
       x = "Distance from patch (m)",
       y = "Predicted probability of use compare to availability",
       colour = "Animal ID") +
  theme_bw() +
  theme(
    text = element_text(colour = "grey25"),
    line = element_line(colour = "grey25"),
    axis.title = element_text(face = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = 2, hjust = 1, vjust = 1)
    # legend.position = "none"
  ))

ggsave(plot = distancePop,
  filename = here("modelOutput", paste0("rsfIndi_effectDistance", ".png")),
       width = 240, height = 220, units = "mm", dpi = 300)
ggsave(plot = landusePop,
  filename = here("modelOutput", paste0("rsfIndi_effectLanduse", ".png")),
       width = 240, height = 220, units = "mm", dpi = 300)
ggsave(plot = interaction,
  filename = here("modelOutput", paste0("rsfIndi_effectInteraction", ".png")),
       width = 240, height = 220, units = "mm", dpi = 300)

# guide_area() + distancePop + landusePop + interaction +
#   plot_layout(guides = "collect") &
#   theme(legend.direction = "horizontal",
#         legend.title.position = "top")
