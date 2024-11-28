#' Create variograms, choose best fitting model, and created weighted AKDE
#'
#' @name read_deer_data
#' @description abc
#' @return abc
#'
#' @export
calculate_akdes <- function(deerData){

  # deerData <- tar_deerData

  # deerData$datetime <- as.POSIXct(deerData$datetime, format = "%Y-%m-%d %H:%M:%S")
  deerData <- as.data.frame(deerData)

  teleObj <- ctmm::as.telemetry(deerData,
                                timeformat = "%Y-%m-%d %H:%M:%S",
                                timezone = "UTC",
                                projection = sp::CRS(SRS_string = "EPSG:27700"))


  print("teleObj")

  listVarioData <- lapply(teleObj, function(x){
    out <- ctmm::variogram(x, fast = FALSE, CI = "Gauss")
    return(out)
  })

  print("vario")

  listGuess <- lapply(teleObj, function(x){
    out <- ctmm::ctmm.guess(x, interactive = FALSE)
    return(out)
  })

  print("guess")

  listFits <- lapply(names(teleObj), function(x){
  # listFits <- lapply(names(teleObj)[1:2], function(x){
    print(x)
    out <- ctmm::ctmm.select(teleObj[[x]], listGuess[[x]], verbose = FALSE,
                             cores = 6, method = "pHREML")
    return(out)
  })
  names(listFits) <- names(teleObj)

  print("fit")

  ### COULD PREDEFINE A GRID TO ENABLE A MEAN HOME RANGE ESTIMATE LATER
  listAKDE <- lapply(names(teleObj), function(x){
  # listAKDE <- lapply(names(teleObj)[1:2], function(x){
    print(x)
    out <- ctmm::akde(teleObj[[x]], listFits[[x]],
               weights = TRUE,
               res = 10#,
               # grid = list(
               #   extent =
               #   align.to.origin = TRUE
               # )
               )
    return(out)
  })
  names(listAKDE) <- names(teleObj)

  print("contours")

  listSF <- lapply(names(listAKDE), function(x){
  # listSF <- lapply(names(listAKDE)[1:2], function(x){
    print(x)
    out <- as.sf(listAKDE[[x]], error = FALSE, level.UD = c(0.95, 0.99))
    return(out)
  })
  names(listSF) <- names(listAKDE)

  print("summary")

  listSummary <- lapply(names(listGuess), function(x){
  # listSummary <- lapply(names(listGuess)[1:2], function(x){
    print(x)
    modelSum <- summary(listGuess[[x]])
    modelSummaryDataframe <- as.data.frame(modelSum$CI)
    modelSummaryDataframe$variable <- rownames(modelSummaryDataframe)
    modelSummaryDataframe$model <- modelSum$name
    modelSummaryDataframe$Animal_ID <- x
    return(modelSummaryDataframe)
  })
  names(listSummary) <- names(listGuess)

  print("area")

  listArea <- lapply(names(listAKDE), function(x){
  # listArea <- lapply(names(listAKDE)[1:2], function(x){
    print(x)

    kdeSummaryList <- lapply(c(0.9, 0.95, 0.99), function(y){
      # y <- 0.95
      kdeSum <- summary(listAKDE[[x]], level.UD = y)
      kdeSummary <- as.data.frame(kdeSum$CI)
      kdeSummary$level <- y
      kdeSummary$unit <- rownames(kdeSummary)
      if(str_detect(kdeSummary$unit, "square kilo")){
        kdeSummary$low <- kdeSummary$low *100
        kdeSummary$est <- kdeSummary$est *100
        kdeSummary$high <- kdeSummary$high *100
        kdeSummary$unit <- "area (hectares)"
      }
      return(kdeSummary)
    })
    kdeSummaryAll <- do.call(rbind, kdeSummaryList)
    kdeSummaryAll$Animal_ID <- x
    return(kdeSummaryAll)
  })
  names(listArea) <- names(listAKDE)

  completeList <- list(
    tele = teleObj,
    vario = listVarioData,
    guess = listGuess,
    fits = listFits,
    akde = listAKDE,
    summary = listSummary,
    area = listArea,
    sf = listSF
  )

  return(completeList)

}
