#' Pull in stored palettes
#'
#' @name load_deer_palette
#' @description a
#' @return a
#'
#' @export
load_deer_palette <- function(){

  corePal <- c("#DCBD0A", "#B54D17","#CD602A","#E79559", "#9F7E93", "#85AB7A", "#789542")
  names(corePal) <- c("Arable", "Aquatic", "Heathland", "Human Settlements", "Other", "Grassland", "Woodland")

  deerSpeciesPal <- corePal[c(2,4)]
  names(deerSpeciesPal) <- c("Roe", "Fallow")

  deerSexPal <- corePal[c(2,4)]
  names(deerSexPal) <- c("M", "F")

  highWhitePal <- c(corePal[c(2)], "#ffffff")
  names(highWhitePal) <- c("high", "low")

  highMidLowPal <- c(scales::muted(corePal[2]), corePal[c(3:4)])
  names(highMidLowPal) <- c("high", "mid", "low")

  baseGrey <- "#303030"
  noSigGrey <- "#505050"

  highSigLowSigNoSig <- c(corePal[2], corePal[4], noSigGrey)
  names(highSigLowSigNoSig) <- c("Significant +", "Significant -", "Not Significant")

  paletteList <- list(
    corePal = corePal,
    deerSpeciesPal = deerSpeciesPal,
    deerSexPal = deerSexPal,
    highWhitePal = highWhitePal,
    highMidLowPal = highMidLowPal,
    baseGrey = baseGrey,
    highSigLowSigNoSig = highSigLowSigNoSig
  )

  return(paletteList)

}
