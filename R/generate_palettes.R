#' Generate palettes for plots
#'
#' @name generate_palettes
#' @description Pre-created stored palettes to keep colours consistent
#' @return List of various named or DF colour schemes
#'
#' @export
generate_palettes <- function(){

  UKCEHcolourCodes <- read.csv(here("data", "GIS data", "UKCEHcolourCodes.csv"))

  UKCEHcoloursVecColour <- UKCEHcolourCodes$colour
  names(UKCEHcoloursVecColour) <- paste0("LCM_", UKCEHcolourCodes$value)
  UKCEHcoloursVecColours <- UKCEHcolourCodes$colour
  names(UKCEHcoloursVecColours) <- paste0("LCM_", UKCEHcolourCodes$value)
  UKCEHcoloursVecNames <- UKCEHcolourCodes$label
  names(UKCEHcoloursVecNames) <- paste0("LCM_", UKCEHcolourCodes$value)

  paletteList <- list(
    "UKCEHcolourCodes" = UKCEHcolourCodes,
    "UKCEHcoloursVecColours" = UKCEHcoloursVecColours,
    "UKCEHcoloursVecNames" = UKCEHcoloursVecNames
  )

  return(paletteList)

}
