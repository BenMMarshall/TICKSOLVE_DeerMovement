#' Render the manuscript rmd
#'
#' @name render_rmd
#' @description A
#' @return Nothing, PDF (or output) will be saved to a folder.
#'
#' @export
render_rmd <- function(fileIN, fileOUT,
                       ...){

  rmarkdown::render(input = fileIN,
                    output_file = fileOUT)

}
