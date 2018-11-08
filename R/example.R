#' Get path to external example data.
#'
#' `rgslib` comes bundled with some sample files in its `inst/extdata`
#' directory. This function make them easy to access.
#'
#' This function is a direct modification of the function used by the [`readr`
#' package](https://github.com/tidyverse/readr/blob/master/R/example.R)
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' rgslib_example()
#' rgslib_example("samples-na.dat")
rgslib_example <- function(path = NULL) {
    if (is.null(path)) {
        dir(system.file("extdata", package = "rgslib"))
    } else {
        system.file("extdata", path, package = "rgslib", mustWork = TRUE)
    }
}
