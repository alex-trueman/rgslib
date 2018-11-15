#' Run the CCG \code{vl_lmc} to Automatically Model Multivariate Variograms
#'
#' \code{vl_lmcR} is an interface to the Fortran program \code{varfit_lmc}.
#' It auotmaticaly models direct and cross variograms given experimental points
#' and models fo the direc variograms.
#'
#' @param nvars Scalar numeric: number of variables being modelled.
#' @param corr0 Numeric vector: cross-variogram nugget effects.
#' @param corr Numeric matrix: correlation matrix (e.g., from \code{cor}).
#' @param varcalcs Character: prefix of experimental variogram files.
#' @param varfits Character: prefix of direct variogram model files.
#' @param invdistwt Boolean: inverse distance weighting.
#' @param npairswt Boolean: number of pairs weighting.
#' @param minpairs Scalar integer: minimum pairs.
#' @param maxiter Scalar integer: number of fitting iterations.
#' @param varfitslmc Character: prefix of output variogram model files.
#' @param corrs Character: output correlation matrix system file.
#' @return A list with two data frames: `model` contains the variogram models
#'   and `corrs` contains the model cross-variogram sills for validation.
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @importFrom purrr imap map
#' @importFrom rlang .data set_names
#' @importFrom tidyselect everything
#' @export
vl_lmcR <- function(nvars, corr0, corr, varcalcs="vg_", varfits="vm_",
  invdistwt=TRUE, npairswt=TRUE, minpairs=10, maxiter=100, varfitslmc="vl_",
  corrs="corrs.txt") {

  # Build a parameter string.
  parstring <- c(
    "START OF PARAMETERS:",
    paste0(
      nvars, "  \n",
      varcalcs, "  \n",
      varfits, "  "
    )
  )
  # Correlation matrix.
  for(i in 1:nvars) {
    parstring <- c(parstring, paste(corr[i,], collapse = "  "))
  }
  parstring <- c(
    parstring,
    paste0(
      paste(corr0, collapse = "  "), "\n",
      as.numeric(invdistwt), "  ", as.numeric(npairswt), "  ", minpairs, "  \n",
      maxiter, "  \n",
      varfitslmc, "  \n",
      corrs, "  "
    )
  )

  # Write parameters to a file.
  file_conn <- file("vl_lmc.par")
  writeLines(parstring, file_conn)
  close(file_conn)

  # Delete pre-existing `vl_lmc` outputs.
  shell(paste0("del ", varfitslmc, "*.var"))
  # Run the `vl_lmc` program.
  shell("vl_lmc vl_lmc.par")

  # Import GSLIB variogram models.
  models <- list.files(
    ".",
    pattern = paste0(varfitslmc, ".+\\.var"),
    full.names = TRUE
  ) %>%
    set_names(basename(.)) %>%
    map(read_gslib_mvario)
  names(models) <- sub(
    paste0("^", varfitslmc, "([0-9]+)_([0-9]+)\\.var"),
    "\\1_\\2",
    names(models)
  )
  # Add var1 and var2 columns used in `usgsimR` when bulding mvario parameter
  # string.
  models <- imap(models, ~mutate(
    .x, var1 = sub("(.+)_(.+)", "\\1", .y), var2 = sub("(.+)_(.+)", "\\2", .y)))

  # Re-set class of data frames as imap removes the class "variogramModel".
  for(df in seq_along(models)){
    class(models[[df]]) <- c("variogramModel", "data.frame")
  }

  class(models) <- c("variogramModelList", "list")

  return(models)

}
