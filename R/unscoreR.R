#' Run the CCG \code{unscore} Program to Normal Score Transform Sample Data.
#'
#' \code{unscoreR} transforms selected variables to a normal distribution using
#' the CCG Fortran program \code{declus}. Currently the options to use a
#' category or to use a reference distribution are not supported. Trimming
#' limits are fixed to -1.0e21 and +1.0e21.
#'
#' @param data Data frame with value and optional weight and category columns.
#' @param vars Character vector, column names in \code{data} for transformation.
#' @param weight Character scalar, name of weight field, default \code{NULL}.
#' @param cat Character scalar, name of category field, default \code{NULL}.
#' @param quantiles Integer scalar, number of quantiles output to transformation
#'   tables, default 0, which means all.
#' @param debug Scalar boolean. If \code{TRUE} don't delete temporary system
#'   files. Default is \code{FALSE}.
#'
#' @return A list of two data frames: \code{data} contains original input data
#'   plus columns of normal score transformed data for each specified column and
#'   \code{trans} contains the transformation tables for each transformed
#'   variable.
#' @export
#' @importFrom utils read.table
#' @examples
#' samples_ns <- unscoreR(samples_2d, c("thk", "accum"))
#' par(mfrow=c(1,2))
#' hist(samples_ns$data$accum, breaks=30, main="raw", xlab="accum")
#' hist(samples_ns$data$NS_accum, main="normal", xlab="nscore accum")
unscoreR <- function(
  data, vars, weight=NULL, cat=NULL, quantiles=0, debug=FALSE
  ) {

  # Get numeric columns only. GSLIB only handles numeric data.
  nums <- unlist(lapply(data, is.numeric))
  data_num <- data[,nums]

  # Get column indices.
  vars_i <- paste(which(colnames(data_num) %in% vars), collapse = "  ")
  if(!is.null(weight)){
    weight_i <- which(colnames(data_num) == weight)
  } else {
    weight_i <- 0
  }
  if(!is.null(cat)){
    cat_i <- which(colnames(data_num) == cat)
  } else {
    cat_i <- 0
  }

  # Export data for external program.
  write_gslib(data_num, "unscore-in.dat", title="Sample data")

  # Build a parameter file for the unscore executable.
  file_conn <- file("unscore.par")
  writeLines(
    paste0(
      "START OF PARAMETERS:\n",
      "unscore-in.dat \n",
      length(vars), "  ", vars_i, "  \n",
      weight_i, "  \n",
      cat_i, "  \n",
      nrow(data_num), "  \n",
      "-1.0e21  1.0e21  \n",
      "0  \n",
      "\n",
      "1 2 0  \n",
      quantiles, "  \n",
      "unscore-out.dat  \n",
      "unscore-trn.dat  \n"
      ),
    file_conn)
  close(file_conn)

  # Run the unscore program.
  shell("unscore unscore.par")

  # Read the output.
  nscore_data <- read_gslib("unscore-out.dat")

  # Read the transformation tables.
  for(i in seq_along(vars)) {
    d <- read.table(paste0("unscore-trn", i, ".dat"),
        col.names = c("raw", "nscore"))
    d[,"variable"] <- vars[i]
    d <- d[,c(3, 1, 2)]
    if(i == 1) {
      trn_data <- d
    } else {
      trn_data <- rbind(trn_data, d)
    }
  }

  # Clean up.
  if(!debug) {
    shell("del unscore.par unscore-in.dat unscore-out.dat unscore-trn*.dat")
  }

  return(list(data = nscore_data, trans = trn_data))

}

