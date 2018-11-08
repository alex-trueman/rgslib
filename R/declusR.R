#' Run the GSLIB \code{declus} Program to Decluster Sample Data.
#'
#' \code{declusR} calculates decluster weights for two or three-dimensional
#' sample data using the CCG Fortran program \code{declus}. Cell declustering is
#' used with optional offsets of the grid origin. The program automatically
#' selects the decluster cell size from a range of deinfined sizes based on
#' either the minimum or maximum declustered mean. Trimming limits are fixed to
#' -1.0e21 and +1.0e21.
#'
#' @param data Data frame with coordinate and value fields.
#' @param cols Character vector of column names in \code{data}:
#'   \code{cols[1:2]} names of numeric X and Y coordinate columns, respectively.
#'   \code{cols[3]} name of numeric Z coordinate column for three-dimensional
#'   sample data. For two-dimensional sample data leave out.
#'   \code{cols[4]} (or \code{cols[3]} for 2D) name of numeric grade/value
#'   column in \code{data}.
#' @param yz_aniso Numeric vector of Y and Z cell dimension anisotropy factors.
#'   Y and Z cell size is determined by multiplying these factors and the X cell
#'   size. Default is 1, 1.
#' @param min_max Scalar integer 0 or 1. When selecting the decluster cell size
#'   consider either the minimum mean (0) or the maximum mean (1). Default is 0.
#' @param cells Numeric vector defining delcuster cells. 1 is number of cell
#'   size intervals to use in determining optimal cell size. 2 and 3 are minimum
#'   and maximum x-axis cell sizes to consider. 4 is is number of origin
#'   offsets for decluster grid. Default is \code{[50, 10, 400, 5]}.
#' @param debug Scalar boolean. If \code{TRUE} don't delete temporary system
#'   files. Default is \code{FALSE}.
#'
#' @return A list of three data frames: \code{data} contains the coordinates,
#'   value, and decluster weight; \code{plot_data} contains decluster cell sizes
#'   and means for plotting; and \code{stats} contains summary statistics
#'   associated with the declustering.
#' @export
#' @importFrom stats weighted.mean
#'
#' @examples
#' test2d <- declusR(samples_2d, c("x", "y", "value"), cells=c(100, 10, 100, 5))
#' # Plot the cell size versus the declustered mean.
#' plot(test2d$plot_data$cell_size, test2d$plot_data$mean)
#' abline(h=test2d$stats[test2d$stats$statistic == "naive mean",]$value, lty=2)
declusR <- function(
  data, cols, yz_aniso=c(1, 1),
  min_max=0L, cells=c(50, 10, 400, 5), debug=FALSE
  ) {

  # Get column indices.
  if(length(cols) == 4) {
    # 3D data with z-coordinate.
    cols_i <- paste(which(colnames(data) %in% cols), collapse = "  ")
  } else if(length(cols) == 3) {
    # 2D data, no z-coordinate.
    cols_i <- paste(c(
      which(colnames(data) %in% cols[1:2]),
        0,
        which(colnames(data) %in% cols[3])),
      collapse = "  ")
  } else {
    # Vector of column names must be either length 3 or 4.
    return(FALSE)
  }

    # Export data for external program.
  write_gslib(data, "declus-in.dat", title="sample data")

  # Build a parameter file for the declus executable.
  file_conn <- file("declus.par")
  writeLines(
    paste0(
      "START OF PARAMETERS:\n",
      "declus-in.dat \n",
      cols_i, "  \n",
      "-1.0e21  1.0e21  \n",
      "declus-stats.dat  \n",
      "declus-out.dat  \n",
      yz_aniso[1], "  ", yz_aniso[2], "  \n",
      min_max, "  \n",
      cells[1], "  ", cells[2], "  ", cells[3], "  \n",
      cells[4]
      ),
    file_conn)
  close(file_conn)

  # Run the declus program.
  shell("declus declus.par")

  # Read in the declustered data.
  dec_data <- read_gslib("declus-out.dat")
  colnames(dec_data)[names(dec_data) == "DeclusteringWeight"] <- "weight"

  # Get data for plotting delcutser stats.
  dec_plot <- read_gslib("declus-stats.dat")
  colnames(dec_plot) <- c("cell_size", "mean")

  # Create some summary statistics.
  dec_stats <- data.frame(statistic=character(), value=double(),
    stringsAsFactors = FALSE)
  dec_stats <- rbind(dec_stats, data.frame(statistic="naive mean",
    value=dec_plot$mean[1]))
  # Remove the first row of the plot data, it is the naive mean.
  dec_plot <- dec_plot[2:nrow(dec_plot),]
  dec_mean <- weighted.mean(dec_data[,cols[3]], w = dec_data$weight)
  dec_stats <- rbind(dec_stats, data.frame(statistic="declustered mean",
    value=dec_mean))
  dec_cellx <- dec_plot[which.min(abs(dec_plot$mean - dec_mean)),]$cell_size
  dec_stats <- rbind(dec_stats, data.frame(statistic="xcell",
    value=dec_cellx))
  dec_stats <- rbind(dec_stats, data.frame(statistic="ycell",
    value=dec_cellx * yz_aniso[1]))
  dec_stats <- rbind(dec_stats, data.frame(statistic="zcell",
    value=dec_cellx * yz_aniso[2]))
  dec_stats <- rbind(dec_stats, data.frame(statistic="minimum weight",
    value=min(dec_data$weight)))
  dec_stats <- rbind(dec_stats, data.frame(statistic="maximum weight",
    value=max(dec_data$weight)))

  # Clean up.
  if(!debug) {
    shell("del declus.par declus-in.dat declus-out.dat declus-stats.dat")
  }

  return(list(data = dec_data, plot_data = dec_plot, stats = dec_stats))

}
