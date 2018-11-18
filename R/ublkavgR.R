#' Scale Grid Size Up or Down Using \code{ublkavg}.
#'
#' \code{ublkavgR} is an interface to CCG Fortran program \code{ublkavg}. In
#' this implimentation the scaling of the grid is done via a \code{factor]
#' argument. \code{factor} can be a numeric vector with an a separate factor for
#' each dimension. Alternatively it can be a single number, in which case, it
#' will be applied to each dimension.
#'
#' There is currently an issue with the implimentation of the \code{domain}
#' argument. It is supposed to produce a variable 0 to 1 in the output grid,
#' which is the average of a domain indicator. It current gives wrong result and
#' I am not sure if it is this implimentation of the Fortran program at fault.
#'
#' @param data Data frame containing x, y, and, optionally z coordinates and one
#'   or more fields to average or assign by dominance.
#' @param vars Character vecotr of variables to average or assign by dominance.
#' @param domain Character vector for domain field to assign by dominance.
#' @param xyz Character vector of coordinate field names.
#' @param weights Charcater vector of weighting field name(s).If one field it is
#'   applied to all fields except \code{domain}.
#' @param factor Numeric positive number: multiplicative factor to scale grid.
#' @param minfrac Minimimum fraction of upscaled grid node that is represented
#'   by the input grid.
#' @param avg_method Integer vector of averaging methods to use: 0 = arithmetic
#'   mean; 1 = geometric mean; 2 = harmonic mean; 3 = majority rules (for
#'   categorical variables)
#' @param append_frac Scalar boolean: append fraction field to output.
#' @param indims Numeric vector of grid dimensions for input grid.
#'
#' @return Data frame of re-scaled grid.
#' @export
ublkavgR <- function(
    data, vars, domain=NULL, xyz=c("x", "y"), weights=NULL, factor=2,
    minfrac=1e-5, avg_method=0, append_frac=FALSE, indims=c(1, 1, 1)
) {

    realz <- length(unique(data$r))

    # Simplify `data`.
    data_gslib <- data[,c(xyz, vars, domain)]

    # Get variable column indices.
    vars_i <- get_column_indices(data_gslib, vars)
    nvars <- length(vars)
    # Get coordinate column indices for 2D or 3D.
    if(length(xyz) == 3){
        xyz_i <- get_column_indices(data_gslib, xyz)
    } else if(length(xyz) == 2) {
        xyz_i <- c(get_column_indices(data_gslib, xyz), 0)
        # Must be 2 or 3 dimensional.
    } else {
        return(FALSE)
    }
    # Get weight column indices.
    if(is.null(weights)) {
        weights_i <- rep(0, nvars)
    } else {
        weights_i <- get_column_indices(data_gslib, weights)
        # Zero pad if necessary.
        if(length(weights) < nvars) {
            weights_i <- c(weights_i, rep(0, nvars - length(weights)))
            # Trim if too long.
        } else {
            weights_i <- weights_i[1:nvars]
        }
    }

    # Averaging method.
    if(length(avg_method) == 1) {
        avg_method <- rep(avg_method, nvars)
    } else if(length(avg_method) < nvars) {
        avg_method <- c(avg_method,
            rep(avg_method[length(avg_method)], nvars - length(avg_method)))
    } else {
        avg_method <- avg_method[1:nvars]
    }

    # Domain column index.
    if(!is.null(domain)) {
        # Update parameter variables.
        vars <- c(vars, domain)
        nvars <- nvars + 1
        vars_i <- append(vars_i, get_column_indices(data_gslib, domain))
        avg_method <- append(avg_method, 0)
        weights_i <- append(weights_i, 0)
        # Convert domain to indicator for averaging.
        data_gslib[,domain] <- as.numeric(!is.na(data_gslib[,domain]))
    }

    # Write out the data.
    write_gslib(data_gslib, "grid-in.dat", griddim=indims, gridxyz=xyz,
        gridrealz=realz)


    # Create parameter file.
    parstring <- c(
        "START OF PARAMETERS  ",
        "grid-in.dat   ",
        paste0(nvars, "  "),
        paste(vars_i, collapse = "  "),
        paste(weights_i, collapse = "  "),
        "-998.0  1.0e21  "
    )
    # Get input grid definition.
    grid_def <- create_gslib_griddef(data_gslib, indims, xyz, realz)
    parstring <- c(
        parstring,
        paste0(
            grid_def["n_x"], "  ", grid_def["min_x"], "  ", grid_def["dim_x"], " \n",
            grid_def["n_y"], "  ", grid_def["min_y"], "  ", grid_def["dim_y"], " \n",
            grid_def["n_z"], "  ", grid_def["min_z"], "  ", grid_def["dim_z"], "  "
        )
    )
    # Use factor to calculate output grid definition.
    out_def <- grid_def
    out_def[c("dim_x", "dim_y", "dim_z")] <-
        grid_def[c("dim_x", "dim_y", "dim_z")] * factor
    out_def[c("n_x", "n_y", "n_z")] <-
        ceiling(grid_def[c("n_x", "n_y", "n_z")] / factor)
    out_def[c("min_x", "min_y", "min_z")] <-
        grid_def[c("min_x", "min_y", "min_z")] -
        (0.5 * grid_def[c("dim_x", "dim_y", "dim_z")]) + 0.5 *
        out_def[c("dim_x", "dim_y", "dim_z")]
    parstring <- c(
        parstring,
        paste0(
            out_def["n_x"], "  ", out_def["min_x"], "  ", out_def["dim_x"], " \n",
            out_def["n_y"], "  ", out_def["min_y"], "  ", out_def["dim_y"], " \n",
            out_def["n_z"], "  ", out_def["min_z"], "  ", out_def["dim_z"], "  "
        )
    )
    # Additional parameters.
    parstring <- c(
        parstring,
        paste0(
            realz, "  \n",
            minfrac, "  "
        )
    )
    parstring <- c(
        parstring,
        paste(avg_method, collapse = "  "),
        "blkavg.out  ",
        paste0(as.numeric(append_frac), "  ")
    )

    # Write parameters to a file.
    file_conn <- file("ublkavg.par")
    writeLines(parstring, file_conn)
    close(file_conn)

    # Run the programme.
    shell("ublkavg ublkavg.par")

    sims <- read_gslib_usgsim("blkavg.out", vars, out_def)

    return(sims)

}
