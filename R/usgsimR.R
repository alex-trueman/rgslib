#' Simulation Using \code{usgsim} Fortran Program from CCG.
#'
#' \code{usgsimR} is an interface to the CCG simulation program \code{usgsim},
#' which uses SGS. Some functionality of the Fortran program is not implimented
#' in this interface:
#'     - Weight field.
#'     - Rock types.
#'     - Seconday variables.
#'     - Normal-score transformation reference distributions.
#'     - Trend data.
#'     - Only writes output format 0: regular.
#'
#' @param data Data frame containing coordinates and variables to condition the
#'   simulations.
#' @param mvario Variogram model data frame or list of models as \code{gstat}
#'   class "variogramModel" or "variogramModelList".
#' @param vars Character vector of column names to be simulated.
#' @param corr Correalation matrix (from \code{cor}) for \code{vars}.
#' @param xyz Character vector of coordinate column names.
#' @param n_realz Scalar integer number of simulation realizations.
#' @param seed Scalar integer seed for renadom number generation.
#' @param xdef Numeric vector with x-axis grid definition: number, origin, size.
#' @param ydef Numeric vector with y-axis grid definition: number, origin, size.
#' @param zdef Numeric vector with z-axis grid definition: number, origin, size.
#' @param simout Name of GeoEase grid file to contain output simulations.
#' @param imputeout Name of file to contain imputed data in heterotopic case.
#' @param debuglevel Scalar integer either 0 (none) or 1, 2, or 3 for increasing
#'   levels of reporting.
#' @param n_prev Scalar integer number of previously simulated nodes to use.
#' @param srchdist Numeric vector search readii: x, y, z.
#' @param srchang  Numeric vector search angles: ang1, ang2, ang3.
#' @param sortmethod Scalar numeric: sort by distance (0) or covariance (1).
#' @param covarsort Numeric vector: if sorting by covariance, indicate variogram
#'   rock type, head, tail to use.
#' @param clip Scalar boolean: clip data to grid.
#' @param assign Scalar integer: assign to the grid, 0=none, 1=nearest, and
#'   2=average.
#' @param trans Scalar boolean: perform normal score transormation of input
#'   samples and backtransofrmation of simulation.
#' @param nquant Scalar integer: number of quantiles to keep from transform.
#' @param krigmethod Scalar integer: kriging method: 1=independent, 2=CCK, 4=CK,
#'   5=ICCK, 6=BU.
#' @param altflag Scalar integer: option for primary variables if BU; or for
#'   secondary variables if CK.
#' @param cosim Scalar boolean: perform cosimulation of multiple variables.
#' @param domgrid Data frame containing x, y, and, optionally z coordinate
#'   columns that define the domain grid and a domain or zone field(s). Output
#'   data will be contain  domain code for those coorindate points. Coordinate
#'   columns must be the same as \code{xyz}.
#' @importFrom magrittr %<>%
#' @importFrom dplyr semi_join
#' @return Data frame of simulation results.
#' @export
usgsimR <- function(
    data, mvario, vars, corr, xyz=c("x", "y"), n_realz=1, seed=60221409,
    xdef=c(50, 0.5, 1.0), ydef=c(50, 0.5, 1.0),
    zdef=c(50, 0.5, 1.0), simout='sgsim', imputeout='impute',
    debuglevel=0, n_prev=12, srchdist=c(10, 10, 10), srchang=c(0, 0, 0),
    sortmethod=0, covarsort=c(1, 1, 1), clip=TRUE, assign=1, trans=TRUE,
    nquant=200, krigmethod=2, altflag=0, cosim=TRUE, domgrid=NULL
){

    # Arguments are hard-coded for now but some support for future integration.
    weight <- NULL # Weight field in `data`.
    rocktype <- NULL # Rock type field in `data`.
    nrefdistrib <- 0 # Number of reference distributions for normal scores.

    # Make a GSLIB sample data file.
    data_gslib <- data[,c(xyz, vars, weight, rocktype)]
    write_gslib(data_gslib, "samples.dat")

    # Get column indices.
    nvars <- length(vars)
    vars_i <- which(colnames(data_gslib) %in% vars)
    if(length(xyz) == 3) {
        # 3D data with z-coordinate.
        xyz_i <- paste(which(colnames(data_gslib) %in% xyz), collapse = "  ")
    } else if(length(xyz) == 2) {
        # 2D data, no z-coordinate.
        xyz_i <- paste(c(
            which(colnames(data_gslib) %in% xyz),
            0),
            collapse = "  ")
    } else {
        # Vector of coordinate names must be either length 2 or 3.
        return(FALSE)
    }
    if(is.null(weight)) {
        weight_i = 0
    } else {
        weight_i <- paste(
            which(colnames(data_gslib) %in% weight), collapse = "  ")
    }
    if(is.null(rocktype)) {
        rocktype_i = 0
    } else {
        rocktype_i <- paste(
            which(colnames(data_gslib) %in% rocktype), collapse = "  ")
    }

    # Build parameter file.
    parstring <- paste0(
        "START OF MAIN: \n",
        n_realz, "  \n",
        nvars, "  \n",
        "0  \n",
        seed, "  \n",
        xdef[1], "  ", xdef[2], "  ", xdef[3], "  \n",
        ydef[1], "  ", ydef[2], "  ", ydef[3], "  \n",
        zdef[1], "  ", zdef[2], "  ", zdef[3], "  \n",
        simout, ".out  \n",
        "0  \n",
        imputeout, ".out  \n",
        debuglevel, "  \n",
        "sgsim.dbg  \n",
        "\n",
        "\n",
        "START OF SRCH:\n",
        n_prev, "  \n",
        srchdist[1], "  ", srchdist[2], "  ", srchdist[3], "  \n",
        srchang[1], "  ", srchang[2], "  ", srchang[3], "  \n",
        sortmethod, "  \n",
        covarsort[1], "  ", covarsort[2], "  ", covarsort[3], "  \n",
        "\n",
        "\n",
        "START OF VARG: "
    )
    # Variogram models. Is either a single model in a data frame of a series
    # of models in a list of data frames.
    if(is.data.frame(mvario)) {
        parstring <- c(
            parstring,
            paste0(
                "1  \n",
                "1  1  1",
                mvario_parstring(mvario)
            )
        )
    } else {
        parstring <- c(
            parstring,
            paste0(length(mvario), "  ")
        )
        for(df in seq_along(mvario)){
            mvario_df <- mvario[[df]]
            parstring <- c(parstring,paste0(
                "1  ", mvario_df[1,10], "  ", mvario_df[1,11], "  "
            ))
            parstring <- c(
                parstring,
                mvario_parstring(mvario_df)
            )
        }
    }
    # Data file.
    parstring <- c(
        parstring,
        paste0(
            "\n",
            "START OF DATA: \n",
            "samples.dat  \n",
            xyz_i, "  ", weight_i, "  ", rocktype_i, "  \n",
            paste(vars_i, collapse = "  "), "  \n",
            as.numeric(clip),  "  \n",
            assign, "  \n",
            "-999  1.0e21 \n",
            "\n",
            "\n",
            "START OF TRAN: \n",
            as.numeric(trans), "  \n",
            nquant, "  \n",
            nvars, "  "
        )
    )
    # Variable limits for normal score transormation.
    for(var in seq_along(vars)) {
        maxvar <- round(max(data_gslib[,vars[var]]) * 1.01, 4)
        parstring <- c(
            parstring, paste0("1  ", vars_i[var], "  0  ", maxvar, "  ")
        )
    }
    # Refernece distribution.
    parstring <- c(
        parstring,
        paste0(nrefdistrib, "  "),
        "1  1   ",
        "ref11.dat  ",
        "1  0  ",
        "1  2  ",
        "ref12.dat  ",
        "1  0  "
    )
    # MULT parameters.
    parstring <- c(
        parstring,
        "\n",
        "START OF MULT: ",
        paste0(
            krigmethod, "  \n",
            altflag, "  \n",
            as.numeric(cosim), "  \n",
            "1  "
        )
    )
    # Correlation matrix.
    for(i in 1:length(vars)) {
        parstring <- c(parstring, paste(corr[i,], collapse = "  "))
    }

    # Write parameters to a file.
    file_conn <- file("usgsim.par")
    writeLines(parstring, file_conn)
    close(file_conn)

    # Run the program.
    shell("usgsim usgsim.par")

    # Import the simulated data.
    sims <- read_gslib_usgsim(paste0(simout, ".out"), vars)

    # Assign domain code to the grid.
    if(!is.null(domgrid)) {
        sims %<>%
            left_join(domgrid, by = xyz)
    }

    return(sims)
}

