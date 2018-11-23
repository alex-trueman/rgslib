
#' Interface to \code{kt3dn} 3D Kriging Program.
#'
#' Anomalies compared with raw program:
#'
#' \itemize{
#'   \item Cross and jacknife options not supported.
#'   \item Auto search optimization not supported (will run if minimum set to
#'     negative value, however no otion to change max and increment and only
#'     first 'realization' is recognized by this implimentation.)
#'   \item Kriging with external drift not supported  (will run if \code{ktype}
#'     is set to 5, but no option to set drift indicators, etc.).
#'   \item Input grid is required with a key value to define nodes to estimate.
#' }
#'
#' @param grid Data frame of output grid data. The first three columns are the
#'   coordinate columns as specified in \code{xyz}. The z-column must be present
#'   even if only x and y provided in \code{xyz} an can be set at a consatant
#'   such as 1. The fourth column contains an indicator of the the nodes to
#'   estimate as specified in \code{keyval}.
#' @param samples Data frame of sample data.
#' @param vars Character vector (length 1--2) of variables in \code{samples} to
#'   be estimated. First is primary, second is secondary.
#' @param griddims Numeric vector of xyz grid node spacing. Length 2 for 2D.
#' @param bhid Scalar character name of hole id column in \code{samples}. Only
#'   needed if \code{ndhmax} is > 0.
#' @param xyz Charcater vector of coordinate column names in \code{samples}.
#' @param keyval Scalar integer key value in \code{keygrid} that indicates nodes
#'   to be estiimated. Must be in column 4 after the three coordinate columns.
#' @param n_dataspac Scalar integer number fo drillhole neighbours for
#'   estimation of 2D drillhole spacing.
#' @param l_dataspac Scalar numeric composite length to use when estimating
#'   drillhole spacing in 3D data.
#' @param idbg Scalar integer debug level: 0 none; or 3, 5, 10.
#' @param ndis Integer vecots with number of discretization points in x, y and
#'   z. Use all 1s for point kriging.
#' @param minmax Integer vector with minimum and maximum number of samples in
#'   the search neighbourhood.
#' @param noct Scalar integer minimum samples for each octant. Use zero for no
#'   octants.
#' @param ndhmax Scalar integer maximum samples per drillhole. Use zero for
#'   deactivate. If > 0 must specify \code{bhid} column name. Only useful for 3D
#'   kriging.
#' @param srchdist Numeric vecotor of search radii in rotated x, y, and z.
#' @param srchang Numeric vector of search oriention, azimuth, dip, and tilt.
#' @param ktype Scalar integer kriging type: 0=SK, 1=OK, 2=LVM(resid),
#'   3=LVM((1-w)*m(u))) ,4=colo, 5=exdrift, 6=ICCK.
#' @param skmean Scalar numeric global mean grade for \code{ktype} 0, 4, 5, 6.
#' @param corr Scalar numeric correlation for \code{ktype} 4 or 6.
#' @param zvar Scalar numeric variance reduction factor for \code{ktype} 4.
#' @param mvario Variogram model data frame or list of models as \code{gstat}
#'   class "variogramModel" or "variogramModelList".
#' @param idw Boolean scalar do inverse distance weighting instead of kriging.
#' @param debug Boolean scalar, if \code{TRUE} don't delete system files.
#'
#' @return Data frame grid with estimated variable.
#' @export
kt3dnR <- function(grid, samples, vars, griddims=c(1, 1), bhid=NULL,
    xyz=c("x", "y"), keyval=0, n_dataspac=4, l_dataspac=6, idbg=0,
    ndis=c(3, 3, 1), minmax=c(16, 30), noct=2, ndhmax=0, srchdist=c(10, 10, 10),
    srchang=c(0, 0, 0), ktype=1, skmean=0, corr=0, zvar=0, mvario=NULL,
    idw=TRUE, debug=FALSE
    ) {

    # Make a GSLIB sample data file.
    samp_gslib <- samples[,c(bhid, xyz, vars)]
    write_gslib(samp_gslib, "xxsamples.xx")

    # Make a GSLIB grid file.
    write_gslib(grid, "xxkeyout.xx")
    # Create a grid definition for the paramter file.
    grid_def <- create_gslib_griddef(grid, dims=griddims, xyz=xyz)

    # Get column indices.
    vars_i <- paste(get_column_indices(samp_gslib, vars), collapse = "  ")
    if(length(xyz) == 3) {
        # 3D data with z-coordinate.
        xyz_i <- paste(get_column_indices(samp_gslib, xyz), collapse = "  ")
    } else if(length(xyz) == 2) {
        # 2D data, no z-coordinate.
        xyz_i <- paste(c( get_column_indices(samp_gslib, xyz), 0),
            collapse = "  ")
    } else {
        # Vector of coordinate names must be either length 2 or 3.
        return(FALSE)
    }
    if(is.null(bhid)) {
        bhid_i = 0
    } else {
        bhid_i <- get_column_indices(samp_gslib, bhid)
    }

    # Build parameters file.
    parstring <- paste0(
        "START OF PARAMETERS:  \n",
        "xxsamples.xx  \n",
        bhid_i, "  ", xyz_i, "  ",vars_i, "  \n",
        "-998.0  1.ee21  \n",
        "0  \n",
        "xxvk.dat  \n",
        "0  0  0  0  0  \n",
        "xdataspacing.sum  \n",
        n_dataspac, "  ", l_dataspac, "  \n",
        idbg, " 100  0  \n",
        "xxdebug.xx  \n",
        "xxkt3dn.xx  \n",
        grid_def["n_x"], " ", grid_def["min_x"], " ", grid_def["dim_x"], " \n",
        grid_def["n_y"], " ", grid_def["min_y"], " ", grid_def["dim_y"], " \n",
        grid_def["n_z"], " ", grid_def["min_z"], " ", grid_def["dim_z"], " \n",
        paste(ndis, collapse = "  "), "  \n",
        paste(minmax, collapse = "  "), "  100  10  \n",
        noct, "  ", ndhmax, "  \n",
        srchdist[1], "  ", srchdist[2], "  ", srchdist[3], "  \n",
        srchang[1], "  ", srchang[2], "  ", srchang[3], "  \n",
        ktype, "  \n",
        skmean, "  ", corr, "  ", zvar, "  \n",
        "0 0 0 0 0 0 0 0 0  \n",
        "0  \n",
        "xxextdrift.xx  \n",
        "4  \n",
        "xxkeyout.xx  \n",
        "4  ", keyval, "  "
    )
    # Variogram models.
    if(idw) {
        parstring <- c(
            parstring,
            "-1  0  \n"
            )
    } else {
        parstring <- c(
            parstring,
            paste0(mvario_parstring(mvario))
        )
    }

    # Write parameters to a file.
    file_conn <- file("xxkt3dn-par.xx")
    writeLines(parstring, file_conn)
    close(file_conn)

    # Run the program.
    shell("kt3dn xxkt3dn-par.xx")

    # Read in the estimated data.
    data <- read_gslib("xxkt3dn.xx")

    if(!debug) {shell("del xx*.xx")}

    return(data)

}
