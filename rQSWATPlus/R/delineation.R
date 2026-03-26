#' Run Watershed Delineation Using TauDEM
#'
#' Performs watershed delineation on a DEM using TauDEM tools via the
#' `traudem` package. This replicates the delineation workflow from
#' the QSWATPlus QGIS plugin.
#'
#' @param project A `qswat_project` object created by [qswat_setup()].
#' @param threshold Numeric. Flow accumulation threshold for stream
#'   definition (number of cells). If NULL, a default threshold of
#'   1% of total cells is used.
#' @param channel_threshold Numeric or NULL. Threshold for channel
#'   definition. Defaults to `threshold / 10` if NULL.
#' @param n_processes Integer. Number of MPI processes for TauDEM.
#'   Default is 1 (no parallelization).
#' @param quiet Logical. If TRUE, suppress TauDEM output messages.
#'   Default is FALSE.
#'
#' @return An updated `qswat_project` object with delineation results.
#'
#' @details
#' The delineation workflow follows these steps:
#' \enumerate{
#'   \item **Pit removal**: Fill pits/depressions in the DEM
#'   \item **D8 flow direction**: Calculate flow directions using D8
#'     algorithm
#'   \item **D-infinity flow direction**: Calculate slope and flow
#'     angles
#'   \item **D8 contributing area**: Calculate flow accumulation
#'   \item **Stream definition**: Apply threshold to identify streams
#'   \item **Move outlets**: Snap outlet points to nearest stream
#'     (if outlets provided)
#'   \item **Stream network**: Generate stream network topology and
#'     watershed boundaries
#' }
#'
#' Requires TauDEM to be installed and accessible. Use
#' `traudem::taudem_sitrep()` to verify your TauDEM installation.
#'
#' @examples
#' \dontrun{
#' # After qswat_setup()
#' project <- qswat_delineate(project, threshold = 100)
#' }
#' 
#' @export
qswat_delineate <- function(project,
                            threshold = NULL,
                            channel_threshold = NULL,
                            n_processes = 1L,
                            quiet = FALSE) {

  if (!inherits(project, "qswat_project")) {
    stop("'project' must be a qswat_project object.", call. = FALSE)
  }

  if (!requireNamespace("traudem", quietly = TRUE)) {
    stop(
      "Package 'traudem' is required for watershed delineation.\n",
      "Install it with: install.packages('traudem')\n",
      "Then install TauDEM: see vignette('taudem-installation', package = 'traudem')",
      call. = FALSE
    )
  }

  dem_file <- project$dem_file
  raster_dir <- file.path(project$project_dir, "Watershed", "Rasters")

  # Default threshold: 1% of total cells

  if (is.null(threshold)) {
    threshold <- max(1, floor(project$nrow * project$ncol * 0.01))
    message("Using default stream threshold: ", threshold, " cells")
  }
  if (is.null(channel_threshold)) {
    channel_threshold <- max(1, floor(threshold / 10))
    message("Using default channel threshold: ", channel_threshold, " cells")
  }

  project$stream_threshold <- threshold
  project$channel_threshold <- channel_threshold

  # Set n_processes for traudem
  if (n_processes > 1L) {
    old_np <- Sys.getenv("TAUDEM_NPROC")
    Sys.setenv(TAUDEM_NPROC = n_processes)
    on.exit(Sys.setenv(TAUDEM_NPROC = old_np), add = TRUE)
  }

  # --- Step 1: Pit Removal ---
  if (!quiet) message("Step 1/7: Removing pits from DEM...")
  fel_file <- traudem::taudem_pitremove(
    input_elevation = dem_file,
    output_elevation = file.path(raster_dir, "fel.tif"),
    quiet = quiet
  )
  project$fel_file <- fel_file

  # --- Step 2: D8 Flow Direction ---
  if (!quiet) message("Step 2/7: Computing D8 flow directions...")
  d8_result <- traudem::taudem_d8flowdir(
    input_elevation = fel_file,
    output_d8flowdir = file.path(raster_dir, "p.tif"),
    output_d8slope = file.path(raster_dir, "sd8.tif"),
    quiet = quiet
  )
  project$p_file <- d8_result[["output_d8flowdir_grid"]]
  project$sd8_file <- d8_result[["output_d8slopes_grid"]]

  # --- Step 3: D-infinity Flow Direction ---
  if (!quiet) message("Step 3/7: Computing D-infinity flow directions...")
  dinf_result <- traudem::taudem_exec(
    program = "DinfFlowDir",
    args = c("-fel", fel_file,
             "-slp", file.path(raster_dir, "slp.tif"),
             "-ang", file.path(raster_dir, "ang.tif")),
    quiet = quiet
  )
  project$slp_file <- file.path(raster_dir, "slp.tif")
  project$ang_file <- file.path(raster_dir, "ang.tif")

  # --- Step 4: D8 Contributing Area ---
  if (!quiet) message("Step 4/7: Computing D8 contributing area...")
  ad8_args <- list(
    input_d8flowdir = project$p_file,
    output_contributing_area = file.path(raster_dir, "ad8.tif"),
    quiet = quiet
  )
  if (!is.null(project$outlet_file)) {
    ad8_args$outlet_file = project$outlet_file
  }
  ad8_file <- do.call(traudem::taudem_aread8, ad8_args)
  project$ad8_file <- ad8_file

  # --- Step 5: Stream Definition (Threshold) ---
  if (!quiet) message("Step 5/7: Defining streams with threshold = ", threshold, "...")
  src_stream_file <- traudem::taudem_threshold(
    input_area = project$ad8_file,
    output_stream_raster = file.path(raster_dir, "src_stream.tif"),
    threshold = threshold,
    quiet = quiet
  )
  project$src_stream_file <- src_stream_file

  # Channel threshold (finer network)
  if (!quiet) message("  Defining channels with threshold = ", channel_threshold, "...")
  src_channel_file <- traudem::taudem_threshold(
    input_area = project$ad8_file,
    output_stream_raster = file.path(raster_dir, "src_channel.tif"),
    threshold = channel_threshold,
    quiet = quiet
  )
  project$src_channel_file <- src_channel_file

  # --- Step 6: Move Outlets to Streams (if outlets provided) ---
  if (!is.null(project$outlet_file)) {
    if (!quiet) message("Step 6/7: Snapping outlets to streams...")
    moved_outlet <- traudem::taudem_moveoutletstostream(
      input_d8flowdir = project$p_file,
      input_stream_raster = project$src_stream_file,
      outlet_file = project$outlet_file,
      output_moved_outlet = file.path(project$project_dir, "Watershed",
                                       "Shapes", "moved_outlet.shp"),
      quiet = quiet
    )
    project$outlet_file <- moved_outlet
  } else {
    if (!quiet) message("Step 6/7: No outlets provided, skipping outlet snapping.")
  }

  # --- Step 7: Stream Network ---
  if (!quiet) message("Step 7/7: Generating stream network...")
  shape_dir <- file.path(project$project_dir, "Watershed", "Shapes")

  stream_args <- c(
    "-fel", project$fel_file,
    "-p", project$p_file,
    "-ad8", project$ad8_file,
    "-src", project$src_stream_file,
    "-ord", file.path(raster_dir, "ord.tif"),
    "-tree", file.path(project$project_dir, "Watershed", "Text", "tree.dat"),
    "-coord", file.path(project$project_dir, "Watershed", "Text", "coord.dat"),
    "-net", file.path(shape_dir, "stream.shp"),
    "-w", file.path(raster_dir, "w.tif")
  )
  if (!is.null(project$outlet_file)) {
    stream_args <- c(stream_args, "-o", project$outlet_file)
  }

  traudem::taudem_exec(
    program = "StreamNet",
    args = stream_args,
    quiet = quiet
  )

  project$ord_file <- file.path(raster_dir, "ord.tif")
  project$stream_file <- file.path(shape_dir, "stream.shp")
  project$watershed_file <- file.path(raster_dir, "w.tif")
  
  # Check if files were created
  if (!file.exists(project$stream_file)) {
    cli::cli_abort("Stream shapefile not created: 
                   {.file {project$stream_file}}")
  }
  if (!file.exists(project$watershed_file)) {
    cli::cli_abort("Watershed raster not created: 
                   {.file {project$watershed_file}}")
  }
  

  # Also create channel network with finer threshold
  if (!quiet) message("  Generating channel network...")
  channel_args <- c(
    "-fel", project$fel_file,
    "-p", project$p_file,
    "-ad8", project$ad8_file,
    "-src", project$src_channel_file,
    "-ord", file.path(raster_dir, "ord_channel.tif"),
    "-tree", file.path(project$project_dir, "Watershed", "Text",
                       "tree_channel.dat"),
    "-coord", file.path(project$project_dir, "Watershed", "Text",
                        "coord_channel.dat"),
    "-net", file.path(shape_dir, "channel.shp"),
    "-w", file.path(raster_dir, "w_channel.tif")
  )
  if (!is.null(project$outlet_file)) {
    channel_args <- c(channel_args, "-o", project$outlet_file)
  }

  traudem::taudem_exec(
    program = "StreamNet",
    args = channel_args,
    quiet = quiet
  )

  project$channel_file <- file.path(shape_dir, "channel.shp")
  project$channel_watershed_file <- file.path(raster_dir, "w_channel.tif")

  if (!quiet) message("Delineation complete.")
  return(project)
}


#' Create Stream/Channel Network from Delineation
#'
#' Reads the stream and channel network shapefiles generated during
#' delineation and extracts topology information (connectivity,
#' stream order, lengths, slopes).
#'
#' @param project A `qswat_project` object that has been through
#'   [qswat_delineate()].
#'
#' @return An updated `qswat_project` object with stream topology
#'   information.
#'
#' @details
#' Extracts the following topology data from TauDEM's StreamNet output:
#' \itemize{
#'   \item Stream links and their downstream connections
#'   \item Subbasin (watershed) identifiers
#'   \item Stream orders (Strahler)
#'   \item Channel lengths and slopes
#'   \item Drainage areas
#' }
#'
#' @examples
#' \dontrun{
#' # After qswat_delineate()
#' project <- qswat_create_streams(project)
#' }
#'
#' @export
qswat_create_streams <- function(project) {

  if (!inherits(project, "qswat_project")) {
    stop("'project' must be a qswat_project object.", call. = FALSE)
  }
  if (is.null(project$stream_file)) {
    stop("Delineation must be run first. Use qswat_delineate().", call. = FALSE)
  }
  if (!file.exists(project$stream_file)) {
    stop("Stream file not found: ", project$stream_file, call. = FALSE)
  }

  # Read stream network shapefile
  streams <- sf::st_read(project$stream_file, quiet = TRUE)

  # Extract topology from TauDEM StreamNet output
  # Standard TauDEM field names
  topology <- data.frame(
    LINKNO = streams$LINKNO,
    DSLINKNO = streams$DSLINKNO,
    USLINKNO1 = if ("USLINKNO1" %in% names(streams)) streams$USLINKNO1 else NA,
    USLINKNO2 = if ("USLINKNO2" %in% names(streams)) streams$USLINKNO2 else NA,
    strmOrder = if ("strmOrder" %in% names(streams)) streams$strmOrder else NA,
    Length = if ("Length" %in% names(streams)) streams$Length else NA,
    WSAreaKm2 = if ("WSAreaKm2" %in% names(streams)) streams$WSAreaKm2 else
      if ("DSContArea" %in% names(streams)) streams$DSContArea else NA,
    WSNO = if ("WSNO" %in% names(streams)) streams$WSNO else NA,
    stringsAsFactors = FALSE
  )

  project$stream_topology <- topology
  project$streams_sf <- streams

  # Read channel network if available
  if (!is.null(project$channel_file) && file.exists(project$channel_file)) {
    channels <- sf::st_read(project$channel_file, quiet = TRUE)
    channel_topology <- data.frame(
      LINKNO = channels$LINKNO,
      DSLINKNO = channels$DSLINKNO,
      WSNO = if ("WSNO" %in% names(channels)) channels$WSNO else NA,
      strmOrder = if ("strmOrder" %in% names(channels)) channels$strmOrder else NA,
      Length = if ("Length" %in% names(channels)) channels$Length else NA,
      stringsAsFactors = FALSE
    )
    project$channel_topology <- channel_topology
    project$channels_sf <- channels
  }

  # Read watershed raster for subbasin mapping
  if (!is.null(project$watershed_file) && file.exists(project$watershed_file)) {
    project$watershed_rast <- terra::rast(project$watershed_file)
  }

  message("Stream topology extracted: ", nrow(topology), " stream links")
  return(project)
}
