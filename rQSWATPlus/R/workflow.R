#' Run Complete QSWATPlus Workflow
#'
#' Convenience function that runs the full QSWATPlus workflow in a single
#' call: project setup, watershed delineation, stream network creation,
#' HRU generation, and database output.
#'
#' @param project_dir Character. Path to the project directory.
#' @param dem_file Character. Path to the DEM raster file.
#' @param landuse_file Character. Path to the land use raster file.
#' @param soil_file Character. Path to the soil raster file.
#' @param landuse_lookup Character. Path to the land use lookup CSV.
#' @param soil_lookup Character. Path to the soil lookup CSV.
#' @param outlet_file Character or NULL. Optional outlet shapefile.
#' @param threshold Numeric or NULL. Stream threshold (cells).
#'   If NULL, 1 percent of total cells is used.
#' @param channel_threshold Numeric or NULL. Channel threshold.
#' @param slope_breaks Numeric vector. Slope class boundaries in
#'   percent. Default `c(0, 9999)` for a single class.
#' @param landuse_threshold Numeric. Min land use percent. Default 0.
#' @param soil_threshold Numeric. Min soil percent. Default 0.
#' @param slope_threshold Numeric. Min slope percent. Default 0.
#' @param n_processes Integer. Number of MPI processes. Default 1.
#' @param quiet Logical. Suppress output. Default FALSE.
#' @param db_file Character or NULL. Output database path.
#' @param usersoil Character or NULL. Soil physical properties dataset to
#'   use when writing the project database. Passed to [qswat_setup()] and
#'   stored in the project object. Accepted values are the same as for
#'   [qswat_setup()]: `"FAO_usersoil"`, `"global_usersoil"`, a CSV file
#'   path, or `NULL` (default, leaves `global_usersoil` empty).
#'
#' @return A `qswat_project` object with all results.
#'
#' @examples
#' \dontrun{
#' dem <- system.file("extdata", "ravn_dem.tif", package = "rQSWATPlus")
#' landuse <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
#' soil <- system.file("extdata", "ravn_soil.tif", package = "rQSWATPlus")
#' lu_lookup <- system.file("extdata", "ravn_landuse.csv",
#'                          package = "rQSWATPlus")
#' soil_lookup <- system.file("extdata", "ravn_soil.csv",
#'                            package = "rQSWATPlus")
#' outlet <- system.file("extdata", "ravn_outlet.shp", package = "rQSWATPlus")
#'
#' project <- qswat_run(
#'   project_dir = tempdir(),
#'   dem_file = dem,
#'   landuse_file = landuse,
#'   soil_file = soil,
#'   landuse_lookup = lu_lookup,
#'   soil_lookup = soil_lookup,
#'   outlet_file = outlet,
#'   threshold = 100,
#'   slope_breaks = c(0, 5, 15, 9999)
#' )
#' }
#'
#' @export
qswat_run <- function(project_dir,
                      dem_file,
                      landuse_file,
                      soil_file,
                      landuse_lookup,
                      soil_lookup,
                      outlet_file = NULL,
                      threshold = NULL,
                      channel_threshold = NULL,
                      slope_breaks = c(0, 9999),
                      landuse_threshold = 0,
                      soil_threshold = 0,
                      slope_threshold = 0,
                      n_processes = 1L,
                      quiet = FALSE,
                      db_file = NULL,
                      usersoil = NULL) {

  # Step 1: Setup
  if (!quiet) message("=== Step 1/5: Setting up project ===")
  project <- qswat_setup(
    project_dir = project_dir,
    dem_file = dem_file,
    landuse_file = landuse_file,
    soil_file = soil_file,
    landuse_lookup = landuse_lookup,
    soil_lookup = soil_lookup,
    outlet_file = outlet_file,
    usersoil = usersoil,
    overwrite = TRUE
  )

  # Step 2: Delineation
  if (!quiet) message("\n=== Step 2/5: Running watershed delineation ===")
  project <- qswat_delineate(
    project = project,
    threshold = threshold,
    channel_threshold = channel_threshold,
    n_processes = n_processes,
    quiet = quiet
  )

  # Step 3: Stream network
  if (!quiet) message("\n=== Step 3/5: Creating stream network ===")
  project <- qswat_create_streams(project)

  # Step 4: HRU creation
  if (!quiet) message("\n=== Step 4/5: Creating HRUs ===")
  lu_lookup <- qswat_read_landuse_lookup(project$landuse_lookup)
  soil_lkp <- qswat_read_soil_lookup(project$soil_lookup)
  slopes <- qswat_create_slope_classes(slope_breaks)

  project <- qswat_create_hrus(
    project = project,
    landuse_lookup = lu_lookup,
    soil_lookup = soil_lkp,
    slope_classes = slopes,
    landuse_threshold = landuse_threshold,
    soil_threshold = soil_threshold,
    slope_threshold = slope_threshold
  )

  # Step 5: Write database
  if (!quiet) message("\n=== Step 5/5: Writing database ===")
  project <- qswat_write_database(project, db_file = db_file, overwrite = TRUE)

  if (!quiet) message("\n=== QSWATPlus workflow complete! ===")
  return(project)
}
