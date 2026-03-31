# Helper for TauDEM skip guards used across workflow integration tests
skip_if_no_taudem <- function() {
  skip_if_not_installed("traudem")
  taudem_ok <- tryCatch({
    traudem::taudem_sitrep()
    TRUE
  }, error = function(e) FALSE)
  skip_if(!taudem_ok, "TauDEM not available")
}

test_that("stepped workflow works", {
  skip_if_no_taudem()

  dem <- system.file("extdata", "ravn_dem.tif", package = "rQSWATPlus")
  landuse <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
  soil <- system.file("extdata", "ravn_soil.tif", package = "rQSWATPlus")
  lu_lookup <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
  soil_lookup <- system.file("extdata", "ravn_soil.csv", package = "rQSWATPlus")
  outlet <- system.file("extdata", "ravn_outlet.shp", package = "rQSWATPlus")

  skip_if(dem == "", "Example data not available")

  project_dir <- file.path(tempdir(), "ravn_swatplus")
  on.exit(unlink(project_dir, recursive = TRUE), add = TRUE)

  # Initialize the project
  project <- qswat_setup(
    project_dir = project_dir,
    dem_file = dem,
    landuse_file = landuse,
    soil_file = soil,
    landuse_lookup = lu_lookup,
    soil_lookup = soil_lookup,
    outlet_file = outlet
  )

  project <- qswat_delineate(
    project,
    threshold = 500,
    channel_threshold = 50,
    quiet = TRUE
  )
  project <- qswat_create_streams(project)

  # Read lookup tables
  lu <- qswat_read_landuse_lookup(lu_lookup)
  soil_lkp <- qswat_read_soil_lookup(soil_lookup)

  # Define slope classes: 0-5%, 5-15%, and >15%
  slopes <- qswat_create_slope_classes(c(0, 5, 15, 9999))

  # Create HRUs with area thresholds to filter small units
  project <- qswat_create_hrus(
    project,
    landuse_lookup = lu,
    soil_lookup = soil_lkp,
    slope_classes = slopes,
    landuse_threshold = 5,
    soil_threshold = 5,
    slope_threshold = 0
  )

  project <- qswat_write_database(project, overwrite = TRUE)
  expect_true(file.exists(project$db_file))
})

test_that("qswat_run works", {
  skip_if_no_taudem()

  dem <- system.file("extdata", "ravn_dem.tif", package = "rQSWATPlus")
  landuse <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
  soil <- system.file("extdata", "ravn_soil.tif", package = "rQSWATPlus")
  lu_lookup <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
  soil_lookup <- system.file("extdata", "ravn_soil.csv", package = "rQSWATPlus")
  outlet <- system.file("extdata", "ravn_outlet.shp", package = "rQSWATPlus")

  skip_if(dem == "", "Example data not available")

  proj_dir <- file.path(tempdir(), "ravn_quick")
  on.exit(unlink(proj_dir, recursive = TRUE), add = TRUE)

  project <- qswat_run(
    project_dir = proj_dir,
    dem_file = dem,
    landuse_file = landuse,
    soil_file = soil,
    landuse_lookup = lu_lookup,
    soil_lookup = soil_lookup,
    outlet_file = outlet,
    threshold = 500,
    slope_breaks = c(0, 5, 15, 9999),
    landuse_threshold = 5,
    soil_threshold = 5,
    db_file = "swat.db"
  )

  expect_true(file.exists(project$db_file))
})

# Full end-to-end integration test: build project and verify SWAT+ Editor readiness
test_that("example dataset produces a SWAT+ Editor-ready database", {
  skip_if_no_taudem()

  dem        <- system.file("extdata", "ravn_dem.tif",     package = "rQSWATPlus")
  landuse    <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
  soil       <- system.file("extdata", "ravn_soil.tif",    package = "rQSWATPlus")
  lu_lookup  <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
  soil_lookup <- system.file("extdata", "ravn_soil.csv",   package = "rQSWATPlus")
  outlet     <- system.file("extdata", "ravn_outlet.shp",  package = "rQSWATPlus")

  skip_if(dem == "", "Example data not available")

  proj_dir <- file.path(tempdir(), "ravn_editor_ready")
  on.exit(unlink(proj_dir, recursive = TRUE), add = TRUE)

  # ---- Run the full workflow ------------------------------------------------
  project <- qswat_run(
    project_dir    = proj_dir,
    dem_file       = dem,
    landuse_file   = landuse,
    soil_file      = soil,
    landuse_lookup = lu_lookup,
    soil_lookup    = soil_lookup,
    outlet_file    = outlet,
    threshold      = 500,
    slope_breaks   = c(0, 5, 15, 9999),
    landuse_threshold = 5,
    soil_threshold    = 5,
    quiet          = TRUE
  )

  db_file <- project$db_file
  expect_true(file.exists(db_file))

  # ---- Reference database was copied to the project folder -----------------
  ref_db <- file.path(proj_dir, "swatplus_datasets.sqlite")
  expect_true(
    file.exists(ref_db),
    label = "swatplus_datasets.sqlite exists in project folder"
  )

  # ---- project_config is correctly populated --------------------------------
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  pc <- DBI::dbGetQuery(con, "SELECT * FROM project_config")
  expect_equal(nrow(pc), 1L, label = "project_config has exactly one row")
  expect_equal(
    pc$reference_db,
    normalizePath(ref_db, mustWork = FALSE),
    label = "reference_db points to the copied dataset"
  )
  expect_equal(pc$delineation_done, 1L)
  expect_equal(pc$hrus_done, 1L)
  expect_equal(pc$imported_gis, 1L)
  expect_equal(pc$use_gwflow, 0L, label = "gwflow disabled by default")

  # ---- Core GIS tables are populated ----------------------------------------
  subs <- DBI::dbGetQuery(con, "SELECT * FROM gis_subbasins")
  expect_gt(nrow(subs), 0L, label = "gis_subbasins has rows")

  hrus <- DBI::dbGetQuery(con, "SELECT * FROM gis_hrus")
  expect_gt(nrow(hrus), 0L, label = "gis_hrus has rows")

  chans <- DBI::dbGetQuery(con, "SELECT * FROM gis_channels")
  expect_gt(nrow(chans), 0L, label = "gis_channels has rows")

  lsus <- DBI::dbGetQuery(con, "SELECT * FROM gis_lsus")
  expect_gt(nrow(lsus), 0L, label = "gis_lsus has rows")

  routing <- DBI::dbGetQuery(con, "SELECT * FROM gis_routing")
  expect_gt(nrow(routing), 0L, label = "gis_routing has rows")

  # ---- SWAT+ Editor compatibility check passes ------------------------------
  result <- qswat_check_database(db_file, verbose = FALSE)
  expect_true(
    result$passed,
    label = paste(
      "Database passes SWAT+ Editor compatibility check.",
      if (!result$passed) paste("Errors:", paste(result$errors, collapse = "; ")) else ""
    )
  )
  expect_equal(
    sum(result$checks$status == "FAIL"), 0L,
    label = "No failed checks"
  )
})


