test_that("qswat_delineate requires qswat_project", {
  expect_error(
    qswat_delineate(list()),
    "qswat_project"
  )
})

test_that("qswat_create_streams requires delineation", {
  project <- structure(list(stream_file = NULL), class = "qswat_project")
  expect_error(
    qswat_create_streams(project),
    "Delineation must be run first"
  )
})

# Integration test that requires TauDEM - skipped if not available
test_that("full delineation workflow runs with TauDEM", {
  skip_if_not_installed("traudem")

  # Check TauDEM availability
  taudem_ok <- tryCatch({
    traudem::taudem_sitrep()
    TRUE
  }, error = function(e) FALSE)
  skip_if(!taudem_ok, "TauDEM not available")

  dem <- system.file("extdata", "ravn_dem.tif", package = "rQSWATPlus")
  landuse <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
  soil <- system.file("extdata", "ravn_soil.tif", package = "rQSWATPlus")
  lu_lookup <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
  soil_lookup <- system.file("extdata", "ravn_soil.csv", package = "rQSWATPlus")

  skip_if(dem == "", message = "Example data not available")

  proj_dir <- file.path(tempdir(), "test_delineation")
  on.exit(unlink(proj_dir, recursive = TRUE), add = TRUE)

  project <- qswat_setup(
    project_dir = proj_dir,
    dem_file = dem,
    landuse_file = landuse,
    soil_file = soil,
    landuse_lookup = lu_lookup,
    soil_lookup = soil_lookup
  )

  project <- qswat_delineate(project, threshold = 500, quiet = FALSE)

  expect_false(is.null(project$fel_file))
  expect_true(file.exists(project$fel_file))
  expect_false(is.null(project$p_file))
  expect_true(file.exists(project$p_file))
  expect_false(is.null(project$ad8_file))
  expect_true(file.exists(project$ad8_file))
  expect_false(is.null(project$stream_file))
  expect_true(file.exists(project$stream_file))
  expect_false(is.null(project$watershed_file))
  expect_true(file.exists(project$watershed_file))
  expect_equal(project$stream_threshold, 500)
})
