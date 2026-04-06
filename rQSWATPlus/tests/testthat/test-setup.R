test_that("qswat_setup creates project structure", {
  # Get example data paths
  dem <- system.file("extdata", "ravn_dem.tif", package = "rQSWATPlus")
  landuse <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
  soil <- system.file("extdata", "ravn_soil.tif", package = "rQSWATPlus")
  lu_lookup <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
  soil_lookup <- system.file("extdata", "ravn_soil.csv", package = "rQSWATPlus")

  skip_if(dem == "", message = "Example data not available")

  proj_dir <- file.path(tempdir(), "test_setup")
  on.exit(unlink(proj_dir, recursive = TRUE), add = TRUE)

  project <- qswat_setup(
    project_dir = proj_dir,
    dem_file = dem,
    landuse_file = landuse,
    soil_file = soil,
    landuse_lookup = lu_lookup,
    soil_lookup = soil_lookup
  )

  expect_s3_class(project, "qswat_project")
  expect_true(dir.exists(proj_dir))
  expect_true(dir.exists(file.path(proj_dir, "Source")))
  expect_true(dir.exists(file.path(proj_dir, "Watershed")))
  expect_true(dir.exists(file.path(proj_dir, "Watershed", "Rasters")))
  expect_true(dir.exists(file.path(proj_dir, "Watershed", "Shapes")))
  expect_true(dir.exists(file.path(proj_dir, "Watershed", "Text")))
  expect_true(file.exists(project$dem_file))
  expect_true(file.exists(project$landuse_file))
  expect_true(file.exists(project$soil_file))
  expect_true(project$nrow > 0)
  expect_true(project$ncol > 0)
})

test_that("qswat_setup validates missing files", {
  expect_error(
    qswat_setup(
      project_dir = tempdir(),
      dem_file = "nonexistent.tif",
      landuse_file = "nonexistent.tif",
      soil_file = "nonexistent.tif",
      landuse_lookup = "nonexistent.csv",
      soil_lookup = "nonexistent.csv"
    ),
    "File not found"
  )
})

test_that("qswat_setup with outlet file", {
  dem <- system.file("extdata", "ravn_dem.tif", package = "rQSWATPlus")
  landuse <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
  soil <- system.file("extdata", "ravn_soil.tif", package = "rQSWATPlus")
  lu_lookup <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
  soil_lookup <- system.file("extdata", "ravn_soil.csv", package = "rQSWATPlus")
  outlet <- system.file("extdata", "ravn_outlet.shp", package = "rQSWATPlus")

  skip_if(dem == "", message = "Example data not available")
  skip_if(outlet == "", message = "Outlet data not available")

  proj_dir <- file.path(tempdir(), "test_setup_outlet")
  on.exit(unlink(proj_dir, recursive = TRUE), add = TRUE)

  project <- qswat_setup(
    project_dir = proj_dir,
    dem_file = dem,
    landuse_file = landuse,
    soil_file = soil,
    landuse_lookup = lu_lookup,
    soil_lookup = soil_lookup,
    outlet_file = outlet
  )

  expect_s3_class(project, "qswat_project")
  expect_false(is.null(project$outlet_file))
  expect_true(file.exists(project$outlet_file))
})

test_that("print.qswat_project works", {
  dem <- system.file("extdata", "ravn_dem.tif", package = "rQSWATPlus")
  landuse <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
  soil <- system.file("extdata", "ravn_soil.tif", package = "rQSWATPlus")
  lu_lookup <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
  soil_lookup <- system.file("extdata", "ravn_soil.csv", package = "rQSWATPlus")

  skip_if(dem == "", message = "Example data not available")

  proj_dir <- file.path(tempdir(), "test_print")
  on.exit(unlink(proj_dir, recursive = TRUE), add = TRUE)

  project <- qswat_setup(
    project_dir = proj_dir,
    dem_file = dem,
    landuse_file = landuse,
    soil_file = soil,
    landuse_lookup = lu_lookup,
    soil_lookup = soil_lookup
  )

  output <- capture.output(print(project))
  expect_true(any(grepl("QSWATPlus Project", output)))
  expect_true(any(grepl("DEM:", output)))
})

test_that("qswat_setup stores usersoil in project object", {
  proj_dir <- file.path(tempdir(), "test_setup_usersoil")
  on.exit(unlink(proj_dir, recursive = TRUE), add = TRUE)

  # usersoil = NULL (default)
  p_null <- qswat_setup(project_dir = proj_dir)
  expect_null(p_null$usersoil)

  # usersoil = "FAO_usersoil"
  p_fao <- qswat_setup(project_dir = proj_dir, overwrite = TRUE,
                        usersoil = "FAO_usersoil")
  expect_equal(p_fao$usersoil, "FAO_usersoil")

  # usersoil = "global_usersoil"
  p_global <- qswat_setup(project_dir = proj_dir, overwrite = TRUE,
                           usersoil = "global_usersoil")
  expect_equal(p_global$usersoil, "global_usersoil")

  # usersoil = arbitrary CSV path string (stored as-is, validated later)
  p_csv <- qswat_setup(project_dir = proj_dir, overwrite = TRUE,
                        usersoil = "/path/to/my_soils.csv")
  expect_equal(p_csv$usersoil, "/path/to/my_soils.csv")
})
