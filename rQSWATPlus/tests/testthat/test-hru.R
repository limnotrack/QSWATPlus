test_that("internal slope classification works", {
  slopes <- qswat_create_slope_classes(c(0, 5, 15, 9999))
  # Test classification
  classes <- rQSWATPlus:::.classify_slope(c(0, 2.5, 5, 10, 15, 50, NA), slopes)
  expect_equal(classes[1], 1L)  # 0% -> class 1
  expect_equal(classes[2], 1L)  # 2.5% -> class 1
  expect_equal(classes[3], 2L)  # 5% -> class 2
  expect_equal(classes[4], 2L)  # 10% -> class 2
  expect_equal(classes[5], 3L)  # 15% -> class 3
  expect_equal(classes[6], 3L)  # 50% -> class 3
  expect_true(is.na(classes[7]))  # NA -> NA
})

test_that("internal lookup mapping works", {
  values <- c(1, 2, 3, 4, 5, 99)
  lookup_vals <- c(1, 2, 3, 4, 5)
  lookup_names <- c("AGRL", "FRSD", "PAST", "WATR", "URBN")

  result <- rQSWATPlus:::.map_lookup(values, lookup_vals, lookup_names)
  expect_equal(result[1], "AGRL")
  expect_equal(result[5], "URBN")
  expect_true(is.na(result[6]))  # 99 not in lookup
})

test_that("internal cell area calculation works", {
  skip_if_not_installed("terra")

  # Create a small test raster
  r <- terra::rast(nrows = 10, ncols = 10,
                   xmin = 0, xmax = 1000, ymin = 0, ymax = 1000,
                   crs = "EPSG:32632")  # UTM zone 32N (meters)
  area <- rQSWATPlus:::.compute_cell_area(r, "meters")
  # Each cell is 100m x 100m = 10000 m2 = 1 ha
  expect_equal(area, 1.0)
})

test_that("qswat_create_hrus validates input", {
  expect_error(
    qswat_create_hrus(list(), data.frame(), data.frame()),
    "qswat_project"
  )
})

# Integration test for HRU creation (requires TauDEM for delineation)
test_that("HRU creation works with delineated project", {
  skip_if_not_installed("traudem")
  taudem_ok <- tryCatch({
    traudem::taudem_sitrep()
    TRUE
  }, error = function(e) FALSE)
  skip_if(!taudem_ok, "TauDEM not available")

  dem <- system.file("extdata", "ravn_dem.tif", package = "rQSWATPlus")
  landuse <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
  soil <- system.file("extdata", "ravn_soil.tif", package = "rQSWATPlus")
  lu_lookup_file <- system.file("extdata", "ravn_landuse.csv",
                                 package = "rQSWATPlus")
  soil_lookup_file <- system.file("extdata", "ravn_soil.csv",
                                   package = "rQSWATPlus")

  skip_if(dem == "", message = "Example data not available")

  proj_dir <- file.path(tempdir(), "test_hru")
  on.exit(unlink(proj_dir, recursive = TRUE), add = TRUE)

  project <- qswat_setup(
    project_dir = proj_dir,
    dem_file = dem,
    landuse_file = landuse,
    soil_file = soil,
    landuse_lookup = lu_lookup_file,
    soil_lookup = soil_lookup_file
  )

  project <- qswat_delineate(project, threshold = 500, quiet = TRUE)
  project <- qswat_create_streams(project)

  lu <- qswat_read_landuse_lookup(lu_lookup_file)
  soil_lkp <- qswat_read_soil_lookup(soil_lookup_file)
  slopes <- qswat_create_slope_classes(c(0, 5, 15, 9999))

  project <- qswat_create_hrus(project, lu, soil_lkp, slopes)
  
  # Write the project database
  db_path <- qswat_write_database(project, overwrite = TRUE)

  expect_false(is.null(project$hru_data))
  expect_true(nrow(project$hru_data) > 0)
  expect_true("subbasin" %in% names(project$hru_data))
  expect_true("landuse" %in% names(project$hru_data))
  expect_true("soil" %in% names(project$hru_data))
  expect_true("area_ha" %in% names(project$hru_data))

  expect_false(is.null(project$basin_data))
  expect_true(nrow(project$basin_data) > 0)
})
