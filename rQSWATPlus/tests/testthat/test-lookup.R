test_that("qswat_read_landuse_lookup reads CSV correctly", {
  lu_file <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
  skip_if(lu_file == "", message = "Example data not available")

  lu <- qswat_read_landuse_lookup(lu_file)

  expect_s3_class(lu, "data.frame")
  expect_true("value" %in% names(lu))
  expect_true("landuse" %in% names(lu))
  expect_true(nrow(lu) > 0)
  expect_true(is.integer(lu$value))
  expect_true(is.character(lu$landuse))
  # Check some expected values
  expect_true("PAST" %in% lu$landuse)
})

test_that("qswat_read_soil_lookup reads CSV correctly", {
  soil_file <- system.file("extdata", "ravn_soil.csv", package = "rQSWATPlus")
  skip_if(soil_file == "", message = "Example data not available")

  soil <- qswat_read_soil_lookup(soil_file)

  expect_s3_class(soil, "data.frame")
  expect_true("value" %in% names(soil))
  expect_true("soil" %in% names(soil))
  expect_true(nrow(soil) > 0)
  expect_true(is.integer(soil$value))
  expect_true(is.character(soil$soil))
})

test_that("qswat_read_landuse_lookup validates missing file", {
  expect_error(
    qswat_read_landuse_lookup("nonexistent.csv"),
    "not found"
  )
})

test_that("qswat_read_soil_lookup validates missing file", {
  expect_error(
    qswat_read_soil_lookup("nonexistent.csv"),
    "not found"
  )
})

test_that("qswat_create_slope_classes creates valid classes", {
  # Single class
  sc <- qswat_create_slope_classes(c(0, 9999))
  expect_equal(nrow(sc), 1)
  expect_equal(sc$min_slope, 0)
  expect_equal(sc$class_id, 1)

  # Multiple classes
  sc <- qswat_create_slope_classes(c(0, 5, 15, 9999))
  expect_equal(nrow(sc), 3)
  expect_equal(sc$class_id, c(1, 2, 3))
  expect_equal(sc$min_slope, c(0, 5, 15))
  expect_equal(sc$max_slope, c(5, 15, 9999))

  # Custom labels
  sc <- qswat_create_slope_classes(c(0, 10, 9999),
                                    labels = c("gentle", "steep"))
  expect_equal(sc$label, c("gentle", "steep"))
})

test_that("qswat_create_slope_classes validates input", {
  expect_error(qswat_create_slope_classes(c(5)), "At least two")
  expect_error(qswat_create_slope_classes(c(10, 5)), "strictly increasing")
  expect_error(qswat_create_slope_classes(c(0, 9999), labels = c("a", "b")),
               "Number of labels")
})

test_that("qswat_read_usersoil reads valid CSV and normalises column names", {
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file), add = TRUE)

  utils::write.csv(data.frame(
    snam    = c("MySoil1", "MySoil2"),
    nlayers = c(2L, 3L),
    hydgrp  = c("B", "C"),
    sol_zmx = c(1000, 1500)
  ), csv_file, row.names = FALSE)

  result <- qswat_read_usersoil(csv_file)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  # Column names should be uppercase
  expect_true("SNAM" %in% names(result))
  expect_true("NLAYERS" %in% names(result))
  expect_true("HYDGRP" %in% names(result))
  expect_equal(result$SNAM, c("MySoil1", "MySoil2"))
})

test_that("qswat_read_usersoil errors on missing file", {
  expect_error(qswat_read_usersoil("nonexistent_soils.csv"), "not found")
})

test_that("qswat_read_usersoil errors when SNAM column is absent", {
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file), add = TRUE)
  utils::write.csv(data.frame(SoilName = "X", SOL_Z1 = 300), csv_file,
                   row.names = FALSE)
  expect_error(qswat_read_usersoil(csv_file), "SNAM")
})

test_that("qswat_read_usersoil drops rows with empty SNAM", {
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file), add = TRUE)
  utils::write.csv(data.frame(
    SNAM    = c("SoilA", "", NA, "SoilB"),
    NLAYERS = c(2, 1, 1, 3)
  ), csv_file, row.names = FALSE)

  result <- qswat_read_usersoil(csv_file)
  expect_equal(nrow(result), 2L)
  expect_equal(result$SNAM, c("SoilA", "SoilB"))
})
