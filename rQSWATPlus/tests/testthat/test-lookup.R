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
