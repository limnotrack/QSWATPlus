test_that("qswat_plot_landuse_summary works with mock data", {
  skip_if_not_installed("ggplot2")

  project <- structure(list(
    hru_data = data.frame(
      hru_id = 1:6,
      subbasin = c(1L, 1L, 1L, 2L, 2L, 2L),
      landuse = c("AGRL", "FRSD", "PAST", "AGRL", "PAST", "WATR"),
      soil = c("TX047", "TX047", "TX236", "TX047", "TX236", "TX236"),
      slope_class = c(1L, 1L, 1L, 2L, 2L, 2L),
      cell_count = c(100L, 80L, 50L, 120L, 90L, 30L),
      area_ha = c(10.0, 8.0, 5.0, 12.0, 9.0, 3.0),
      mean_elevation = c(500, 510, 520, 600, 610, 590),
      mean_slope = c(3.0, 4.0, 5.0, 8.0, 9.0, 7.0),
      stringsAsFactors = FALSE
    ),
    slope_classes = qswat_create_slope_classes(c(0, 5, 15, 9999))
  ), class = "qswat_project")

  # Test percent plot
  p <- qswat_plot_landuse_summary(project, type = "percent")
  expect_s3_class(p, "ggplot")

  # Test area plot
  p2 <- qswat_plot_landuse_summary(project, type = "area")
  expect_s3_class(p2, "ggplot")
})

test_that("qswat_plot_soil_summary works with mock data", {
  skip_if_not_installed("ggplot2")

  project <- structure(list(
    hru_data = data.frame(
      hru_id = 1:4,
      subbasin = c(1L, 1L, 2L, 2L),
      landuse = c("AGRL", "FRSD", "PAST", "AGRL"),
      soil = c("TX047", "TX236", "TX047", "TX619"),
      slope_class = c(1L, 1L, 2L, 2L),
      cell_count = c(100L, 50L, 200L, 150L),
      area_ha = c(10.0, 5.0, 20.0, 15.0),
      mean_elevation = c(500, 510, 600, 610),
      mean_slope = c(3.0, 4.0, 8.0, 9.0),
      stringsAsFactors = FALSE
    ),
    slope_classes = qswat_create_slope_classes(c(0, 5, 15, 9999))
  ), class = "qswat_project")

  p <- qswat_plot_soil_summary(project)
  expect_s3_class(p, "ggplot")
})

test_that("qswat_plot_hru_summary works with mock data", {
  skip_if_not_installed("ggplot2")

  project <- structure(list(
    hru_data = data.frame(
      hru_id = 1:6,
      subbasin = c(1L, 1L, 1L, 2L, 2L, 2L),
      landuse = c("AGRL", "FRSD", "PAST", "AGRL", "PAST", "WATR"),
      soil = c("TX047", "TX047", "TX236", "TX047", "TX236", "TX236"),
      slope_class = c(1L, 1L, 2L, 1L, 2L, 2L),
      cell_count = c(100L, 80L, 50L, 120L, 90L, 30L),
      area_ha = c(10.0, 8.0, 5.0, 12.0, 9.0, 3.0),
      mean_elevation = c(500, 510, 520, 600, 610, 590),
      mean_slope = c(3.0, 4.0, 5.0, 8.0, 9.0, 7.0),
      stringsAsFactors = FALSE
    ),
    slope_classes = qswat_create_slope_classes(c(0, 5, 15, 9999))
  ), class = "qswat_project")

  # Faceted summary
  p <- qswat_plot_hru_summary(project)
  expect_s3_class(p, "ggplot")

  # By-subbasin stacked bar
  p2 <- qswat_plot_hru_summary(project, by_subbasin = TRUE)
  expect_s3_class(p2, "ggplot")
})

test_that("summary plots validate inputs", {
  skip_if_not_installed("ggplot2")

  expect_error(qswat_plot_landuse_summary(list()), "qswat_project")
  expect_error(qswat_plot_soil_summary(list()), "qswat_project")
  expect_error(qswat_plot_hru_summary(list()), "qswat_project")

  empty_proj <- structure(
    list(hru_data = NULL),
    class = "qswat_project"
  )
  expect_error(qswat_plot_landuse_summary(empty_proj), "HRU data")
  expect_error(qswat_plot_soil_summary(empty_proj), "HRU data")
  expect_error(qswat_plot_hru_summary(empty_proj), "HRU data")
})

test_that("tmap plot functions require tmap", {
  
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
  project <- qswat_delineate(project, threshold = 500, quiet = TRUE)
  project <- qswat_create_streams(project)
  

  if (!requireNamespace("tmap", quietly = TRUE)) {
    expect_error(qswat_plot_dem(project), "tmap")
    expect_error(qswat_plot_landuse(project), "tmap")
    expect_error(qswat_plot_soil(project), "tmap")
  } else {
    # If tmap IS installed, just check they return tmap objects
    p <- qswat_plot_dem(project)
    expect_true(inherits(p, "tmap"))
    p2 <- qswat_plot_landuse(project)
    expect_true(inherits(p2, "tmap"))
    p3 <- qswat_plot_soil(project)
    expect_true(inherits(p3, "tmap"))
    p4 <- qswat_plot_streams(project)
    expect_true(inherits(p4, "tmap"))
    p5 <- qswat_plot_watershed(project)
    expect_true(inherits(p5, "tmap"))
  }
})

test_that("stream/watershed plots validate state", {
  project <- structure(list(
    streams_sf = NULL,
    watershed_file = NULL,
    dem_file = system.file("extdata", "ravn_dem.tif",
                           package = "rQSWATPlus")
  ), class = "qswat_project")

  skip_if_not_installed("tmap")
  expect_error(qswat_plot_streams(project), "qswat_create_streams")
  expect_error(qswat_plot_watershed(project), "qswat_delineate")
})
