test_that("qswat_write_database validates input", {
  expect_error(
    qswat_write_database(list()),
    "qswat_project"
  )
  project <- structure(list(hru_data = NULL), class = "qswat_project")
  expect_error(
    qswat_write_database(project),
    "No HRU data"
  )
})

test_that("database table creation works", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  rQSWATPlus:::.create_db_tables(con)

  tables <- DBI::dbListTables(con)

  # GIS tables
  expect_true("gis_subbasins" %in% tables)
  expect_true("gis_hrus" %in% tables)
  expect_true("gis_routing" %in% tables)
  expect_true("gis_channels" %in% tables)
  expect_true("gis_lsus" %in% tables)
  expect_true("gis_aquifers" %in% tables)
  expect_true("gis_deep_aquifers" %in% tables)
  expect_true("gis_water" %in% tables)
  expect_true("gis_points" %in% tables)
  expect_true("gis_elevationbands" %in% tables)
  expect_true("gis_landexempt" %in% tables)
  expect_true("gis_splithrus" %in% tables)

  # project_config
  expect_true("project_config" %in% tables)

  # Configuration tables
  expect_true("config_delin" %in% tables)
  expect_true("config_hru" %in% tables)
  expect_true("config_landuse" %in% tables)
  expect_true("config_lsu" %in% tables)
  expect_true("config_observed" %in% tables)
  expect_true("config_params" %in% tables)
  expect_true("config_soil" %in% tables)

  # Intermediate data tables
  expect_true("BASINSDATA" %in% tables)
  expect_true("HRUSDATA" %in% tables)
  expect_true("LSUSDATA" %in% tables)
  expect_true("WATERDATA" %in% tables)

  # Reference / lookup tables
  expect_true("global_landuses" %in% tables)
  expect_true("global_soils" %in% tables)
  expect_true("global_usersoil" %in% tables)
  expect_true("plant" %in% tables)
  expect_true("urban" %in% tables)
  expect_true("WGEN_User" %in% tables)
  expect_true("WGEN_User_mon" %in% tables)
})

test_that("subbasin table write works", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  rQSWATPlus:::.create_db_tables(con)

  basin_data <- data.frame(
    subbasin = c(1L, 2L),
    area_ha = c(100.0, 200.0),
    mean_elevation = c(500.0, 600.0),
    min_elevation = c(400.0, 500.0),
    max_elevation = c(600.0, 700.0),
    mean_slope = c(5.0, 10.0),
    n_hrus = c(3L, 4L),
    n_landuses = c(2L, 3L),
    n_soils = c(2L, 2L)
  )

  rQSWATPlus:::.write_subbasin_table(con, basin_data)

  result <- DBI::dbGetQuery(con, "SELECT * FROM gis_subbasins")
  expect_equal(nrow(result), 2)
  expect_equal(result$id, c(1, 2))
  expect_equal(result$area, c(100, 200))
})

# Full database integration test
test_that("full database write works with mock data", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  # Create mock project
  project <- structure(list(
    project_dir = tempdir(),
    hru_data = data.frame(
      hru_id = 1:4,
      subbasin = c(1L, 1L, 2L, 2L),
      landuse = c("AGRL", "FRSD", "PAST", "AGRL"),
      soil = c("TX047", "TX047", "TX236", "TX236"),
      slope_class = c(1L, 1L, 2L, 2L),
      cell_count = c(100L, 50L, 200L, 150L),
      area_ha = c(10.0, 5.0, 20.0, 15.0),
      mean_elevation = c(500, 510, 600, 610),
      mean_slope = c(3.0, 4.0, 8.0, 9.0),
      stringsAsFactors = FALSE
    ),
    basin_data = data.frame(
      subbasin = c(1L, 2L),
      area_ha = c(15.0, 35.0),
      mean_elevation = c(505, 605),
      min_elevation = c(490, 580),
      max_elevation = c(520, 640),
      mean_slope = c(3.5, 8.5),
      n_hrus = c(2L, 2L),
      n_landuses = c(2L, 2L),
      n_soils = c(1L, 1L),
      stringsAsFactors = FALSE
    ),
    slope_classes = qswat_create_slope_classes(c(0, 5, 15, 9999)),
    stream_topology = data.frame(
      LINKNO = c(1L, 2L),
      DSLINKNO = c(-1L, 1L),
      WSNO = c(1L, 2L),
      strmOrder = c(2L, 1L),
      Length = c(1000, 500),
      stringsAsFactors = FALSE
    )
  ), class = "qswat_project")

  result <- qswat_write_database(project, db_file = db_file, overwrite = TRUE)
  expect_true(file.exists(db_file))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  subs <- DBI::dbGetQuery(con, "SELECT * FROM gis_subbasins")
  expect_equal(nrow(subs), 2)

  hrus <- DBI::dbGetQuery(con, "SELECT * FROM gis_hrus")
  expect_equal(nrow(hrus), 4)

  routing <- DBI::dbGetQuery(con, "SELECT * FROM gis_routing")
  expect_true(nrow(routing) > 0)

  channels <- DBI::dbGetQuery(con, "SELECT * FROM gis_channels")
  expect_true(nrow(channels) > 0)

  # Check project_config
  pc <- DBI::dbGetQuery(con, "SELECT * FROM project_config")
  expect_equal(nrow(pc), 1)
  expect_equal(pc$delineation_done, 1)
  expect_equal(pc$hrus_done, 1)

  # Check BASINSDATA
  bd <- DBI::dbGetQuery(con, "SELECT * FROM BASINSDATA")
  expect_equal(nrow(bd), 2)

  # Check HRUSDATA
  hd <- DBI::dbGetQuery(con, "SELECT * FROM HRUSDATA")
  expect_equal(nrow(hd), 4)

  # Check LSUSDATA
  ld <- DBI::dbGetQuery(con, "SELECT * FROM LSUSDATA")
  expect_equal(nrow(ld), 2)
})
