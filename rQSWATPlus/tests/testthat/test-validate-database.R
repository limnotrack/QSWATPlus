test_that("qswat_check_database validates missing file", {
  expect_error(
    qswat_check_database("/nonexistent/path.sqlite"),
    "not found"
  )
})

test_that("qswat_check_database detects missing tables", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  # Only create gis_subbasins - missing others
  DBI::dbExecute(con, "CREATE TABLE gis_subbasins (id INTEGER PRIMARY KEY)")
  DBI::dbDisconnect(con)

  result <- qswat_check_database(db_file, verbose = FALSE)
  expect_false(result$passed)
  expect_s3_class(result, "qswat_db_check")
  expect_true(any(grepl("missing", result$errors)))
})

test_that("qswat_check_database detects empty tables", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  # Create all required tables but leave them empty
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  rQSWATPlus:::.create_db_tables(con)
  DBI::dbDisconnect(con)

  result <- qswat_check_database(db_file, verbose = FALSE)
  expect_false(result$passed)
  expect_true(any(grepl("empty", result$errors)))
})

test_that("qswat_check_database passes on valid database", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  # Build a valid project and write the database
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

  qswat_write_database(project, db_file = db_file, overwrite = TRUE)

  result <- qswat_check_database(db_file, verbose = FALSE)
  expect_true(result$passed)
  expect_equal(sum(result$checks$status == "FAIL"), 0)
})

test_that("qswat_check_database detects referential integrity issues", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  rQSWATPlus:::.create_db_tables(con)

  # Write a subbasin
  DBI::dbExecute(con, "INSERT INTO gis_subbasins VALUES (1,100,5,100,5,0,0,500,400,600,0)")
  # Write an HRU referencing non-existent subbasin 99
  DBI::dbExecute(con, "INSERT INTO gis_hrus VALUES (1,99,'AGRL','TX047',1,10,3,0,0,500,50,50,50,100)")
  # Write a channel for subbasin 1
  DBI::dbExecute(con, "INSERT INTO gis_channels VALUES (1,1,1,1,1000,0.01,1.0,0.5,0,0,0,0)")
  # Write an LSU
  DBI::dbExecute(con, "INSERT INTO gis_lsus VALUES (1,0,1,1,100,5,100,5,1.0,0.5,0,0,500)")
  # Write routing with outlet
  DBI::dbExecute(con, "INSERT INTO gis_routing VALUES (1,'sub','tot',0,'outlet',100)")
  DBI::dbDisconnect(con)

  result <- qswat_check_database(db_file, verbose = FALSE)
  expect_false(result$passed)
  expect_true(any(grepl("non-existent LSU", result$errors)))
})

test_that("qswat_check_database detects routing loops", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  rQSWATPlus:::.create_db_tables(con)

  # Write subbasins
  DBI::dbExecute(con, "INSERT INTO gis_subbasins VALUES (1,100,5,100,5,0,0,500,400,600,0)")
  DBI::dbExecute(con, "INSERT INTO gis_subbasins VALUES (2,100,5,100,5,0,0,500,400,600,0)")
  # HRUs
  DBI::dbExecute(con, "INSERT INTO gis_hrus VALUES (1,1,'AGRL','TX047',1,10,3,0,0,500,50,50,50,100)")
  DBI::dbExecute(con, "INSERT INTO gis_hrus VALUES (2,2,'AGRL','TX047',1,10,3,0,0,500,50,50,50,100)")
  # Channels
  DBI::dbExecute(con, "INSERT INTO gis_channels VALUES (1,1,1,1,1000,0.01,1.0,0.5,0,0,0,0)")
  DBI::dbExecute(con, "INSERT INTO gis_channels VALUES (2,2,1,1,1000,0.01,1.0,0.5,0,0,0,0)")
  # LSUs
  DBI::dbExecute(con, "INSERT INTO gis_lsus VALUES (1,0,1,1,100,5,100,5,1.0,0.5,0,0,500)")
  DBI::dbExecute(con, "INSERT INTO gis_lsus VALUES (2,0,2,2,100,5,100,5,1.0,0.5,0,0,500)")
  # Create routing loop: 1→2→1
  DBI::dbExecute(con, "INSERT INTO gis_routing VALUES (1,'sub','tot',2,'sub',100)")
  DBI::dbExecute(con, "INSERT INTO gis_routing VALUES (2,'sub','tot',1,'sub',100)")
  DBI::dbDisconnect(con)

  result <- qswat_check_database(db_file, verbose = FALSE)
  expect_false(result$passed)
  expect_true(any(grepl("[Ll]oop", result$errors)))
})

test_that("qswat_check_database detects missing outlet", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  rQSWATPlus:::.create_db_tables(con)

  DBI::dbExecute(con, "INSERT INTO gis_subbasins VALUES (1,100,5,100,5,0,0,500,400,600,0)")
  DBI::dbExecute(con, "INSERT INTO gis_hrus VALUES (1,1,'AGRL','TX047',1,10,3,0,0,500,50,50,50,100)")
  DBI::dbExecute(con, "INSERT INTO gis_channels VALUES (1,1,1,1,1000,0.01,1.0,0.5,0,0,0,0)")
  DBI::dbExecute(con, "INSERT INTO gis_lsus VALUES (1,0,1,1,100,5,100,5,1.0,0.5,0,0,500)")
  # Routing that doesn't go to outlet - just sub→sub
  DBI::dbExecute(con, "INSERT INTO gis_routing VALUES (1,'sub','tot',2,'sub',100)")
  DBI::dbDisconnect(con)

  result <- qswat_check_database(db_file, verbose = FALSE)
  expect_false(result$passed)
  expect_true(any(grepl("[Oo]utlet", result$errors)))
})

test_that("qswat_check_database detects bad routing percentages", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  rQSWATPlus:::.create_db_tables(con)

  DBI::dbExecute(con, "INSERT INTO gis_subbasins VALUES (1,100,5,100,5,0,0,500,400,600,0)")
  DBI::dbExecute(con, "INSERT INTO gis_hrus VALUES (1,1,'AGRL','TX047',1,10,3,0,0,500,50,50,50,100)")
  DBI::dbExecute(con, "INSERT INTO gis_channels VALUES (1,1,1,1,1000,0.01,1.0,0.5,0,0,0,0)")
  DBI::dbExecute(con, "INSERT INTO gis_lsus VALUES (1,0,1,1,100,5,100,5,1.0,0.5,0,0,500)")
  # Routing with bad percentage (50 instead of 100)
  DBI::dbExecute(con, "INSERT INTO gis_routing VALUES (1,'sub','tot',0,'outlet',50)")
  DBI::dbDisconnect(con)

  result <- qswat_check_database(db_file, verbose = FALSE)
  expect_false(result$passed)
  expect_true(any(grepl("100%", result$errors)))
})

test_that("print.qswat_db_check works", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  # Valid database
  project <- structure(list(
    project_dir = tempdir(),
    hru_data = data.frame(
      hru_id = 1:2,
      subbasin = c(1L, 2L),
      landuse = c("AGRL", "FRSD"),
      soil = c("TX047", "TX236"),
      slope_class = c(1L, 1L),
      cell_count = c(100L, 200L),
      area_ha = c(10.0, 20.0),
      mean_elevation = c(500, 600),
      mean_slope = c(3.0, 8.0),
      stringsAsFactors = FALSE
    ),
    basin_data = data.frame(
      subbasin = c(1L, 2L),
      area_ha = c(10.0, 20.0),
      mean_elevation = c(500, 600),
      min_elevation = c(490, 580),
      max_elevation = c(510, 620),
      mean_slope = c(3.0, 8.0),
      n_hrus = c(1L, 1L),
      n_landuses = c(1L, 1L),
      n_soils = c(1L, 1L),
      stringsAsFactors = FALSE
    ),
    slope_classes = qswat_create_slope_classes(),
    stream_topology = data.frame(
      LINKNO = c(1L, 2L),
      DSLINKNO = c(-1L, 1L),
      WSNO = c(1L, 2L),
      strmOrder = c(2L, 1L),
      Length = c(1000, 500),
      stringsAsFactors = FALSE
    )
  ), class = "qswat_project")

  qswat_write_database(project, db_file = db_file, overwrite = TRUE)
  result <- qswat_check_database(db_file, verbose = FALSE)

  output <- capture.output(print(result))
  expect_true(any(grepl("Compatibility Check", output)))
  expect_true(any(grepl("compatible", output)))
})
