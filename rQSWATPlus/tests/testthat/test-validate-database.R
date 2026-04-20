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

test_that("qswat_check_database warns about missing HAWQS and reference tables", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  # Build a valid project database (no HAWQS extras)
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  project <- structure(list(
    project_dir = tempdir(),
    hru_data = data.frame(
      hru_id = 1:2, subbasin = c(1L, 2L),
      landuse = c("AGRL", "FRSD"), soil = c("TX047", "TX236"),
      slope_class = c(1L, 1L), cell_count = c(100L, 200L),
      area_ha = c(10.0, 20.0), mean_elevation = c(500, 600),
      mean_slope = c(3.0, 8.0), stringsAsFactors = FALSE
    ),
    basin_data = data.frame(
      subbasin = c(1L, 2L), area_ha = c(10.0, 20.0),
      mean_elevation = c(500, 600), min_elevation = c(490, 580),
      max_elevation = c(510, 620), mean_slope = c(3.0, 8.0),
      n_hrus = c(1L, 1L), n_landuses = c(1L, 1L), n_soils = c(1L, 1L),
      stringsAsFactors = FALSE
    ),
    slope_classes = qswat_create_slope_classes(),
    stream_topology = data.frame(
      LINKNO = c(1L, 2L), DSLINKNO = c(-1L, 1L),
      WSNO = c(1L, 2L), strmOrder = c(2L, 1L),
      Length = c(1000, 500), stringsAsFactors = FALSE
    )
  ), class = "qswat_project")

  qswat_write_database(project, db_file = db_file, overwrite = TRUE)
  result <- qswat_check_database(db_file, verbose = FALSE)

  # Core checks should pass (no fatal errors)
  expect_true(result$passed,
              info = paste("Errors:", paste(result$errors, collapse = "; ")))

  # With the bundled HAWQS databases present, HAWQS tables should now
  # be populated by populate_from_datasets(), so they should NOT appear
  # as missing in warnings.
  hawqs_missing_warns <- result$warnings[
    grepl("HAWQS-specific", result$warnings)]

  # If the bundled databases are installed (package installed), no
  # HAWQS warning; otherwise one consolidated warning is acceptable.
  expect_true(length(hawqs_missing_warns) <= 1,
              info = paste("Unexpected HAWQS warnings:",
                           paste(hawqs_missing_warns, collapse = "; ")))
})

test_that("qswat_check_database warns when reference table exists but is empty", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  rQSWATPlus:::.create_db_tables(con)

  # Add minimal data to pass required-table checks
  DBI::dbExecute(con, "INSERT INTO gis_subbasins VALUES (1,100,5,100,5,0,0,500,400,600,0)")
  DBI::dbExecute(con, "INSERT INTO gis_hrus VALUES (1,1,'AGRL','TX047',1,10,3,0,0,500,50,50,50,100)")
  DBI::dbExecute(con, "INSERT INTO gis_channels VALUES (1,1,1,1,1000,0.01,1.0,0.5,0,0,0,0)")
  DBI::dbExecute(con, "INSERT INTO gis_lsus VALUES (1,0,1,1,100,5,100,5,1.0,0.5,0,0,500)")
  DBI::dbExecute(con, "INSERT INTO gis_routing VALUES (1,'sub','tot',0,'outlet',100)")
  DBI::dbExecute(con, "INSERT INTO project_config (id, project_name, delineation_done, hrus_done, use_gwflow) VALUES (1,'test',1,1,0)")

  # Add an empty plants_plt table (exists but has no data)
  tryCatch(DBI::dbExecute(con, "DROP TABLE IF EXISTS plants_plt"), error = function(e) NULL)
  DBI::dbExecute(con, "CREATE TABLE plants_plt (id INTEGER PRIMARY KEY, name TEXT)")
  DBI::dbDisconnect(con)

  result <- qswat_check_database(db_file, verbose = FALSE)

  # Should warn about the empty reference table
  ref_warns <- result$warnings[grepl("plants_plt.*no data", result$warnings)]
  expect_true(length(ref_warns) >= 1,
              info = paste("Expected warning about empty plants_plt.",
                           "Got warnings:", paste(result$warnings, collapse = "; ")))
})

# ---- gwflow validation tests ----

# Helper to build a minimal valid database with optional gwflow setup
.make_minimal_db <- function(db_file, use_gwflow = 0L) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  rQSWATPlus:::.create_db_tables(con)
  DBI::dbExecute(con, "INSERT INTO gis_subbasins VALUES (1,100,5,100,5,0,0,500,400,600,0)")
  DBI::dbExecute(con, "INSERT INTO gis_hrus VALUES (1,1,'AGRL','TX047',1,10,3,0,0,500,50,50,50,100)")
  DBI::dbExecute(con, "INSERT INTO gis_channels VALUES (1,1,1,1,1000,0.01,1.0,0.5,0,0,0,0)")
  DBI::dbExecute(con, "INSERT INTO gis_lsus VALUES (1,0,1,1,100,5,100,5,1.0,0.5,0,0,500)")
  DBI::dbExecute(con, "INSERT INTO gis_routing VALUES (1,'sub','tot',0,'outlet',100)")
  DBI::dbExecute(con,
    paste0("INSERT INTO project_config (id, project_name, delineation_done, ",
           "hrus_done, use_gwflow) VALUES (1,'test',1,1,", use_gwflow, ")"))
  con
}

test_that("qswat_check_database skips gwflow checks when use_gwflow=0", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  con <- .make_minimal_db(db_file, use_gwflow = 0L)
  DBI::dbDisconnect(con)

  result <- qswat_check_database(db_file, verbose = FALSE)

  # No gwflow checks should be present
  gwflow_checks <- result$checks[grepl("^gwflow:", result$checks$check), ]
  expect_equal(nrow(gwflow_checks), 0,
               label = "no gwflow check rows when use_gwflow=0")
  expect_true(result$passed,
              info = paste("Errors:", paste(result$errors, collapse = "; ")))
})

test_that("qswat_check_database fails when gwflow enabled but tables missing", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  # Enable gwflow but do NOT create the gwflow tables
  con <- .make_minimal_db(db_file, use_gwflow = 1L)
  DBI::dbDisconnect(con)

  result <- qswat_check_database(db_file, verbose = FALSE)

  expect_false(result$passed)
  expect_true(any(grepl("gwflow is enabled but", result$errors)),
              info = paste("Expected missing-tables error. Got:",
                           paste(result$errors, collapse = "; ")))
})

test_that("qswat_check_database fails when gwflow core tables are empty", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  # Create all gwflow tables but leave them empty
  con <- .make_minimal_db(db_file, use_gwflow = 1L)
  rQSWATPlus:::.create_gwflow_tables(con)
  DBI::dbDisconnect(con)

  result <- qswat_check_database(db_file, verbose = FALSE)

  expect_false(result$passed)
  gwflow_fails <- result$errors[grepl("^gwflow", result$checks$check[
    result$checks$status == "FAIL"])]
  # At minimum gwflow_base, gwflow_zone, gwflow_grid, gwflow_solutes,
  # gwflow_rivcell should all fail the non-empty check
  fail_checks <- result$checks$check[result$checks$status == "FAIL"]
  expect_true(any(grepl("gwflow:", fail_checks)),
              info = paste("Expected gwflow check failures. Failing checks:",
                           paste(fail_checks, collapse = ", ")))
})

test_that("qswat_check_database passes gwflow checks with fully populated tables (LSU recharge)", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  con <- .make_minimal_db(db_file, use_gwflow = 1L)
  rQSWATPlus:::.create_gwflow_tables(con)

  # Populate gwflow_zone (1 zone)
  DBI::dbExecute(con,
    "INSERT INTO gwflow_zone VALUES (1, 10.0, 0.2, 0.005, 0.5)")

  # Populate gwflow_grid (1 active cell)
  DBI::dbExecute(con,
    "INSERT INTO gwflow_grid VALUES (1, 1, 1, 500.0, 20.0, 1.0, 495.0, 0)")

  # Populate gwflow_base (recharge=2 → LSU recharge)
  DBI::dbExecute(con, paste0(
    "INSERT INTO gwflow_base VALUES (",
    "200,1,1,1,2,1,1,0,0,1,1,1,0,1,1,5.0,0,1.0,5.0,5.0,1.22,50.0,5.0,",
    "0,2.0,9.99e-6,0.25,1,1,1,0,0,1.0)"))

  # Populate gwflow_solutes (10 default solutes)
  solutes <- list(
    c("no3-n", 1, -0.0001, 3,    "single", 3.0),
    c("p",     1, 0,       0.05, "single", 0.05),
    c("so4",   1, 0,       0,    "single", 100),
    c("ca",    1, 0,       0,    "single", 50),
    c("mg",    1, 0,       0,    "single", 30),
    c("na",    1, 0,       0,    "single", 40),
    c("k",     1, 0,       0,    "single", 1),
    c("cl",    1, 0,       0,    "single", 25),
    c("co3",   1, 0,       0,    "single", 1),
    c("hco3",  1, 0,       0,    "single", 80)
  )
  for (s in solutes) {
    DBI::dbExecute(con, sprintf(
      "INSERT INTO gwflow_solutes VALUES ('%s',%s,%s,%s,'%s',%s)",
      s[[1]], s[[2]], s[[3]], s[[4]], s[[5]], s[[6]]))
  }

  # Populate gwflow_rivcell (1 river-cell connection)
  DBI::dbExecute(con, "INSERT INTO gwflow_rivcell VALUES (1, 1, 150.0)")

  # Populate gwflow_lsucell (LSU recharge mode)
  DBI::dbExecute(con, "INSERT INTO gwflow_lsucell VALUES (1, 1, 40000.0)")

  DBI::dbDisconnect(con)

  result <- qswat_check_database(db_file, verbose = FALSE)

  gwflow_fail_checks <- result$checks$check[
    grepl("^gwflow:", result$checks$check) &
      result$checks$status == "FAIL"]
  expect_equal(length(gwflow_fail_checks), 0L,
               label = paste("gwflow checks should all pass. Failed:",
                             paste(gwflow_fail_checks, collapse = ", ")))
})

test_that("qswat_check_database fails when gwflow_lsucell empty in LSU recharge mode", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  con <- .make_minimal_db(db_file, use_gwflow = 1L)
  rQSWATPlus:::.create_gwflow_tables(con)

  DBI::dbExecute(con,
    "INSERT INTO gwflow_zone VALUES (1, 10.0, 0.2, 0.005, 0.5)")
  DBI::dbExecute(con,
    "INSERT INTO gwflow_grid VALUES (1, 1, 1, 500.0, 20.0, 1.0, 495.0, 0)")
  # recharge=2 → LSU recharge
  DBI::dbExecute(con, paste0(
    "INSERT INTO gwflow_base VALUES (",
    "200,1,1,1,2,1,1,0,0,1,1,1,0,1,1,5.0,0,1.0,5.0,5.0,1.22,50.0,5.0,",
    "0,2.0,9.99e-6,0.25,1,1,1,0,0,1.0)"))
  for (s in list(
    c("no3-n","single"), c("p","single"), c("so4","single"),
    c("ca","single"),   c("mg","single"), c("na","single"),
    c("k","single"),    c("cl","single"), c("co3","single"),
    c("hco3","single")
  )) {
    DBI::dbExecute(con, sprintf(
      "INSERT INTO gwflow_solutes VALUES ('%s',1,0,0,'%s',1)",
      s[[1]], s[[2]]))
  }
  DBI::dbExecute(con, "INSERT INTO gwflow_rivcell VALUES (1, 1, 150.0)")
  # gwflow_lsucell intentionally left empty

  DBI::dbDisconnect(con)

  result <- qswat_check_database(db_file, verbose = FALSE)

  expect_false(result$passed)
  expect_true(any(grepl("gwflow_lsucell is empty", result$errors)),
              info = paste("Expected lsucell error. Got:",
                           paste(result$errors, collapse = "; ")))
})

test_that("qswat_check_database fails when gwflow_hrucell empty in HRU recharge mode", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  con <- .make_minimal_db(db_file, use_gwflow = 1L)
  rQSWATPlus:::.create_gwflow_tables(con)

  DBI::dbExecute(con,
    "INSERT INTO gwflow_zone VALUES (1, 10.0, 0.2, 0.005, 0.5)")
  DBI::dbExecute(con,
    "INSERT INTO gwflow_grid VALUES (1, 1, 1, 500.0, 20.0, 1.0, 495.0, 0)")
  # recharge=1 → HRU recharge
  DBI::dbExecute(con, paste0(
    "INSERT INTO gwflow_base VALUES (",
    "200,1,1,1,1,1,1,0,0,1,1,1,0,1,1,5.0,0,1.0,5.0,5.0,1.22,50.0,5.0,",
    "0,2.0,9.99e-6,0.25,1,1,1,0,0,1.0)"))
  for (s in list(
    c("no3-n","single"), c("p","single"), c("so4","single"),
    c("ca","single"),   c("mg","single"), c("na","single"),
    c("k","single"),    c("cl","single"), c("co3","single"),
    c("hco3","single")
  )) {
    DBI::dbExecute(con, sprintf(
      "INSERT INTO gwflow_solutes VALUES ('%s',1,0,0,'%s',1)",
      s[[1]], s[[2]]))
  }
  DBI::dbExecute(con, "INSERT INTO gwflow_rivcell VALUES (1, 1, 150.0)")
  # gwflow_hrucell intentionally left empty (gwflow_lsucell also empty - not
  # required for recharge=1)

  DBI::dbDisconnect(con)

  result <- qswat_check_database(db_file, verbose = FALSE)

  expect_false(result$passed)
  expect_true(any(grepl("gwflow_hrucell is empty", result$errors)),
              info = paste("Expected hrucell error. Got:",
                           paste(result$errors, collapse = "; ")))
})
