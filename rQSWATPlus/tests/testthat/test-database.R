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

  # Weather tables
  expect_true("weather_sta_cli" %in% tables)
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
  expect_equal(result$waterid, c(0L, 0L))
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
  expect_true("waterid" %in% names(subs))
  expect_equal(subs$waterid, c(0L, 0L))

  hrus <- DBI::dbGetQuery(con, "SELECT * FROM gis_hrus")
  expect_equal(nrow(hrus), 4)

  routing <- DBI::dbGetQuery(con, "SELECT * FROM gis_routing")
  expect_true(nrow(routing) > 0)
  expect_true("hyd_typ" %in% names(routing))
  expect_true(all(routing$hyd_typ == "tot"))

  channels <- DBI::dbGetQuery(con, "SELECT * FROM gis_channels")
  expect_true(nrow(channels) > 0)
  expect_true("strahler" %in% names(channels))
  expect_true("midlat" %in% names(channels))
  expect_true("midlon" %in% names(channels))
  expect_equal(channels$strahler, c(2L, 1L))

  # Check LSU table has new fields
  lsus <- DBI::dbGetQuery(con, "SELECT * FROM gis_lsus")
  expect_true(nrow(lsus) > 0)
  expect_true("subbasin" %in% names(lsus))
  expect_true("len1" %in% names(lsus))
  expect_equal(lsus$subbasin, c(1L, 2L))

  # Check project_config
  pc <- DBI::dbGetQuery(con, "SELECT * FROM project_config")
  expect_equal(nrow(pc), 1)
  expect_equal(pc$delineation_done, 1)
  expect_equal(pc$hrus_done, 1)
  expect_true("use_gwflow" %in% names(pc))
  expect_equal(pc$use_gwflow, 0L)

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

test_that("reference database is copied to project folder", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  project_dir <- tempfile("refdb_")
  dir.create(project_dir)
  on.exit(unlink(project_dir, recursive = TRUE), add = TRUE)

  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  project <- structure(list(
    project_dir = project_dir,
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

  result <- qswat_write_database(project, db_file = db_file, overwrite = TRUE)

  # Reference database should have been copied
  ref_db <- file.path(project_dir, "swatplus_datasets.sqlite")
  expect_true(file.exists(ref_db))

  # project_config.reference_db should point to the copy
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  pc <- DBI::dbGetQuery(con, "SELECT reference_db FROM project_config")
  expect_equal(pc$reference_db, normalizePath(ref_db, mustWork = FALSE))
})

test_that("second write does not re-copy reference database", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  project_dir <- tempfile("refdb2_")
  dir.create(project_dir)
  on.exit(unlink(project_dir, recursive = TRUE), add = TRUE)

  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  project <- structure(list(
    project_dir = project_dir,
    hru_data = data.frame(
      hru_id = 1L, subbasin = 1L, landuse = "AGRL", soil = "TX047",
      slope_class = 1L, cell_count = 100L, area_ha = 10.0,
      mean_elevation = 500, mean_slope = 3.0, stringsAsFactors = FALSE
    ),
    basin_data = data.frame(
      subbasin = 1L, area_ha = 10.0, mean_elevation = 500,
      min_elevation = 490, max_elevation = 510, mean_slope = 3.0,
      n_hrus = 1L, n_landuses = 1L, n_soils = 1L,
      stringsAsFactors = FALSE
    ),
    slope_classes = qswat_create_slope_classes(),
    stream_topology = data.frame(
      LINKNO = 1L, DSLINKNO = -1L, WSNO = 1L, strmOrder = 1L,
      Length = 500, stringsAsFactors = FALSE
    )
  ), class = "qswat_project")

  qswat_write_database(project, db_file = db_file, overwrite = TRUE)
  ref_db <- file.path(project_dir, "swatplus_datasets.sqlite")
  mtime1 <- file.info(ref_db)$mtime

  # Write again - should NOT overwrite the existing copy
  qswat_write_database(project, db_file = db_file, overwrite = TRUE)
  mtime2 <- file.info(ref_db)$mtime
  expect_equal(mtime1, mtime2)
})

# Regression test: TauDEM sometimes assigns WSNO=0 to the outlet reach when
# an outlet point is used.  That channel has no corresponding subbasin and
# must not end up in gis_channels (it would otherwise cause a referential
# integrity failure in qswat_check_database).  Upstream routing must be
# redirected directly to "outlet".
test_that("WSNO=0 outlet stream is excluded from channels and routing", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  project_dir <- tempfile("test_wsno0_outlet_")
  dir.create(project_dir)
  on.exit(unlink(project_dir, recursive = TRUE), add = TRUE)

  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  # Topology: two real subbasins (WSNO 1, 2) draining into an outlet reach
  # (WSNO=0) which flows to the watershed outlet.
  # Link 2 (WSNO=2) → Link 1 (WSNO=1) → Link 3 (WSNO=0, outlet reach) → out
  project <- structure(list(
    project_dir = project_dir,
    hru_data = data.frame(
      hru_id = c(1L, 2L),
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
      LINKNO    = c(1L, 2L, 3L),
      DSLINKNO  = c(3L, 1L, -1L),   # 1→3(WSNO=0), 2→1, 3 is outlet
      WSNO      = c(1L, 2L, 0L),    # WSNO=0 for the outlet reach
      strmOrder = c(2L, 1L, 3L),
      Length    = c(1000, 500, 200),
      stringsAsFactors = FALSE
    )
  ), class = "qswat_project")

  qswat_write_database(project, db_file = db_file, overwrite = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # gis_channels must not contain a row with subbasin = 0
  channels <- DBI::dbGetQuery(con, "SELECT * FROM gis_channels")
  expect_true(
    all(channels$subbasin > 0),
    label = "No channel has subbasin = 0"
  )
  # Only the 2 real subbasins should appear as channels
  expect_equal(nrow(channels), 2L)

  # gis_routing must not have a source row for subbasin 0
  routing <- DBI::dbGetQuery(con, "SELECT * FROM gis_routing")
  expect_true(
    all(routing$sourceid > 0),
    label = "No routing source has id = 0"
  )

  # Subbasin 1 was previously routing to subbasin 0 (the outlet reach) - it
  # should now route directly to the outlet
  sub1_route <- routing[routing$sourceid == 1L, ]
  expect_equal(nrow(sub1_route), 1L)
  expect_equal(sub1_route$sinkcat, "outlet")

  # The overall database must pass qswat_check_database
  result <- qswat_check_database(db_file, verbose = FALSE)
  expect_true(
    result$passed,
    label = paste(
      "Database with WSNO=0 topology passes SWAT+ Editor check.",
      if (!result$passed)
        paste("Errors:", paste(result$errors, collapse = "; ")) else ""
    )
  )
})


# ---- midlat/midlon tests ----

test_that("midlat and midlon are computed from streams_sf geometry", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  skip_if_not_installed("sf")
  dem        <- system.file("extdata", "ravn_dem.tif",     package = "rQSWATPlus")
  
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  # Build two simple linestrings in UTM-like projected coords (EPSG:32632)
  # Line 1: from (500000, 5000000) to (501000, 5001000)  -> midpoint ~(500500, 5000500)
  # Line 2: from (502000, 5002000) to (502500, 5002500)  -> midpoint ~(502250, 5002250)
  line1 <- sf::st_linestring(matrix(c(500000, 5000000, 501000, 5001000), ncol = 2, byrow = TRUE))
  line2 <- sf::st_linestring(matrix(c(502000, 5002000, 502500, 5002500), ncol = 2, byrow = TRUE))
  streams_sf <- sf::st_sf(
    LINKNO   = c(1L, 2L),
    DSLINKNO = c(-1L, 1L),
    WSNO     = c(1L, 2L),
    strmOrder = c(2L, 1L),
    Length   = c(1000, 500),
    geometry = sf::st_sfc(line1, line2, crs = 32632)
  )

  project <- structure(list(
    dem_file = dem,
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
    ),
    streams_sf = streams_sf
  ), class = "qswat_project")

  qswat_write_database(project, db_file = db_file, overwrite = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  channels <- DBI::dbGetQuery(con, "SELECT midlat, midlon FROM gis_channels")
  expect_equal(nrow(channels), 2L)

  # midlat/midlon must be non-zero (projected midpoints converted to WGS84)
  expect_true(all(channels$midlat != 0), label = "midlat is non-zero")
  expect_true(all(channels$midlon != 0), label = "midlon is non-zero")

  # Both midlatitudes should be positive (northern hemisphere, UTM zone 32N ~45 N)
  expect_true(all(channels$midlat > 0), label = "midlat is positive (N hemisphere)")

  # midlon values should be within a plausible range for UTM zone 32 (~6-18 E)
  expect_true(all(channels$midlon > 0 & channels$midlon < 30),
              label = "midlon in plausible range for UTM zone 32")
})

# ---- usersoil tests ----

# Shared minimal project fixture used by all usersoil tests
.make_usersoil_project <- function() {
  project_dir <- tempfile("usersoil_")
  dir.create(project_dir)
  structure(list(
    project_dir  = project_dir,
    hru_data     = data.frame(
      hru_id        = 1:2,
      subbasin      = c(1L, 2L),
      landuse       = c("AGRL", "FRSD"),
      soil          = c("TX047", "TX236"),
      slope_class   = c(1L, 1L),
      cell_count    = c(100L, 200L),
      area_ha       = c(10.0, 20.0),
      mean_elevation = c(500, 600),
      mean_slope    = c(3.0, 8.0),
      stringsAsFactors = FALSE
    ),
    basin_data = data.frame(
      subbasin       = c(1L, 2L),
      area_ha        = c(10.0, 20.0),
      mean_elevation = c(500, 600),
      min_elevation  = c(490, 580),
      max_elevation  = c(510, 620),
      mean_slope     = c(3.0, 8.0),
      n_hrus         = c(1L, 1L),
      n_landuses     = c(1L, 1L),
      n_soils        = c(1L, 1L),
      stringsAsFactors = FALSE
    ),
    slope_classes  = qswat_create_slope_classes(),
    stream_topology = data.frame(
      LINKNO = c(1L, 2L), DSLINKNO = c(-1L, 1L),
      WSNO = c(1L, 2L), strmOrder = c(2L, 1L),
      Length = c(1000, 500), stringsAsFactors = FALSE
    )
  ), class = "qswat_project")
}

test_that("usersoil = 'FAO_usersoil' populates global_usersoil from FAO data", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  proj_hawqs <- system.file("extdata", "QSWATPlusProjHAWQS.sqlite",
                             package = "rQSWATPlus")
  skip_if(proj_hawqs == "", message = "QSWATPlusProjHAWQS.sqlite not available")

  project  <- .make_usersoil_project()
  db_file  <- tempfile(fileext = ".sqlite")
  on.exit({
    unlink(db_file)
    unlink(project$project_dir, recursive = TRUE)
  }, add = TRUE)

  result <- qswat_write_database(project, db_file = db_file,
                                  overwrite = TRUE,
                                  usersoil = "FAO_usersoil")
  expect_true(file.exists(db_file))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  us <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM global_usersoil")
  expect_gt(us$n, 0L, label = "global_usersoil has rows after FAO_usersoil load")

  # FAO dataset has 13 soil types
  expect_equal(us$n, 13L)

  gs <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM global_soils")
  expect_gt(gs$n, 0L, label = "global_soils populated alongside global_usersoil")

  # SNAM values should be present
  snams <- DBI::dbGetQuery(con, "SELECT SNAM FROM global_usersoil")$SNAM
  expect_true(all(nzchar(snams)))
})

test_that("usersoil = 'global_usersoil' populates global_usersoil with full dataset", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  proj_hawqs <- system.file("extdata", "QSWATPlusProjHAWQS.sqlite",
                             package = "rQSWATPlus")
  skip_if(proj_hawqs == "", message = "QSWATPlusProjHAWQS.sqlite not available")

  project <- .make_usersoil_project()
  db_file <- tempfile(fileext = ".sqlite")
  on.exit({
    unlink(db_file)
    unlink(project$project_dir, recursive = TRUE)
  }, add = TRUE)

  result <- qswat_write_database(project, db_file = db_file,
                                  overwrite = TRUE,
                                  usersoil = "global_usersoil")
  expect_true(file.exists(db_file))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  us <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM global_usersoil")
  expect_gt(us$n, 0L)

  # global dataset is much larger than FAO
  fao_n <- 13L
  expect_gt(us$n, fao_n, label = "global_usersoil > FAO count")

  gs <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM global_soils")
  expect_gt(gs$n, 0L)
})

test_that("usersoil = CSV path populates global_usersoil from user file", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  # Write a minimal usersoil CSV with 3 soil types
  csv_file <- tempfile(fileext = ".csv")
  usersoil_df <- data.frame(
    SNAM       = c("MySoilA", "MySoilB", "MySoilC"),
    NLAYERS    = c(2L, 3L, 1L),
    HYDGRP     = c("B", "C", "A"),
    SOL_ZMX    = c(1000, 1500, 500),
    ANION_EXCL = c(0.5, 0.5, 0.5),
    SOL_CRK    = c(0.5, 0.5, 0.5),
    SOL_Z1     = c(300, 200, 500),
    SOL_BD1    = c(1.4, 1.5, 1.3),
    SOL_AWC1   = c(0.15, 0.12, 0.18),
    SOL_K1     = c(8.0, 5.0, 12.0),
    SOL_CBN1   = c(1.5, 2.0, 1.0),
    CLAY1      = c(25, 35, 15),
    SILT1      = c(30, 25, 20),
    SAND1      = c(45, 40, 65),
    stringsAsFactors = FALSE
  )
  utils::write.csv(usersoil_df, csv_file, row.names = FALSE)
  on.exit(unlink(csv_file), add = TRUE)

  project <- .make_usersoil_project()
  db_file <- tempfile(fileext = ".sqlite")
  on.exit({
    unlink(db_file)
    unlink(project$project_dir, recursive = TRUE)
  }, add = TRUE)

  result <- qswat_write_database(project, db_file = db_file,
                                  overwrite = TRUE,
                                  usersoil = csv_file)
  expect_true(file.exists(db_file))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  us <- DBI::dbGetQuery(con, "SELECT * FROM global_usersoil")
  expect_equal(nrow(us), 3L)
  expect_true("SNAM" %in% names(us))
  expect_setequal(us$SNAM, c("MySoilA", "MySoilB", "MySoilC"))
  expect_equal(us$NLAYERS, c(2, 3, 1))

  gs <- DBI::dbGetQuery(con, "SELECT * FROM global_soils")
  expect_equal(nrow(gs), 3L)
  expect_setequal(gs$SNAM, c("MySoilA", "MySoilB", "MySoilC"))
})

test_that("usersoil = NULL leaves global_usersoil empty (default behaviour)", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  project <- .make_usersoil_project()
  db_file <- tempfile(fileext = ".sqlite")
  on.exit({
    unlink(db_file)
    unlink(project$project_dir, recursive = TRUE)
  }, add = TRUE)

  qswat_write_database(project, db_file = db_file, overwrite = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  us <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM global_usersoil")
  expect_equal(us$n, 0L, label = "global_usersoil empty when usersoil = NULL")
})

test_that("usersoil CSV with missing SNAM column raises an error", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  bad_csv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(SoilName = "X", SOL_Z1 = 300), bad_csv,
                   row.names = FALSE)
  on.exit(unlink(bad_csv), add = TRUE)

  project <- .make_usersoil_project()
  db_file <- tempfile(fileext = ".sqlite")
  on.exit({
    unlink(db_file)
    unlink(project$project_dir, recursive = TRUE)
  }, add = TRUE)

  expect_error(
    qswat_write_database(project, db_file = db_file, overwrite = TRUE,
                          usersoil = bad_csv),
    "SNAM"
  )
})

test_that("usersoil = non-existent file path raises an error", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  project <- .make_usersoil_project()
  db_file <- tempfile(fileext = ".sqlite")
  on.exit({
    unlink(db_file)
    unlink(project$project_dir, recursive = TRUE)
  }, add = TRUE)

  expect_error(
    qswat_write_database(project, db_file = db_file, overwrite = TRUE,
                          usersoil = "/nonexistent/path/soils.csv"),
    "not found"
  )
})

test_that("project$usersoil is used when no explicit usersoil arg given", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  proj_hawqs <- system.file("extdata", "QSWATPlusProjHAWQS.sqlite",
                             package = "rQSWATPlus")
  skip_if(proj_hawqs == "", message = "QSWATPlusProjHAWQS.sqlite not available")

  # Build a project object that has usersoil = "FAO_usersoil" stored in it
  project <- .make_usersoil_project()
  project$usersoil <- "FAO_usersoil"

  db_file <- tempfile(fileext = ".sqlite")
  on.exit({
    unlink(db_file)
    unlink(project$project_dir, recursive = TRUE)
  }, add = TRUE)

  # No explicit usersoil argument - should use project$usersoil
  result <- qswat_write_database(project, db_file = db_file, overwrite = TRUE)
  expect_true(file.exists(db_file))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  us <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM global_usersoil")
  expect_equal(us$n, 13L,
               label = "project$usersoil='FAO_usersoil' loads 13 FAO rows")
})

test_that("explicit usersoil arg overrides project$usersoil", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  proj_hawqs <- system.file("extdata", "QSWATPlusProjHAWQS.sqlite",
                             package = "rQSWATPlus")
  skip_if(proj_hawqs == "", message = "QSWATPlusProjHAWQS.sqlite not available")

  # Project says FAO, but we override with global_usersoil at write time
  project <- .make_usersoil_project()
  project$usersoil <- "FAO_usersoil"

  db_file <- tempfile(fileext = ".sqlite")
  on.exit({
    unlink(db_file)
    unlink(project$project_dir, recursive = TRUE)
  }, add = TRUE)

  # Explicit "global_usersoil" overrides the project-level "FAO_usersoil"
  qswat_write_database(project, db_file = db_file, overwrite = TRUE,
                        usersoil = "global_usersoil")

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  us <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM global_usersoil")
  expect_gt(us$n, 13L,
            label = "explicit usersoil='global_usersoil' overrides FAO (>13 rows)")
})

test_that("qswat_setup with usersoil flows through qswat_write_database", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  proj_hawqs <- system.file("extdata", "QSWATPlusProjHAWQS.sqlite",
                             package = "rQSWATPlus")
  skip_if(proj_hawqs == "", message = "QSWATPlusProjHAWQS.sqlite not available")

  # Simulate what qswat_setup() produces (without DEM loading)
  proj_dir <- tempfile("setup_flow_")
  dir.create(proj_dir)
  on.exit(unlink(proj_dir, recursive = TRUE), add = TRUE)

  project <- qswat_setup(project_dir = proj_dir, usersoil = "FAO_usersoil")
  expect_equal(project$usersoil, "FAO_usersoil")

  # Attach mock HRU/basin data so qswat_write_database() is happy
  project$hru_data <- data.frame(
    hru_id = 1:2, subbasin = c(1L, 2L),
    landuse = c("AGRL", "FRSD"), soil = c("TX047", "TX236"),
    slope_class = c(1L, 1L), cell_count = c(100L, 200L),
    area_ha = c(10.0, 20.0), mean_elevation = c(500, 600),
    mean_slope = c(3.0, 8.0), stringsAsFactors = FALSE
  )
  project$basin_data <- data.frame(
    subbasin = c(1L, 2L), area_ha = c(10.0, 20.0),
    mean_elevation = c(500, 600), min_elevation = c(490, 580),
    max_elevation = c(510, 620), mean_slope = c(3.0, 8.0),
    n_hrus = c(1L, 1L), n_landuses = c(1L, 1L), n_soils = c(1L, 1L),
    stringsAsFactors = FALSE
  )
  project$slope_classes <- qswat_create_slope_classes()
  project$stream_topology <- data.frame(
    LINKNO = c(1L, 2L), DSLINKNO = c(-1L, 1L),
    WSNO = c(1L, 2L), strmOrder = c(2L, 1L),
    Length = c(1000, 500), stringsAsFactors = FALSE
  )

  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  result <- qswat_write_database(project, db_file = db_file, overwrite = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  us <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM global_usersoil")
  expect_equal(us$n, 13L,
               label = "usersoil set in qswat_setup flows to database write")
})

