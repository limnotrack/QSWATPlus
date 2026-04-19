test_that("qswat_read_gwflow_config reads bundled ini file", {
  ini <- system.file("extdata", "gwflow.ini", package = "rQSWATPlus")
  cfg <- qswat_read_gwflow_config(ini)

  expect_type(cfg, "list")
  expect_equal(cfg$cell_size, 200L)
  expect_equal(cfg$boundary_conditions, 1L)
  expect_equal(cfg$wt_depth, 5.0)
  expect_equal(cfg$init_no3, 3.0)
  expect_equal(cfg$init_p, 0.05)
  expect_equal(cfg$nit_sorp, 1.0)
  expect_equal(cfg$denit_constant, -0.0001)
  expect_equal(cfg$solute_transport, 1L)
  expect_equal(cfg$ext_pumping, 0L)
  expect_equal(cfg$daily_output, 1L)
  expect_equal(cfg$annual_output, 1L)
})

test_that("qswat_read_gwflow_config uses defaults when NULL path", {
  cfg <- qswat_read_gwflow_config()
  expect_type(cfg, "list")
  expect_equal(cfg$cell_size, 200L)
})

test_that("qswat_read_gwflow_config errors on missing file", {
  expect_error(
    qswat_read_gwflow_config("/nonexistent/gwflow.ini"),
    "not found"
  )
})

test_that("qswat_setup_gwflow creates gwflow tables", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  project_dir <- tempfile("gw_")
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

  project <- qswat_write_database(project, db_file = db_file, overwrite = TRUE)
  cfg <- qswat_read_gwflow_config()
  result <- qswat_setup_gwflow(project, gwflow_config = cfg, overwrite = TRUE)

  expect_true(result$use_gwflow)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tbls <- DBI::dbListTables(con)
  gwflow_tables <- c("gwflow_base", "gwflow_zone", "gwflow_grid",
                     "gwflow_out_days", "gwflow_obs_locs", "gwflow_solutes",
                     "gwflow_init_conc", "gwflow_hrucell", "gwflow_fpcell",
                     "gwflow_rivcell", "gwflow_lsucell", "gwflow_rescell")
  for (tbl in gwflow_tables) {
    expect_true(tbl %in% tbls, label = paste0("table '", tbl, "' exists"))
  }
})

test_that("gwflow_base is populated with correct values", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  project_dir <- tempfile("gwbase_")
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

  project <- qswat_write_database(project, db_file = db_file, overwrite = TRUE)

  cfg <- qswat_read_gwflow_config()
  qswat_setup_gwflow(project, gwflow_config = cfg, overwrite = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  base <- DBI::dbGetQuery(con, "SELECT * FROM gwflow_base")
  expect_equal(nrow(base), 1)
  expect_equal(base$cell_size, cfg$cell_size)
  expect_equal(base$boundary_conditions, cfg$boundary_conditions)
  expect_equal(base$water_table_depth, cfg$wt_depth)
  expect_equal(base$et_extinction_depth, cfg$exdp)
  expect_equal(base$recharge, cfg$hruorlsu_recharge)
  expect_equal(base$solute_transport, cfg$solute_transport)
})

test_that("gwflow_solutes contains 10 default solutes", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  project_dir <- tempfile("gwsol_")
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

  project <- qswat_write_database(project, db_file = db_file, overwrite = TRUE)
  cfg <- qswat_read_gwflow_config()
  qswat_setup_gwflow(project, gwflow_config = cfg, overwrite = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sol <- DBI::dbGetQuery(con, "SELECT * FROM gwflow_solutes")
  expect_equal(nrow(sol), 10)
  expect_true("no3-n" %in% sol$solute_name)
  expect_true("p" %in% sol$solute_name)

  # Check NO3 and P initial concentrations come from config
  no3_row <- sol[sol$solute_name == "no3-n", ]
  expect_equal(no3_row$init_conc, cfg$init_no3)
  p_row <- sol[sol$solute_name == "p", ]
  expect_equal(p_row$init_conc, cfg$init_p)
})

test_that("qswat_setup_gwflow updates use_gwflow in project_config", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  project_dir <- tempfile("gwflag_")
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

  project <- qswat_write_database(project, db_file = db_file, overwrite = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  pc_before <- DBI::dbGetQuery(con, "SELECT use_gwflow FROM project_config")
  DBI::dbDisconnect(con)
  expect_equal(pc_before$use_gwflow, 0L)

  qswat_setup_gwflow(project, overwrite = TRUE)

  con2 <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  pc_after <- DBI::dbGetQuery(con2, "SELECT use_gwflow FROM project_config")
  expect_equal(pc_after$use_gwflow, 1L)
  
  tabs <- DBI::dbListTables(con2)
  gwflow_tabs <- tabs[grepl("gwflow", tabs)]
  for (tbl in gwflow_tabs) {
    # expect_true(tbl %in% tabs, label = paste0("table '", tbl, "' exists"))
    df <- DBI::dbGetQuery(con2, paste0("SELECT * FROM ", tbl))
    expect_true(nrow(df) > 0, label = paste0("table '", tbl, "' is queryable"))
  }
  
  gwflow_grid <- DBI::dbGetQuery(con2, "SELECT * FROM gwflow_grid")
})

test_that("qswat_setup_gwflow validates project object", {
  expect_error(
    qswat_setup_gwflow(list()),
    "qswat_project"
  )
  project <- structure(list(db_file = NULL), class = "qswat_project")
  expect_error(
    qswat_setup_gwflow(project),
    "No database found"
  )
})
