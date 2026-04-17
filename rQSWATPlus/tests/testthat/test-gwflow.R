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

# ==============================================================================
# helpers shared by qswat_populate_gwflow_gis tests
# ==============================================================================

# Build a minimal qswat_project with synthetic spatial data for gwflow GIS tests
.make_gwflow_test_project <- function(tmpdir, cell_size = 200L,
                                       use_lsu_recharge = TRUE) {
  # 1-km x 1-km square basin in a local Cartesian CRS (no EPSG authority)
  basin_coords <- matrix(
    c(0, 0,  1000, 0,  1000, 1000,  0, 1000,  0, 0),
    ncol = 2, byrow = TRUE
  )
  lsu_sf <- sf::st_as_sf(
    data.frame(subbasin = 1L),
    geometry = sf::st_sfc(sf::st_polygon(list(basin_coords))),
    crs = 32632   # WGS 84 / UTM zone 32N (metres)
  )

  # HRU polygon covering the entire basin
  hru_sf <- sf::st_as_sf(
    data.frame(hru_id = 1L, subbasin = 1L),
    geometry = sf::st_sfc(sf::st_polygon(list(basin_coords))),
    crs = 32632
  )

  # Synthetic DEM raster: flat at 500 m
  dem_file <- file.path(tmpdir, "dem.tif")
  dem_rast <- terra::rast(
    nrows = 100, ncols = 100,
    xmin  = 0,   xmax  = 1000,
    ymin  = 0,   ymax  = 1000,
    crs   = "EPSG:32632",
    vals  = 500
  )
  terra::writeRaster(dem_rast, dem_file, overwrite = TRUE)

  # Synthetic thickness raster: 2000 (units = cm, so 20 m after *0.01)
  thick_file <- file.path(tmpdir, "thickness.tif")
  thick_rast <- terra::rast(dem_rast)
  terra::values(thick_rast) <- 2000
  terra::writeRaster(thick_rast, thick_file, overwrite = TRUE)

  # Conductivity shapefile: GLHYMPS format, one polygon covering the basin
  # logK_Ferr_ = -1100 → logK = -11 → K ≈ 0.085 m/day
  k_sf <- sf::st_as_sf(
    data.frame(logK_Ferr_ = -1100L),
    geometry = sf::st_sfc(sf::st_polygon(list(
      matrix(c(-100, -100,  1100, -100,  1100, 1100,
               -100,  1100, -100, -100),
             ncol = 2, byrow = TRUE)
    ))),
    crs = 32632
  )
  k_file <- file.path(tmpdir, "conductivity.shp")
  sf::st_write(k_sf, k_file, quiet = TRUE)

  # Streams: single line running east-west through the middle of the basin
  stream_sf <- sf::st_as_sf(
    data.frame(LINKNO = 1L, DSLINKNO = -1L, WSNO = 1L,
               strmOrder = 1L, Length = 1000),
    geometry = sf::st_sfc(sf::st_linestring(
      matrix(c(0, 500,  1000, 500), ncol = 2, byrow = TRUE)
    )),
    crs = 32632
  )

  db_file <- file.path(tmpdir, "project.sqlite")

  project <- structure(
    list(
      project_dir     = tmpdir,
      dem_file        = dem_file,
      lsu_sf          = lsu_sf,
      hru_sf          = hru_sf,
      streams_sf      = stream_sf,
      stream_topology = data.frame(
        LINKNO = 1L, DSLINKNO = -1L, WSNO = 1L,
        strmOrder = 1L, Length = 1000,
        stringsAsFactors = FALSE
      ),
      hru_data = data.frame(
        hru_id = 1L, subbasin = 1L, landuse = "AGRL", soil = "TX047",
        slope_class = 1L, cell_count = 100L, area_ha = 100.0,
        mean_elevation = 500, mean_slope = 3.0, stringsAsFactors = FALSE
      ),
      basin_data = data.frame(
        subbasin = 1L, area_ha = 100.0, mean_elevation = 500,
        min_elevation = 490, max_elevation = 510, mean_slope = 3.0,
        n_hrus = 1L, n_landuses = 1L, n_soils = 1L,
        stringsAsFactors = FALSE
      ),
      slope_classes = qswat_create_slope_classes()
    ),
    class = "qswat_project"
  )

  project <- qswat_write_database(project, db_file = db_file, overwrite = TRUE)

  # gwflow config with small cells so the 1-km basin produces a 5x5 grid
  cfg              <- qswat_read_gwflow_config()
  cfg$cell_size    <- cell_size
  # recharge = 3 (both HRU + LSU) when use_lsu_recharge = TRUE,
  # recharge = 1 (HRU only)       when use_lsu_recharge = FALSE
  cfg$hruorlsu_recharge <- if (use_lsu_recharge) 3L else 1L

  project <- qswat_setup_gwflow(project, gwflow_config = cfg, overwrite = TRUE)

  list(project = project, cfg = cfg, k_file = k_file, thick_file = thick_file)
}


test_that("qswat_populate_gwflow_gis populates gwflow_grid with active cells", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  tmpdir <- tempfile("gwgis_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  res <- .make_gwflow_test_project(tmpdir, cell_size = 200L)
  project  <- res$project
  cfg      <- res$cfg

  result <- qswat_populate_gwflow_gis(
    project           = project,
    gwflow_config     = cfg,
    conductivity_file = res$k_file,
    thickness_file    = res$thick_file,
    thickness_scale   = 0.01,
    overwrite         = TRUE
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  grid_rows <- DBI::dbGetQuery(con, "SELECT * FROM gwflow_grid")
  # 1-km basin with 200-m cells gives a 5×5 = 25-cell grid; all 25 should be
  # active since the basin covers the full extent
  expect_gt(nrow(grid_rows), 0L)
  expect_true(all(grid_rows$status > 0L))

  # Elevation should be ~500 m (the synthetic DEM value)
  expect_true(all(abs(grid_rows$elevation - 500) < 1e-3))

  # Aquifer thickness: 2000 cm * 0.01 = 20 m
  expect_true(all(abs(grid_rows$aquifer_thickness - 20) < 0.5))

  # initial_head = elevation - wt_depth
  expect_true(all(abs(grid_rows$initial_head -
                        (grid_rows$elevation - cfg$wt_depth)) < 1e-3))
})


test_that("qswat_populate_gwflow_gis populates gwflow_zone from GLHYMPS file", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  tmpdir <- tempfile("gwzone_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  res <- .make_gwflow_test_project(tmpdir)
  project <- res$project
  cfg     <- res$cfg

  qswat_populate_gwflow_gis(
    project           = project,
    gwflow_config     = cfg,
    conductivity_file = res$k_file,
    thickness_file    = res$thick_file,
    thickness_scale   = 0.01,
    overwrite         = TRUE
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  zone_rows <- DBI::dbGetQuery(con, "SELECT * FROM gwflow_zone")
  expect_equal(nrow(zone_rows), 1L)

  # K_mday = 10^(-1100/100) * 1000 * 9.81 / 0.001 * 86400
  expected_k <- 10^(-11) * 1000 * 9.81 / 0.001 * 86400
  expect_equal(zone_rows$aquifer_k, expected_k, tolerance = 1e-3)
  expect_equal(zone_rows$specific_yield,      cfg$init_sy)
  expect_equal(zone_rows$streambed_k,         cfg$streambed_k)
  expect_equal(zone_rows$streambed_thickness, cfg$streambed_thick)
})


test_that("qswat_populate_gwflow_gis populates gwflow_rivcell", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  tmpdir <- tempfile("gwriv_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  res     <- .make_gwflow_test_project(tmpdir, cell_size = 200L)
  project <- res$project
  cfg     <- res$cfg

  qswat_populate_gwflow_gis(
    project           = project,
    gwflow_config     = cfg,
    conductivity_file = res$k_file,
    thickness_file    = res$thick_file,
    overwrite         = TRUE
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  riv <- DBI::dbGetQuery(con, "SELECT * FROM gwflow_rivcell")
  # The 1-km horizontal stream crosses 5 cells; each segment should be ~200 m
  expect_gt(nrow(riv), 0L)
  expect_true(all(riv$length_m > 0))
  # Total stream length across all cells should sum to ~1000 m
  expect_equal(sum(riv$length_m), 1000, tolerance = 1)
})


test_that("qswat_populate_gwflow_gis populates gwflow_lsucell when recharge >= 2", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  tmpdir <- tempfile("gwlsu_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  res     <- .make_gwflow_test_project(tmpdir, cell_size = 200L,
                                        use_lsu_recharge = TRUE)
  project <- res$project
  cfg     <- res$cfg

  qswat_populate_gwflow_gis(
    project           = project,
    gwflow_config     = cfg,
    conductivity_file = res$k_file,
    thickness_file    = res$thick_file,
    overwrite         = TRUE
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  lsu <- DBI::dbGetQuery(con, "SELECT * FROM gwflow_lsucell")
  expect_gt(nrow(lsu), 0L)
  expect_true(all(lsu$area_m2 > 0))

  # Total area across all cell-LSU pairs should equal basin area (1 km^2)
  expect_equal(sum(lsu$area_m2), 1e6, tolerance = 1e3)
})


test_that("qswat_populate_gwflow_gis populates gwflow_hrucell when recharge is 1 or 3", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  # Case 1: recharge = 3 (both HRU + LSU)
  tmpdir <- tempfile("gwhru_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  res     <- .make_gwflow_test_project(tmpdir, cell_size = 200L,
                                        use_lsu_recharge = TRUE)  # recharge = 3
  project <- res$project
  cfg     <- res$cfg

  qswat_populate_gwflow_gis(
    project           = project,
    gwflow_config     = cfg,
    conductivity_file = res$k_file,
    thickness_file    = res$thick_file,
    overwrite         = TRUE
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  hru <- DBI::dbGetQuery(con, "SELECT * FROM gwflow_hrucell")
  expect_gt(nrow(hru), 0L)
  expect_true(all(hru$area_m2 > 0))
  DBI::dbDisconnect(con)

  # Case 2: recharge = 1 (HRU-only; gwflow_hrucell should also be populated)
  tmpdir2 <- tempfile("gwhru1_")
  dir.create(tmpdir2)
  on.exit(unlink(tmpdir2, recursive = TRUE), add = TRUE)

  res2     <- .make_gwflow_test_project(tmpdir2, cell_size = 200L,
                                         use_lsu_recharge = FALSE)  # recharge = 1
  project2 <- res2$project
  cfg2     <- res2$cfg

  qswat_populate_gwflow_gis(
    project           = project2,
    gwflow_config     = cfg2,
    conductivity_file = res2$k_file,
    thickness_file    = res2$thick_file,
    overwrite         = TRUE
  )

  con2 <- DBI::dbConnect(RSQLite::SQLite(), project2$db_file)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)

  hru2 <- DBI::dbGetQuery(con2, "SELECT * FROM gwflow_hrucell")
  expect_gt(nrow(hru2), 0L)

  # With recharge = 1, gwflow_lsucell should be empty
  lsu2 <- DBI::dbGetQuery(con2, "SELECT * FROM gwflow_lsucell")
  expect_equal(nrow(lsu2), 0L)
})


test_that("qswat_populate_gwflow_gis updates gwflow_base row/col counts", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  tmpdir <- tempfile("gwrc_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  res     <- .make_gwflow_test_project(tmpdir, cell_size = 200L)
  project <- res$project
  cfg     <- res$cfg

  qswat_populate_gwflow_gis(
    project           = project,
    gwflow_config     = cfg,
    conductivity_file = res$k_file,
    thickness_file    = res$thick_file,
    overwrite         = TRUE
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  base <- DBI::dbGetQuery(con, "SELECT row_count, col_count FROM gwflow_base")
  # 1-km basin / 200-m cells → 5 rows × 5 cols
  expect_equal(base$row_count, 5L)
  expect_equal(base$col_count, 5L)
})


test_that("qswat_populate_gwflow_gis attaches gwflow_grid_sf to project", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  tmpdir <- tempfile("gwsf_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  res     <- .make_gwflow_test_project(tmpdir, cell_size = 200L)
  project <- res$project
  cfg     <- res$cfg

  result <- qswat_populate_gwflow_gis(
    project           = project,
    gwflow_config     = cfg,
    conductivity_file = res$k_file,
    thickness_file    = res$thick_file,
    overwrite         = TRUE
  )

  expect_true(!is.null(result$gwflow_grid_sf))
  expect_true(inherits(result$gwflow_grid_sf, "sf"))
  expect_true("Id" %in% names(result$gwflow_grid_sf))
  expect_true("Avg_active" %in% names(result$gwflow_grid_sf))
  expect_true("Avg_Thick"  %in% names(result$gwflow_grid_sf))
  expect_true("Avg_elevat" %in% names(result$gwflow_grid_sf))
  expect_true("zone"       %in% names(result$gwflow_grid_sf))
  expect_true("boundary"   %in% names(result$gwflow_grid_sf))
})


test_that("qswat_populate_gwflow_gis validates missing inputs", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  bad_project <- structure(list(db_file = NULL), class = "qswat_project")
  expect_error(
    qswat_populate_gwflow_gis(bad_project, conductivity_file = "x",
                               thickness_file = "y"),
    "No database found"
  )

  tmpdir  <- tempfile("gwval_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  db_file <- file.path(tmpdir, "p.sqlite")
  file.create(db_file)

  no_lsu  <- structure(list(db_file = db_file, lsu_sf = NULL),
                        class = "qswat_project")
  expect_error(
    qswat_populate_gwflow_gis(no_lsu, conductivity_file = "x",
                               thickness_file = "y"),
    "lsu_sf"
  )
})
