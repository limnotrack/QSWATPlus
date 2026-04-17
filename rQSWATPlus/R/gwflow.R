#' Read gwflow Configuration File
#'
#' Reads a SWAT+ gwflow initialisation (`gwflow.ini`) file and returns its
#' settings as a named list.  The ini file uses the `[DEFAULT]` section
#' format produced by Python's `configparser` module.
#'
#' @param ini_file Character.  Path to the `gwflow.ini` file.  If `NULL`
#'   the bundled template shipped with the package is used.
#'
#' @return A named list of gwflow configuration parameters with their
#'   default values filled in where a key is absent from the file.
#'
#' @examples
#' ini <- system.file("extdata", "gwflow.ini", package = "rQSWATPlus")
#' cfg <- qswat_read_gwflow_config(ini)
#' cfg$cell_size
#'
#' @export
qswat_read_gwflow_config <- function(ini_file = NULL) {

  if (is.null(ini_file)) {
    ini_file <- system.file("extdata", "gwflow.ini", package = "rQSWATPlus")
  }

  if (!file.exists(ini_file)) {
    stop("gwflow.ini not found: ", ini_file, call. = FALSE)
  }

  lines <- readLines(ini_file, warn = FALSE)

  # Remove comment lines (starting with #) and blank lines, strip inline comments
  lines <- lines[!grepl("^\\s*#", lines)]
  lines <- lines[nzchar(trimws(lines))]
  lines <- sub("\\s*#.*$", "", lines)  # strip trailing comments

  cfg <- list()
  for (line in lines) {
    line <- trimws(line)
    if (grepl("^\\[", line)) next  # section header
    if (!grepl("=", line)) next
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) next
    key   <- trimws(parts[1])
    value <- trimws(paste(parts[-1], collapse = "="))
    cfg[[key]] <- value
  }

  # Helper to coerce with a default
  get_int  <- function(k, d) { v <- cfg[[k]]; if (is.null(v)) d else as.integer(v) }
  get_dbl  <- function(k, d) { v <- cfg[[k]]; if (is.null(v)) d else as.numeric(v) }

  list(
    cell_size            = get_int("cell_size", 200L),
    boundary_conditions  = get_int("Boundary_conditions", 1L),
    hruorlsu_recharge    = get_int("HRUorLSU_recharge", 2L),
    gw_soiltransfer      = get_int("GW_soiltransfer", 1L),
    saturation_excess    = get_int("Saturation_excess", 1L),
    ext_pumping          = get_int("ext_pumping", 0L),
    reservoir_exchange   = get_int("reservoir_exchange", 1L),
    wetland_exchange     = get_int("wetland_exchange", 1L),
    floodplain_exchange  = get_int("floodplain_exchange", 1L),
    canal_seepage        = get_int("canal_seepage", 0L),
    solute_transport     = get_int("solute_transport", 1L),
    transport_steps      = get_int("transport_steps", 1L),
    recharge_delay       = get_dbl("recharge_delay", 0.0),
    exdp                 = get_dbl("EXDP", 1.00),
    wt_depth             = get_dbl("WT_depth", 5.0),
    river_depth          = get_dbl("river_depth", 5.0),
    tile_depth           = get_dbl("tile_depth", 1.22),
    tile_area            = get_dbl("tile_area", 50.0),
    tile_k               = get_dbl("tile_k", 5.0),
    tile_groups          = get_int("tile_groups", 0L),
    tile_groups_number   = get_int("tile_groups_number", 1L),
    resbed_thickness     = get_dbl("resbed_thickness", 2.0),
    resbed_k             = get_dbl("resbed_k", 9.99e-6),
    wet_thickness        = get_dbl("wet_thickness", 0.25),
    daily_output         = get_int("daily_output", 1L),
    annual_output        = get_int("annual_output", 1L),
    aa_output            = get_int("aa_output", 1L),
    row_det              = get_int("row_det", 0L),
    col_det              = get_int("col_det", 0L),
    timestep_balance     = get_dbl("timestep_balance", 1.0),
    init_sy              = get_dbl("init_sy", 0.2),
    init_n               = get_dbl("init_n", 0.25),
    streambed_k          = get_dbl("streambed_k", 0.005),
    streambed_thick      = get_dbl("streambed_thick", 0.5),
    init_no3             = get_dbl("init_NO3", 3.0),
    init_p               = get_dbl("init_P", 0.05),
    denit_constant       = get_dbl("denit_constant", -0.0001),
    disp_coef            = get_dbl("disp_coef", 5.0),
    nit_sorp             = get_dbl("nit_sorp", 1.0),
    pho_sorp             = get_dbl("pho_sorp", 1.0)
  )
}


#' Set Up SWAT+ Groundwater Flow (gwflow) Tables
#'
#' Creates the gwflow tables in the project SQLite database and populates
#' `gwflow_base` and `gwflow_solutes` from the supplied configuration.
#' This is the R equivalent of the gwflow setup performed by the
#' `GWFlow.createTables()` and related methods in the QGIS plugin's
#' `gwflow.py`.
#'
#' The function mirrors the SWAT+ gwflow database schema defined in the
#' Python plugin: `gwflow_base`, `gwflow_zone`, `gwflow_grid`,
#' `gwflow_out_days`, `gwflow_obs_locs`, `gwflow_solutes`,
#' `gwflow_init_conc`, `gwflow_hrucell`, `gwflow_fpcell`,
#' `gwflow_rivcell`, `gwflow_lsucell`, `gwflow_rescell`.
#'
#' After calling this function the database is ready to receive spatial
#' cell data (zones, grid cells, river cells, etc.) that require GIS
#' processing not yet automated in rQSWATPlus.
#'
#' @param project A `qswat_project` object returned by
#'   [qswat_write_database()].
#' @param gwflow_config A named list of gwflow settings as returned by
#'   [qswat_read_gwflow_config()].  If `NULL` the bundled `gwflow.ini`
#'   defaults are used.
#' @param overwrite Logical.  If `TRUE`, drop and recreate existing gwflow
#'   tables.  Default `FALSE`.
#'
#' @return The `project` object with `project$use_gwflow` set to `TRUE`
#'   (invisibly).
#'
#' @details
#' The following tables are created:
#' \describe{
#'   \item{gwflow_base}{Single-row table with global gwflow parameters.}
#'   \item{gwflow_zone}{Aquifer hydraulic parameter zones.}
#'   \item{gwflow_grid}{Active grid cells (populated by GIS workflow).}
#'   \item{gwflow_out_days}{Optional output day pairs (year, jday).}
#'   \item{gwflow_obs_locs}{Observation cell IDs.}
#'   \item{gwflow_solutes}{Chemical solute definitions and initial concentrations.}
#'   \item{gwflow_init_conc}{Per-cell initial solute concentrations.}
#'   \item{gwflow_hrucell}{HRU-to-cell area mapping.}
#'   \item{gwflow_fpcell}{Floodplain-cell connections.}
#'   \item{gwflow_rivcell}{River-cell connections.}
#'   \item{gwflow_lsucell}{LSU-cell area mapping.}
#'   \item{gwflow_rescell}{Reservoir-cell connections.}
#' }
#'
#' @examples
#' \dontrun{
#' cfg <- qswat_read_gwflow_config()
#' project <- qswat_setup_gwflow(project, gwflow_config = cfg)
#' }
#'
#' @export
qswat_setup_gwflow <- function(project,
                                gwflow_config = NULL,
                                overwrite = FALSE) {

  if (!inherits(project, "qswat_project")) {
    stop("'project' must be a qswat_project object.", call. = FALSE)
  }
  if (is.null(project$db_file) || !file.exists(project$db_file)) {
    stop("No database found. Run qswat_write_database() first.", call. = FALSE)
  }

  if (is.null(gwflow_config)) {
    gwflow_config <- qswat_read_gwflow_config()
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  .create_gwflow_tables(con, overwrite = overwrite)
  .write_gwflow_base(con, gwflow_config)
  .write_gwflow_solutes(con, gwflow_config)

  # Update project_config to flag gwflow as enabled
  DBI::dbExecute(con, "UPDATE project_config SET use_gwflow = 1")

  message("gwflow tables initialised in: ", project$db_file)
  project$use_gwflow <- TRUE
  invisible(project)
}


# ---- internal helpers --------------------------------------------------------

#' Create gwflow Database Tables
#' @noRd
.create_gwflow_tables <- function(con, overwrite = FALSE) {

  # Drop existing tables if overwrite requested (order respects FK constraints)
  if (overwrite) {
    drop_order <- c("gwflow_hrucell", "gwflow_fpcell", "gwflow_rivcell",
                    "gwflow_lsucell", "gwflow_rescell", "gwflow_init_conc",
                    "gwflow_obs_locs", "gwflow_out_days", "gwflow_solutes",
                    "gwflow_grid", "gwflow_zone", "gwflow_base")
    for (tbl in drop_order) {
      DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", tbl))
    }
  }

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gwflow_base (
      cell_size          INTEGER,
      row_count          INTEGER,
      col_count          INTEGER,
      boundary_conditions INTEGER,
      recharge           INTEGER,
      soil_transfer      INTEGER,
      saturation_excess  INTEGER,
      external_pumping   INTEGER,
      tile_drainage      INTEGER,
      reservoir_exchange INTEGER,
      wetland_exchange   INTEGER,
      floodplain_exchange INTEGER,
      canal_seepage      INTEGER,
      solute_transport   INTEGER,
      transport_steps    INTEGER,
      disp_coef          REAL,
      recharge_delay     REAL,
      et_extinction_depth REAL,
      water_table_depth  REAL,
      river_depth        REAL,
      tile_depth         REAL,
      tile_area          REAL,
      tile_k             REAL,
      tile_groups        INTEGER,
      resbed_thickness   REAL,
      resbed_k           REAL,
      wet_thickness      REAL,
      daily_output       INTEGER,
      annual_output      INTEGER,
      aa_output          INTEGER,
      daily_output_row   INTEGER,
      daily_output_col   INTEGER,
      timestep_balance   REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gwflow_zone (
      zone_id            INTEGER PRIMARY KEY,
      aquifer_k          REAL,
      specific_yield     REAL,
      streambed_k        REAL,
      streambed_thickness REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gwflow_grid (
      cell_id            INTEGER PRIMARY KEY,
      status             INTEGER,
      zone               INTEGER REFERENCES gwflow_zone (zone_id),
      elevation          REAL,
      aquifer_thickness  REAL,
      extinction_depth   REAL,
      initial_head       REAL,
      tile               INTEGER
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gwflow_out_days (
      year  INTEGER,
      jday  INTEGER
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gwflow_obs_locs (
      cell_id INTEGER
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gwflow_solutes (
      solute_name TEXT,
      sorption    REAL,
      rate_const  REAL,
      canal_irr   REAL,
      init_data   TEXT,
      init_conc   REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gwflow_init_conc (
      cell_id  INTEGER REFERENCES gwflow_grid (cell_id),
      init_no3 REAL DEFAULT 0,
      init_p   REAL DEFAULT 0,
      init_so4 REAL DEFAULT 0,
      init_ca  REAL DEFAULT 0,
      init_mg  REAL DEFAULT 0,
      init_na  REAL DEFAULT 0,
      init_k   REAL DEFAULT 0,
      init_cl  REAL DEFAULT 0,
      init_co3 REAL DEFAULT 0,
      init_hco3 REAL DEFAULT 0
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gwflow_hrucell (
      cell_id INTEGER REFERENCES gwflow_grid (cell_id),
      hru     INTEGER REFERENCES gis_hrus (id),
      area_m2 REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gwflow_fpcell (
      cell_id     INTEGER REFERENCES gwflow_grid (cell_id),
      channel     INTEGER REFERENCES gis_channels (id),
      area_m2     REAL,
      conductivity REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gwflow_rivcell (
      cell_id  INTEGER REFERENCES gwflow_grid (cell_id),
      channel  INTEGER REFERENCES gis_channels (id),
      length_m REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gwflow_lsucell (
      cell_id INTEGER REFERENCES gwflow_grid (cell_id),
      lsu     INTEGER REFERENCES gis_lsus (id),
      area_m2 REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gwflow_rescell (
      cell_id   INTEGER REFERENCES gwflow_grid (cell_id),
      res       INTEGER REFERENCES gis_water (id),
      res_stage REAL
    )
  ")
}


#' Write gwflow_base Table Row
#' @noRd
.write_gwflow_base <- function(con, cfg) {
  df <- data.frame(
    cell_size           = cfg$cell_size,
    row_count           = 0L,
    col_count           = 0L,
    boundary_conditions = cfg$boundary_conditions,
    recharge            = cfg$hruorlsu_recharge,
    soil_transfer       = cfg$gw_soiltransfer,
    saturation_excess   = cfg$saturation_excess,
    external_pumping    = cfg$ext_pumping,
    tile_drainage       = 0L,
    reservoir_exchange  = cfg$reservoir_exchange,
    wetland_exchange    = cfg$wetland_exchange,
    floodplain_exchange = cfg$floodplain_exchange,
    canal_seepage       = cfg$canal_seepage,
    solute_transport    = cfg$solute_transport,
    transport_steps     = cfg$transport_steps,
    disp_coef           = cfg$disp_coef,
    recharge_delay      = cfg$recharge_delay,
    et_extinction_depth = cfg$exdp,
    water_table_depth   = cfg$wt_depth,
    river_depth         = cfg$river_depth,
    tile_depth          = cfg$tile_depth,
    tile_area           = cfg$tile_area,
    tile_k              = cfg$tile_k,
    tile_groups         = cfg$tile_groups,
    resbed_thickness    = cfg$resbed_thickness,
    resbed_k            = cfg$resbed_k,
    wet_thickness       = cfg$wet_thickness,
    daily_output        = cfg$daily_output,
    annual_output       = cfg$annual_output,
    aa_output           = cfg$aa_output,
    daily_output_row    = cfg$row_det,
    daily_output_col    = cfg$col_det,
    timestep_balance    = cfg$timestep_balance,
    stringsAsFactors    = FALSE
  )
  DBI::dbWriteTable(con, "gwflow_base", df, append = TRUE, row.names = FALSE)
}


#' Write gwflow_solutes Default Rows
#'
#' Inserts the ten default solutes matching the Python gwflow.py defaults.
#' @noRd
.write_gwflow_solutes <- function(con, cfg) {
  solutes <- data.frame(
    solute_name = c("no3-n", "p",      "so4", "ca", "mg",
                    "na",    "k",      "cl",  "co3", "hco3"),
    sorption    = c(cfg$nit_sorp, cfg$pho_sorp, 1, 1, 1, 1, 1, 1, 1, 1),
    rate_const  = c(cfg$denit_constant, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    canal_irr   = c(3,    0.05, 0, 0, 0, 0, 0, 0, 0, 0),
    init_data   = rep("single", 10),
    init_conc   = c(cfg$init_no3, cfg$init_p, 100, 50, 30, 40, 1, 25, 1, 80),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "gwflow_solutes", solutes,
                     append = TRUE, row.names = FALSE)
}


# ==============================================================================
# GIS-dependent gwflow table population
# ==============================================================================

#' Populate GIS-Dependent gwflow Tables
#'
#' Uses spatial data (DEM, aquifer thickness raster, hydraulic conductivity
#' shapefile, stream network, and landscape units stored in the project) to
#' populate the GIS-dependent gwflow tables: `gwflow_grid`, `gwflow_zone`,
#' `gwflow_rivcell`, `gwflow_lsucell`, `gwflow_fpcell`, `gwflow_hrucell`, and
#' `gwflow_rescell`.
#'
#' This function is the R equivalent of the GIS processing performed by the
#' `GWFlow.run()` method in the QGIS plugin's `gwflow.py`, specifically the
#' steps that create and spatially analyse the fishnet grid and write the
#' cell-based gwflow tables to the project database.
#'
#' @param project A `qswat_project` object that has been through
#'   [qswat_write_database()] and [qswat_setup_gwflow()].  Must contain
#'   `lsu_sf` (landscape-unit polygons), `dem_file` (path to the DEM raster),
#'   and `db_file` (path to the project SQLite database).
#' @param gwflow_config A named list from [qswat_read_gwflow_config()].  If
#'   `NULL`, the bundled `gwflow.ini` defaults are used.
#' @param conductivity_file Character.  Path to the aquifer hydraulic
#'   conductivity polygon shapefile.  Two formats are recognised:
#'   \describe{
#'     \item{GLHYMPS}{A file containing a `logK_Ferr_` column with
#'       log10(intrinsic permeability in m\eqn{^2}) × 100 as integer values (as
#'       distributed by the GLHYMPS database).  Values are automatically
#'       converted to hydraulic conductivity in m/day using
#'       \eqn{K = 10^{logK/100} \times \rho g / \mu \times 86400}.}
#'     \item{Generic}{Any polygon shapefile with a numeric column giving
#'       hydraulic conductivity directly in m/day.  Supply the column name via
#'       `conductivity_column`.}
#'   }
#' @param conductivity_column Character or `NULL`.  Name of the column in
#'   `conductivity_file` that contains hydraulic conductivity in m/day.  When
#'   `NULL` (default) the function auto-detects the GLHYMPS format by looking
#'   for a `logK_Ferr_` column.
#' @param thickness_file Character.  Path to the aquifer thickness raster.
#' @param thickness_scale Numeric.  Scale factor applied to raster values to
#'   convert them to metres.  Use `0.01` (default) when the raster stores
#'   thickness in centimetres (as in the GLHYMPS-derived global dataset), or
#'   `1.0` when values are already in metres.
#' @param tiles_file Character or `NULL`.  Path to a tile-drain polygon
#'   shapefile.  When provided, grid cells that intersect this layer receive
#'   `tile = 1` in `gwflow_grid`.  Default `NULL`.
#' @param lake_file Character or `NULL`.  Path to a lake / reservoir polygon
#'   shapefile.  Cells that intersect each lake are added to `gwflow_rescell`.
#'   Default `NULL`.
#' @param overwrite Logical.  If `TRUE` (default), deletes all rows from the
#'   GIS-dependent gwflow tables before inserting new data so the function is
#'   idempotent.
#'
#' @return The `project` object with a new `gwflow_grid_sf` field containing
#'   the fishnet grid `sf` object (invisibly).
#'
#' @details
#' The function replicates the Python plugin's GIS pipeline:
#' \enumerate{
#'   \item **Fishnet creation** – A regular rectangular grid of
#'     `cell_size × cell_size` cells is created over the basin extent.  Cells
#'     are numbered in raster order (left-to-right, top-to-bottom), matching
#'     the Python fishnet function.
#'   \item **Active-cell detection** – Grid cells that intersect the watershed
#'     polygon are flagged as active (`Avg_active = 1`).
#'   \item **Aquifer thickness** – After filling NoData values with a focal
#'     mean, the mean raster value per cell is extracted and scaled by
#'     `thickness_scale`.
#'   \item **Ground elevation** – The mean DEM value per grid cell is
#'     extracted.
#'   \item **Conductivity zones** – The conductivity shapefile is reprojected
#'     to the project CRS, clipped to the basin, and each polygon is assigned a
#'     sequential zone ID.  For each cell the zone with the greatest overlap
#'     determines the assigned zone (ties resolved by mean, matching Python).
#'   \item **Boundary cells** – Cells that touch the basin perimeter receive
#'     `status = 2`; interior active cells receive `status = 1`.
#'   \item **Tile-drain cells** (optional) – Cells intersecting `tiles_file`
#'     receive `tile = 1`.
#'   \item **`gwflow_zone`** – One row per conductivity zone.
#'   \item **`gwflow_grid`** – One row per active cell (status > 0).
#'   \item **`gwflow_rivcell`** – Stream length per (cell, channel) pair.
#'   \item **`gwflow_lsucell`** – Intersection area per (cell, LSU) pair
#'     (populated when `hruorlsu_recharge` ≥ 2).
#'   \item **`gwflow_fpcell`** – Floodplain cells; left empty because
#'     rQSWATPlus does not split landscape units into upslope / floodplain
#'     components.
#'   \item **`gwflow_hrucell`** – Intersection area per (cell, HRU) pair
#'     (populated when `hruorlsu_recharge` is 1 or 3).
#'   \item **`gwflow_rescell`** – Reservoir cells derived from `gis_water`
#'     joined through `gwflow_lsucell`, and optionally from `lake_file`.
#' }
#' After all tables are written, `gwflow_base.row_count` and
#' `gwflow_base.col_count` are updated with the actual fishnet dimensions.
#'
#' @examples
#' \dontrun{
#' cfg     <- qswat_read_gwflow_config()
#' project <- qswat_setup_gwflow(project, gwflow_config = cfg)
#' project <- qswat_populate_gwflow_gis(
#'   project           = project,
#'   gwflow_config     = cfg,
#'   conductivity_file = "GLHYMPS_basin.shp",
#'   thickness_file    = "aquifer_thickness_cm.tif"
#' )
#' }
#'
#' @export
qswat_populate_gwflow_gis <- function(project,
                                       gwflow_config       = NULL,
                                       conductivity_file,
                                       conductivity_column = NULL,
                                       thickness_file,
                                       thickness_scale     = 0.01,
                                       tiles_file          = NULL,
                                       lake_file           = NULL,
                                       overwrite           = TRUE) {

  # ---- validate inputs -------------------------------------------------------
  if (!inherits(project, "qswat_project")) {
    stop("'project' must be a qswat_project object.", call. = FALSE)
  }
  if (is.null(project$db_file) || !file.exists(project$db_file)) {
    stop(
      "No database found. ",
      "Run qswat_write_database() and qswat_setup_gwflow() first.",
      call. = FALSE
    )
  }
  if (is.null(project$lsu_sf)) {
    stop("project$lsu_sf is required. Run qswat_create_hrus() first.",
         call. = FALSE)
  }
  if (!file.exists(conductivity_file)) {
    stop("conductivity_file not found: ", conductivity_file, call. = FALSE)
  }
  if (!file.exists(thickness_file)) {
    stop("thickness_file not found: ", thickness_file, call. = FALSE)
  }
  if (!is.null(tiles_file) && !file.exists(tiles_file)) {
    stop("tiles_file not found: ", tiles_file, call. = FALSE)
  }
  if (!is.null(lake_file) && !file.exists(lake_file)) {
    stop("lake_file not found: ", lake_file, call. = FALSE)
  }

  if (is.null(gwflow_config)) {
    gwflow_config <- qswat_read_gwflow_config()
  }
  cell_size <- as.numeric(gwflow_config$cell_size)

  # ---- 1. Basin geometry -----------------------------------------------------
  basin_sf  <- sf::st_as_sf(sf::st_union(project$lsu_sf))
  proj_crs  <- sf::st_crs(basin_sf)

  # ---- 2. Fishnet grid -------------------------------------------------------
  message("Creating fishnet grid (cell size = ", cell_size, " m)...")
  grid_result <- .gw_make_fishnet(basin_sf, cell_size, proj_crs)
  grid_sf     <- grid_result$grid
  n_rows      <- grid_result$n_rows
  n_cols      <- grid_result$n_cols
  message("  Grid: ", n_rows, " rows x ", n_cols,
          " cols = ", nrow(grid_sf), " cells")

  # ---- 3. Active cells -------------------------------------------------------
  message("Identifying active cells...")
  grid_sf <- .gw_active_cells(grid_sf, basin_sf)

  # ---- 4. Aquifer thickness --------------------------------------------------
  message("Assigning aquifer thickness from raster...")
  grid_sf <- .gw_assign_thickness(grid_sf, thickness_file, thickness_scale,
                                   proj_crs)

  # ---- 5. Ground elevation ---------------------------------------------------
  if (!is.null(project$dem_file) && file.exists(project$dem_file)) {
    message("Assigning ground elevation from DEM...")
    grid_sf <- .gw_assign_elevation(grid_sf, project$dem_file, proj_crs)
  } else {
    grid_sf$Avg_elevat <- 0
  }

  # ---- 6. Conductivity zones -------------------------------------------------
  message("Assigning aquifer conductivity zones...")
  k_result <- .gw_assign_conductivity(grid_sf, basin_sf,
                                       conductivity_file, conductivity_column,
                                       proj_crs)
  grid_sf  <- k_result$grid
  zone_df  <- k_result$zones

  # ---- 7. Boundary cells -----------------------------------------------------
  message("Detecting boundary cells...")
  grid_sf <- .gw_detect_boundary(grid_sf, basin_sf)

  # ---- 8. Tile drain cells ---------------------------------------------------
  grid_sf$tile_cell <- 0L
  if (!is.null(tiles_file)) {
    message("Detecting tile drain cells...")
    grid_sf <- .gw_detect_tile_drains(grid_sf, tiles_file, proj_crs)
  }

  # ---- 9. Write to database --------------------------------------------------
  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (overwrite) {
    gis_tbls <- c("gwflow_rescell", "gwflow_hrucell", "gwflow_fpcell",
                  "gwflow_rivcell", "gwflow_lsucell", "gwflow_init_conc",
                  "gwflow_obs_locs", "gwflow_out_days",
                  "gwflow_grid", "gwflow_zone")
    for (tbl in gis_tbls) {
      if (DBI::dbExistsTable(con, tbl)) {
        DBI::dbExecute(con, paste0("DELETE FROM ", tbl))
      }
    }
  }

  message("Populating gwflow_zone...")
  .gw_write_zone(con, zone_df, gwflow_config)

  message("Populating gwflow_grid...")
  n_active <- .gw_write_grid(con, grid_sf, gwflow_config)
  message("  Inserted ", n_active, " active cells")

  message("Populating gwflow_rivcell...")
  .gw_write_rivcell(con, grid_sf, project, proj_crs)

  recharge <- gwflow_config$hruorlsu_recharge
  if (recharge >= 2L) {
    message("Populating gwflow_lsucell...")
    .gw_write_lsucell(con, grid_sf, project, proj_crs)
  }

  # gwflow_fpcell: rQSWATPlus has no floodplain/upslope landscape split,
  # so this table is intentionally left empty.

  if (recharge == 1L || recharge == 3L) {
    message("Populating gwflow_hrucell...")
    .gw_write_hrucell(con, grid_sf, project, proj_crs)
  }

  message("Populating gwflow_rescell...")
  .gw_write_rescell(con, project, lake_file, grid_sf, proj_crs)

  # Update gwflow_base with the actual row/col counts
  DBI::dbExecute(con,
    "UPDATE gwflow_base SET row_count = ?, col_count = ?",
    params = list(n_rows, n_cols)
  )

  message("gwflow GIS tables populated successfully.")
  project$gwflow_grid_sf <- grid_sf
  invisible(project)
}


# ---- fishnet -----------------------------------------------------------------

#' Create a Fishnet Grid over the Basin
#'
#' Replicates the Python `fishnet()` function: a regular square grid is created
#' over the bounding box of `basin_sf` using `cell_size` metre cells.  Cell IDs
#' are assigned in raster order (left-to-right, top-to-bottom).
#' @noRd
.gw_make_fishnet <- function(basin_sf, cell_size, proj_crs) {
  ext   <- sf::st_bbox(basin_sf)
  min_x <- ext[["xmin"]]
  min_y <- ext[["ymin"]]
  max_x <- ext[["xmax"]]
  max_y <- ext[["ymax"]]

  # Number of columns and rows (matching Python: ceil of range / size)
  n_cols <- ceiling((max_x - min_x) / cell_size)
  n_rows <- ceiling((max_y - min_y) / cell_size)

  # Create the grid; offset aligns the lower-left corner with the basin origin
  grid_geom <- sf::st_make_grid(
    x        = basin_sf,
    cellsize = c(cell_size, cell_size),
    offset   = c(min_x, min_y),
    what     = "polygons"
  )
  grid_sf <- sf::st_sf(geometry = grid_geom)
  sf::st_crs(grid_sf) <- proj_crs

  # Assign IDs in raster order: left-to-right, top-to-bottom (matching the
  # Python fishnet function).  sf::st_make_grid produces cells in column-major
  # order (bottom-to-top within each column, then left-to-right across
  # columns).  For cell k (0-indexed):
  #   column index  = k %/% n_rows
  #   row from top  = n_rows - 1 - (k %% n_rows)
  #   raster ID     = row_from_top * n_cols + col + 1
  k          <- seq_len(nrow(grid_sf)) - 1L
  col_idx    <- k %/% n_rows
  row_top    <- (n_rows - 1L) - (k %% n_rows)
  grid_sf$Id <- row_top * n_cols + col_idx + 1L

  list(
    grid   = grid_sf,
    n_rows = as.integer(n_rows),
    n_cols = as.integer(n_cols)
  )
}


# ---- active cells ------------------------------------------------------------

#' Flag Active Grid Cells
#'
#' Cells that intersect the basin polygon receive `Avg_active = 1`; all others
#' receive `Avg_active = 0`.  Matches the Python `activecells()` function.
#' @noRd
.gw_active_cells <- function(grid_sf, basin_sf) {
  hits          <- lengths(sf::st_intersects(grid_sf, basin_sf)) > 0L
  grid_sf$Avg_active <- as.integer(hits)
  grid_sf
}


# ---- aquifer thickness -------------------------------------------------------

#' Assign Mean Aquifer Thickness to Grid Cells
#'
#' Opens the thickness raster, fills NoData values with a focal mean (matching
#' Python's `gdal.FillNodata`), reprojects to the project CRS if necessary, and
#' extracts the mean value for each grid cell.  The result is scaled by
#' `thickness_scale` (default 0.01) to convert centimetres to metres.
#' @noRd
.gw_assign_thickness <- function(grid_sf, thickness_file, thickness_scale,
                                  proj_crs) {
  thick_rast <- terra::rast(thickness_file)

  # Fill NoData holes with a focal mean, replicating Python's
  # gdal.FillNodata(maxSearchDist = 5).  A first pass with a 3×3 window fills
  # isolated single-pixel gaps; a second pass with a 5×5 window covers larger
  # holes up to ~2 pixels from valid data, matching the search distance of 5.
  thick_rast <- terra::focal(thick_rast, w = 3, fun = mean,
                               na.rm = TRUE, na.policy = "only")
  thick_rast <- terra::focal(thick_rast, w = 5, fun = mean,
                               na.rm = TRUE, na.policy = "only")

  # Reproject raster to match grid CRS when they differ.
  # Always call terra::project with the target WKT; terra handles the no-op
  # case when source and target CRS are already identical.
  grid_wkt <- proj_crs$wkt
  if (!is.null(grid_wkt) && nzchar(grid_wkt)) {
    tryCatch(
      thick_rast <- terra::project(thick_rast, grid_wkt),
      error = function(e)
        message("Note: could not reproject thickness raster: ",
                conditionMessage(e))
    )
  }

  # Extract mean value per grid-cell polygon
  grid_v   <- terra::vect(grid_sf)
  extr     <- terra::extract(thick_rast, grid_v,
                              fun = mean, na.rm = TRUE, ID = FALSE)
  vals     <- extr[, 1] * thickness_scale
  vals[is.na(vals)] <- 0
  grid_sf$Avg_Thick <- vals
  grid_sf
}


# ---- ground elevation --------------------------------------------------------

#' Assign Mean Ground Elevation to Grid Cells
#'
#' Extracts the mean DEM value for each grid cell polygon, replicating the
#' Python `aquif_elevation()` function (which polygonises a coarse-resolution
#' DEM and spatially joins the result with the grid).
#' @noRd
.gw_assign_elevation <- function(grid_sf, dem_file, proj_crs) {
  dem_rast <- terra::rast(dem_file)

  # Reproject raster to match grid CRS when they differ.
  grid_wkt <- proj_crs$wkt
  if (!is.null(grid_wkt) && nzchar(grid_wkt)) {
    tryCatch(
      dem_rast <- terra::project(dem_rast, grid_wkt),
      error = function(e)
        message("Note: could not reproject DEM: ", conditionMessage(e))
    )
  }

  grid_v <- terra::vect(grid_sf)
  extr   <- terra::extract(dem_rast, grid_v,
                            fun = mean, na.rm = TRUE, ID = FALSE)
  vals   <- extr[, 1]
  vals[is.na(vals)] <- 0
  grid_sf$Avg_elevat <- vals
  grid_sf
}


# ---- conductivity zones ------------------------------------------------------

#' Assign Conductivity Zones to Grid Cells
#'
#' Reads the conductivity shapefile, optionally converts GLHYMPS log-K values
#' to m/day, clips polygons to the basin, assigns sequential zone IDs, and
#' performs a left spatial join to assign each grid cell to a zone (taking the
#' mean zone ID when a cell overlaps multiple zones, matching Python).
#'
#' Returns a list with `grid` (updated `grid_sf`) and `zones` (data frame with
#' zone_id and K_mday).
#' @noRd
.gw_assign_conductivity <- function(grid_sf, basin_sf,
                                     conductivity_file, conductivity_column,
                                     proj_crs) {
  k_sf <- sf::st_read(conductivity_file, quiet = TRUE)
  k_sf <- sf::st_transform(k_sf, proj_crs)
  k_sf <- sf::st_make_valid(k_sf)

  # Auto-detect GLHYMPS format
  if (is.null(conductivity_column)) {
    if ("logK_Ferr_" %in% names(k_sf)) {
      # logK_Ferr_ = log10(intrinsic permeability [m^2]) * 100 (stored as int)
      # Hydraulic conductivity in m/day:
      #   K = k_intrinsic * rho * g / mu * 86400
      #   where rho = 1000 kg/m^3, g = 9.81 m/s^2, mu = 0.001 Pa.s
      log_k       <- k_sf$logK_Ferr_ / 100  # log10(m^2)
      k_intrinsic <- 10^log_k              # m^2
      k_sf$K_mday <- k_intrinsic * 1000 * 9.81 / 0.001 * 86400
      conductivity_column <- "K_mday"
    } else {
      stop(
        "Cannot auto-detect conductivity format. ",
        "Supply 'conductivity_column' with K values in m/day, ",
        "or use a GLHYMPS file containing a 'logK_Ferr_' column.",
        call. = FALSE
      )
    }
  }

  if (!conductivity_column %in% names(k_sf)) {
    stop("Column '", conductivity_column, "' not found in conductivity_file.",
         call. = FALSE)
  }

  # Clip conductivity polygons to basin
  basin_valid <- sf::st_make_valid(basin_sf)
  k_clipped   <- tryCatch(
    sf::st_intersection(k_sf, basin_valid),
    error = function(e) {
      message("Note: conductivity clip failed (", conditionMessage(e),
              "); using unclipped polygons.")
      k_sf
    }
  )
  k_clipped <- k_clipped[!sf::st_is_empty(sf::st_geometry(k_clipped)), ]

  if (nrow(k_clipped) == 0L) {
    warning("No conductivity features intersect the basin. ",
            "gwflow_zone and zone assignments will be empty.", call. = FALSE)
    grid_sf$zone <- 0L
    return(list(
      grid  = grid_sf,
      zones = data.frame(zone_id = integer(0), K_mday = numeric(0))
    ))
  }

  # Assign sequential zone IDs (matching Python K_gdf_zones['zone'])
  k_clipped$zone <- seq_len(nrow(k_clipped))

  # Spatial join: assign zone to each grid cell; keep only Id and zone columns
  joined    <- sf::st_join(
    grid_sf[, "Id"],
    k_clipped[, c("zone", conductivity_column)],
    join = sf::st_intersects,
    left = TRUE
  )
  joined_df <- as.data.frame(joined)[, c("Id", "zone"), drop = FALSE]
  joined_df <- joined_df[!is.na(joined_df$zone), , drop = FALSE]

  # For cells with multiple zone matches, take the mean (matching Python
  # groupby('Id').mean()) and round to integer
  if (nrow(joined_df) > 0L) {
    zone_agg <- stats::aggregate(zone ~ Id, data = joined_df,
                                  FUN = function(x) as.integer(round(mean(x))))
    grid_sf$zone <- 0L
    idx <- match(grid_sf$Id, zone_agg$Id)
    grid_sf$zone[!is.na(idx)] <- zone_agg$zone[idx[!is.na(idx)]]
  } else {
    grid_sf$zone <- 0L
  }

  # Build zone summary data frame
  zone_df <- data.frame(
    zone_id = k_clipped$zone,
    K_mday  = as.numeric(k_clipped[[conductivity_column]]),
    stringsAsFactors = FALSE
  )

  list(grid = grid_sf, zones = zone_df)
}


# ---- boundary cells ----------------------------------------------------------

#' Detect Boundary Grid Cells
#'
#' Grid cells that intersect the exterior boundary line of the basin polygon
#' receive `boundary = 1`; all others receive `boundary = 0`.  Matches the
#' Python approach: `borders_gdf['geometry'] = borders_gdf.boundary`.
#' @noRd
.gw_detect_boundary <- function(grid_sf, basin_sf) {
  # Convert basin polygon to its boundary linestring(s)
  boundary_geom <- sf::st_cast(sf::st_geometry(sf::st_union(basin_sf)),
                                 "MULTILINESTRING")
  boundary_sf   <- sf::st_sf(data.frame(boundary = 1L),
                              geometry = boundary_geom)

  hits              <- lengths(sf::st_intersects(grid_sf, boundary_sf)) > 0L
  grid_sf$boundary  <- as.integer(hits)
  grid_sf
}


# ---- tile drain cells --------------------------------------------------------

#' Detect Tile-Drain Grid Cells
#'
#' Grid cells that intersect the tile-drain shapefile receive `tile_cell = 1`.
#' @noRd
.gw_detect_tile_drains <- function(grid_sf, tiles_file, proj_crs) {
  tiles_sf <- tryCatch(
    {
      t <- sf::st_read(tiles_file, quiet = TRUE)
      sf::st_transform(t, proj_crs)
    },
    error = function(e) {
      message("Note: could not read tiles_file (", conditionMessage(e),
              "); tile cells set to 0.")
      NULL
    }
  )
  if (is.null(tiles_sf)) return(grid_sf)

  hits             <- lengths(sf::st_intersects(grid_sf, tiles_sf)) > 0L
  grid_sf$tile_cell <- as.integer(hits)
  grid_sf
}


# ---- database write helpers --------------------------------------------------

#' Write gwflow_zone Table
#'
#' Inserts one row per conductivity zone with K, specific yield, streambed K,
#' and streambed thickness from the gwflow config.
#' @noRd
.gw_write_zone <- function(con, zone_df, gwflow_config) {
  if (nrow(zone_df) == 0L) return(invisible(NULL))

  df <- data.frame(
    zone_id             = zone_df$zone_id,
    aquifer_k           = zone_df$K_mday,
    specific_yield      = gwflow_config$init_sy,
    streambed_k         = gwflow_config$streambed_k,
    streambed_thickness = gwflow_config$streambed_thick,
    stringsAsFactors    = FALSE
  )
  DBI::dbWriteTable(con, "gwflow_zone", df, append = TRUE, row.names = FALSE)
}


#' Write gwflow_grid Table
#'
#' Inserts one row for every active cell (status > 0), matching the Python
#' loop over grid6_gdf where `status = Avg_active + boundary`.
#' @noRd
.gw_write_grid <- function(con, grid_sf, gwflow_config) {
  gdf              <- as.data.frame(grid_sf)
  gdf$status       <- gdf$Avg_active + gdf$boundary
  active           <- gdf[gdf$status > 0L, , drop = FALSE]

  if (nrow(active) == 0L) {
    message("  Warning: no active cells found; gwflow_grid is empty.")
    return(0L)
  }

  df <- data.frame(
    cell_id           = active$Id,
    status            = active$status,
    zone              = active$zone,
    elevation         = active$Avg_elevat,
    aquifer_thickness = active$Avg_Thick,
    extinction_depth  = gwflow_config$exdp,
    initial_head      = active$Avg_elevat - gwflow_config$wt_depth,
    tile              = active$tile_cell,
    stringsAsFactors  = FALSE
  )
  DBI::dbWriteTable(con, "gwflow_grid", df, append = TRUE, row.names = FALSE)
  nrow(df)
}


#' Write gwflow_rivcell Table
#'
#' Intersects the project stream network with the fishnet grid cells and
#' inserts the length of stream within each cell.  Matches the Python code:
#' `river_cell_gdf['riv_length'] = river_cell_gdf['geometry'].length`.
#' @noRd
.gw_write_rivcell <- function(con, grid_sf, project, proj_crs) {
  streams_sf <- project$streams_sf
  if (is.null(streams_sf) || nrow(streams_sf) == 0L) {
    message("  No stream network available; gwflow_rivcell not populated.")
    return(invisible(NULL))
  }

  # Look up gis_channels IDs (subbasin → channel id)
  channels_db <- DBI::dbGetQuery(con, "SELECT id, subbasin FROM gis_channels")
  if (nrow(channels_db) == 0L) {
    message("  gis_channels is empty; gwflow_rivcell not populated.")
    return(invisible(NULL))
  }

  streams_proj <- sf::st_transform(streams_sf, proj_crs)

  # Map each stream feature to its gis_channels.id via WSNO = subbasin
  wsno_col <- if ("WSNO" %in% names(streams_proj)) "WSNO" else NULL
  if (is.null(wsno_col)) {
    message("  streams_sf has no WSNO column; gwflow_rivcell not populated.")
    return(invisible(NULL))
  }
  streams_proj$channel_db_id <- channels_db$id[
    match(streams_proj[[wsno_col]], channels_db$subbasin)
  ]
  streams_proj <- streams_proj[!is.na(streams_proj$channel_db_id), ]
  if (nrow(streams_proj) == 0L) return(invisible(NULL))

  # Intersect stream lines with grid cells; the result contains line segments
  # within each cell with their grid Id attribute attached.
  inter <- tryCatch(
    sf::st_intersection(
      streams_proj[, "channel_db_id"],
      grid_sf[, "Id"]
    ),
    error = function(e) {
      message("  Stream-grid intersection failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(inter) || nrow(inter) == 0L) return(invisible(NULL))

  # Keep only line-like geometries and calculate length
  geom_types <- as.character(sf::st_geometry_type(inter))
  inter      <- inter[grepl("LINE", geom_types, ignore.case = TRUE), ]
  if (nrow(inter) == 0L) return(invisible(NULL))

  inter$length_m <- as.numeric(sf::st_length(inter))

  inter_df <- as.data.frame(inter)[, c("Id", "channel_db_id", "length_m"),
                                    drop = FALSE]
  # Aggregate: sum length per (cell_id, channel) pair
  result <- stats::aggregate(
    length_m ~ Id + channel_db_id,
    data = inter_df,
    FUN  = sum
  )

  df <- data.frame(
    cell_id  = result$Id,
    channel  = result$channel_db_id,
    length_m = result$length_m,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "gwflow_rivcell", df, append = TRUE, row.names = FALSE)
  message("  Inserted ", nrow(df), " rows into gwflow_rivcell")
  invisible(NULL)
}


#' Write gwflow_lsucell Table
#'
#' Intersects LSU (landscape-unit) polygons with grid cells and inserts the
#' intersection area.  Matches the Python code:
#' `lsu_intersection['Poly_area'] = lsu_intersection['geometry'].area`.
#' Only populated when `hruorlsu_recharge >= 2`.
#' @noRd
.gw_write_lsucell <- function(con, grid_sf, project, proj_crs) {
  lsus_sf <- project$lsu_sf
  if (is.null(lsus_sf) || nrow(lsus_sf) == 0L) {
    message("  No LSU polygons available; gwflow_lsucell not populated.")
    return(invisible(NULL))
  }

  # Map subbasin → gis_lsus.id
  lsus_db <- DBI::dbGetQuery(con, "SELECT id, subbasin FROM gis_lsus")
  if (nrow(lsus_db) == 0L) {
    message("  gis_lsus is empty; gwflow_lsucell not populated.")
    return(invisible(NULL))
  }

  lsus_proj <- sf::st_transform(lsus_sf, proj_crs)
  lsus_proj <- sf::st_make_valid(lsus_proj)

  lsus_proj$lsu_db_id <- lsus_db$id[
    match(lsus_proj$subbasin, lsus_db$subbasin)
  ]
  lsus_proj <- lsus_proj[!is.na(lsus_proj$lsu_db_id), ]
  if (nrow(lsus_proj) == 0L) return(invisible(NULL))

  inter <- tryCatch(
    sf::st_intersection(lsus_proj[, "lsu_db_id"], grid_sf[, "Id"]),
    error = function(e) {
      message("  LSU-grid intersection failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(inter) || nrow(inter) == 0L) return(invisible(NULL))

  inter$area_m2 <- as.numeric(sf::st_area(inter))
  inter_df <- as.data.frame(inter)[, c("Id", "lsu_db_id", "area_m2"),
                                    drop = FALSE]

  result <- stats::aggregate(area_m2 ~ Id + lsu_db_id, data = inter_df,
                              FUN = sum)
  df <- data.frame(
    cell_id = result$Id,
    lsu     = result$lsu_db_id,
    area_m2 = result$area_m2,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "gwflow_lsucell", df, append = TRUE, row.names = FALSE)
  message("  Inserted ", nrow(df), " rows into gwflow_lsucell")
  invisible(NULL)
}


#' Write gwflow_hrucell Table
#'
#' Intersects HRU polygons with grid cells and inserts the intersection area
#' per (cell, HRU) pair.  Uses `project$hru_sf` which already covers the full
#' watershed (eliminated HRUs are reassigned to the dominant HRU during HRU
#' creation), making the complex area-redistribution logic in the Python
#' `makeHRUData()` unnecessary.
#' @noRd
.gw_write_hrucell <- function(con, grid_sf, project, proj_crs) {
  hru_sf <- project$hru_sf
  if (is.null(hru_sf) || nrow(hru_sf) == 0L) {
    message("  No HRU polygons available; gwflow_hrucell not populated.")
    return(invisible(NULL))
  }
  if (!"hru_id" %in% names(hru_sf)) {
    message("  hru_sf has no hru_id column; gwflow_hrucell not populated.")
    return(invisible(NULL))
  }

  hru_proj <- sf::st_transform(hru_sf, proj_crs)
  hru_proj <- sf::st_make_valid(hru_proj)

  inter <- tryCatch(
    sf::st_intersection(hru_proj[, "hru_id"], grid_sf[, "Id"]),
    error = function(e) {
      message("  HRU-grid intersection failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(inter) || nrow(inter) == 0L) return(invisible(NULL))

  inter$area_m2 <- as.numeric(sf::st_area(inter))
  inter_df <- as.data.frame(inter)[, c("Id", "hru_id", "area_m2"),
                                    drop = FALSE]

  result <- stats::aggregate(area_m2 ~ Id + hru_id, data = inter_df,
                              FUN = sum)
  df <- data.frame(
    cell_id = result$Id,
    hru     = result$hru_id,
    area_m2 = result$area_m2,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "gwflow_hrucell", df, append = TRUE, row.names = FALSE)
  message("  Inserted ", nrow(df), " rows into gwflow_hrucell")
  invisible(NULL)
}


#' Write gwflow_rescell Table
#'
#' Populates reservoir-cell connections from two sources, matching the Python
#' code:
#' 1. Cells whose LSU contains a water body in `gis_water` (joined via
#'    `gwflow_lsucell`).
#' 2. Cells that intersect the optional `lake_file` polygon shapefile.
#' @noRd
.gw_write_rescell <- function(con, project, lake_file, grid_sf, proj_crs) {

  # Source 1: gis_water INNER JOIN gwflow_lsucell (matching Python)
  res_from_lsu <- tryCatch(
    DBI::dbGetQuery(con,
      "SELECT gwflow_lsucell.cell_id, gis_water.id, gis_water.elev
       FROM gis_water
       INNER JOIN gwflow_lsucell ON gis_water.lsu = gwflow_lsucell.lsu"
    ),
    error = function(e) data.frame()
  )

  # Source 2: lake polygon intersections (if lake_file is supplied)
  lake_df <- NULL
  if (!is.null(lake_file)) {
    lakes_sf <- tryCatch(
      {
        l <- sf::st_read(lake_file, quiet = TRUE)
        sf::st_transform(sf::st_make_valid(l), proj_crs)
      },
      error = function(e) {
        message("  Could not read lake_file: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(lakes_sf) && nrow(lakes_sf) > 0L) {
      # Identify lake ID column (look for common names used by QGIS plugin)
      id_col <- intersect(c("LakeId", "lake_id", "id", "ID"), names(lakes_sf))[1]
      if (is.na(id_col)) id_col <- names(lakes_sf)[1]

      # Use elevation column if available, otherwise default to 0
      elev_col <- intersect(c("elev", "elevation", "ELEV"), names(lakes_sf))[1]

      inter <- tryCatch(
        sf::st_intersection(lakes_sf, grid_sf[, "Id"]),
        error = function(e) NULL
      )
      if (!is.null(inter) && nrow(inter) > 0L) {
        lake_id_vals  <- as.integer(as.data.frame(inter)[[id_col]])
        lake_elev_vals <- if (!is.na(elev_col))
          as.numeric(as.data.frame(inter)[[elev_col]])
        else
          rep(0, nrow(inter))

        lake_df <- data.frame(
          cell_id   = as.data.frame(inter)$Id,
          id        = lake_id_vals,
          elev      = lake_elev_vals,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Combine sources and deduplicate
  rows <- list()
  if (nrow(res_from_lsu) > 0L) {
    names(res_from_lsu) <- c("cell_id", "id", "elev")
    rows[["lsu"]] <- res_from_lsu
  }
  if (!is.null(lake_df) && nrow(lake_df) > 0L) {
    rows[["lake"]] <- lake_df
  }
  if (length(rows) == 0L) return(invisible(NULL))

  combined <- unique(do.call(rbind, rows))
  df <- data.frame(
    cell_id   = combined$cell_id,
    res       = combined$id,
    res_stage = combined$elev,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "gwflow_rescell", df, append = TRUE, row.names = FALSE)
  message("  Inserted ", nrow(df), " rows into gwflow_rescell")
  invisible(NULL)
}
