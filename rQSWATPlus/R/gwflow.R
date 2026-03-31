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
      transport_steps    REAL,
      disp_coef          REAL,
      recharge_delay     INTEGER,
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
