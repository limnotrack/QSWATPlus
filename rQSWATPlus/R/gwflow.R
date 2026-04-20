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
#' them from the supplied configuration and available spatial data.
#' This is the R equivalent of the gwflow setup performed by
#' `GWFlow.createTables()` and the GIS routines in the QGIS plugin's
#' `gwflow.py`.
#'
#' The function mirrors the SWAT+ gwflow database schema defined in the
#' Python plugin: `gwflow_base`, `gwflow_zone`, `gwflow_grid`,
#' `gwflow_out_days`, `gwflow_obs_locs`, `gwflow_solutes`,
#' `gwflow_init_conc`, `gwflow_hrucell`, `gwflow_fpcell`,
#' `gwflow_rivcell`, `gwflow_lsucell`, `gwflow_rescell`.
#'
#' When the project object contains spatial data (`lsu_sf`, `hru_sf`,
#' `channels_sf` or `streams_sf`) the function also runs the GIS
#' processing steps to populate the grid and cell connection tables.
#' This happens automatically when called from [qswat_run()] with
#' `use_gwflow = TRUE`.
#'
#' @param project A `qswat_project` object returned by
#'   [qswat_write_database()].
#' @param gwflow_config A named list of gwflow settings as returned by
#'   [qswat_read_gwflow_config()].  If `NULL` the bundled `gwflow.ini`
#'   defaults are used.
#' @param overwrite Logical.  If `TRUE`, drop and recreate existing gwflow
#'   tables.  Default `FALSE`.
#' @param aquifer_thickness Numeric (constant thickness in metres,
#'   default `20`) **or** a file path to a raster (`.tif`) or polygon
#'   shapefile (`.shp`) containing aquifer thickness values.  When a
#'   shapefile is provided it must have a numeric attribute named
#'   `"thickness"` or `"Avg_Thick"`.  When a raster is provided the
#'   mean cell value (in metres) is extracted for each grid cell.
#' @param conductivity_file Optional path to a polygon shapefile that
#'   defines aquifer hydraulic-conductivity zones.  The shapefile must
#'   contain a numeric column `"aquifer_k"` (K in m/day) or
#'   `"logK_Ferr_"` (log10 intrinsic permeability × 100 as stored in
#'   GLHYMPS).  If `NULL` a single default zone is created using
#'   `default_aquifer_k`.
#' @param default_aquifer_k Numeric.  Default hydraulic conductivity
#'   (m/day) for the single zone created when `conductivity_file` is
#'   `NULL`.  Default `1.0`.
#'
#' @return The `project` object with `project$use_gwflow` set to `TRUE`
#'   (invisibly).
#'
#' @details
#' The following tables are created and, when spatial data are
#' available, populated:
#' \describe{
#'   \item{gwflow_base}{Single-row table with global gwflow parameters
#'     (always populated; `row_count`/`col_count` updated after grid
#'     creation).}
#'   \item{gwflow_zone}{Aquifer hydraulic parameter zones (one default
#'     zone, or one row per zone from `conductivity_file`).}
#'   \item{gwflow_grid}{Active grid cells with elevation, aquifer
#'     thickness, initial head, and zone assignment.}
#'   \item{gwflow_out_days}{Optional output day pairs (not populated
#'     automatically; left empty for user entry).}
#'   \item{gwflow_obs_locs}{Observation cell IDs (not populated
#'     automatically; left empty for user entry).}
#'   \item{gwflow_solutes}{Chemical solute definitions and initial
#'     concentrations (always populated with 10 default solutes).}
#'   \item{gwflow_init_conc}{Per-cell initial solute concentrations
#'     (not populated automatically).}
#'   \item{gwflow_hrucell}{HRU-to-cell area mapping (populated when
#'     HRU recharge mode is selected and `hru_sf` is available).}
#'   \item{gwflow_fpcell}{Floodplain-cell connections (empty in
#'     rQSWATPlus because LSUs are 1:1 with subbasins, with no
#'     floodplain landscape units).}
#'   \item{gwflow_rivcell}{River-cell connections (channel-cell
#'     intersection lengths).}
#'   \item{gwflow_lsucell}{LSU-cell area mapping.}
#'   \item{gwflow_rescell}{Reservoir-cell connections (populated via
#'     the `gis_water`–`gwflow_lsucell` join when water bodies are
#'     present).}
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
                                overwrite = FALSE,
                                aquifer_thickness = 20.0,
                                conductivity_file = NULL,
                                default_aquifer_k = 1.0) {

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

  # Populate spatial tables (grid, zones, cell connections) when spatial
  # data from qswat_create_hrus() / qswat_create_streams() is present.
  if (!is.null(project$lsu_sf) && inherits(project$lsu_sf, "sf") &&
      nrow(project$lsu_sf) > 0) {
    .populate_gwflow_tables(
      con               = con,
      project           = project,
      cfg               = gwflow_config,
      aquifer_thickness = aquifer_thickness,
      conductivity_file = conductivity_file,
      default_aquifer_k = default_aquifer_k
    )
  } else {
    message("Note: No spatial data in project object; gwflow_zone, ",
            "gwflow_grid, gwflow_rivcell, gwflow_lsucell, and ",
            "gwflow_hrucell remain empty.")
    message("  Run qswat_setup_gwflow() after qswat_run() to populate them.")
  }

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


#' Populate gwflow Spatial Tables from Project GIS Data
#'
#' Runs the GIS processing steps that mirror the Python gwflow.py routines:
#' fishnet grid creation, active-cell detection, elevation extraction,
#' conductivity zone assignment, and spatial intersections with channels,
#' LSUs, and (optionally) HRUs and water bodies.
#'
#' @param con  DBI connection to the project SQLite database.
#' @param project `qswat_project` object with `lsu_sf`, `hru_sf`,
#'   `channels_sf`/`streams_sf`, and `dem_file` populated.
#' @param cfg  gwflow configuration list from [qswat_read_gwflow_config()].
#' @param aquifer_thickness Numeric constant (metres) or file path.
#' @param conductivity_file Path to conductivity shapefile, or `NULL`.
#' @param default_aquifer_k Default K in m/day when no file is given.
#' @noRd
.populate_gwflow_tables <- function(con, project, cfg,
                                     aquifer_thickness = 20.0,
                                     conductivity_file = NULL,
                                     default_aquifer_k = 1.0) {

  watershed_sf <- project$lsu_sf
  crs_obj      <- sf::st_crs(watershed_sf)
  cell_size    <- cfg$cell_size

  # ------------------------------------------------------------------
  # 1. Build fishnet grid over the watershed bounding box
  # ------------------------------------------------------------------
  message("  [gwflow] Creating fishnet grid (cell_size = ", cell_size, " m)...")
  bbox_obj <- sf::st_bbox(watershed_sf)

  grid_geom <- sf::st_make_grid(
    watershed_sf,
    cellsize = cell_size,
    what     = "polygons"
  )
  grid_sf <- sf::st_sf(
    cell_id  = seq_along(grid_geom),
    geometry = grid_geom,
    crs      = crs_obj
  )

  # Compute grid dimensions for gwflow_base
  n_cols <- ceiling((bbox_obj["xmax"] - bbox_obj["xmin"]) / cell_size)
  n_rows <- ceiling((bbox_obj["ymax"] - bbox_obj["ymin"]) / cell_size)

  # ------------------------------------------------------------------
  # 2. Identify active cells (those that intersect the watershed)
  # ------------------------------------------------------------------
  message("  [gwflow] Identifying active cells...")
  watershed_union <- sf::st_union(watershed_sf)
  hits <- which(lengths(sf::st_intersects(grid_sf, watershed_union)) > 0)
  if (length(hits) == 0) {
    message("  [gwflow] Warning: no grid cells intersect the watershed.")
    return(invisible(NULL))
  }
  grid_active <- grid_sf[hits, , drop = FALSE]

  # ------------------------------------------------------------------
  # 3. Assign elevation from DEM
  # ------------------------------------------------------------------
  message("  [gwflow] Extracting DEM elevation per cell...")
  cell_elev <- rep(0.0, nrow(grid_active))
  if (!is.null(project$dem_file) && file.exists(project$dem_file)) {
    dem_rast <- terra::rast(project$dem_file)
    # Project grid to DEM CRS for extraction
    grid_v_dem <- terra::vect(
      sf::st_transform(grid_active, crs = terra::crs(dem_rast))
    )
    ex <- terra::extract(dem_rast, grid_v_dem, fun = "mean", na.rm = TRUE)
    if (ncol(ex) >= 2) {
      vals <- ex[, 2]
      vals[is.na(vals)] <- 0.0
      cell_elev <- as.numeric(vals)
    }
  }

  # ------------------------------------------------------------------
  # 4. Assign aquifer thickness
  # ------------------------------------------------------------------
  message("  [gwflow] Assigning aquifer thickness...")
  cell_thick <- rep(as.numeric(aquifer_thickness[1]), nrow(grid_active))

  if (is.character(aquifer_thickness) && file.exists(aquifer_thickness)) {
    ext <- tolower(tools::file_ext(aquifer_thickness))
    if (ext %in% c("tif", "tiff", "img")) {
      # Raster source
      thick_rast <- terra::rast(aquifer_thickness)
      grid_v_aq  <- terra::vect(
        sf::st_transform(grid_active, crs = terra::crs(thick_rast))
      )
      ex_t <- terra::extract(thick_rast, grid_v_aq, fun = "mean", na.rm = TRUE)
      if (ncol(ex_t) >= 2) {
        vals <- ex_t[, 2]
        vals[is.na(vals) | vals <= 0] <- as.numeric(aquifer_thickness[1])
        cell_thick <- as.numeric(vals)
      }
    } else if (ext %in% c("shp", "gpkg", "geojson")) {
      # Vector source: spatial join and average
      thick_sf <- sf::st_read(aquifer_thickness, quiet = TRUE)
      thick_sf  <- sf::st_transform(thick_sf, crs = crs_obj)
      # Detect the thickness column
      t_col <- intersect(c("thickness", "Avg_Thick"),
                         names(thick_sf))[1]
      if (!is.na(t_col)) {
        grid_j <- sf::st_join(
          grid_active[, "cell_id"],
          thick_sf[, c(t_col, "geometry")],
          join = sf::st_intersects
        )
        t_agg <- stats::aggregate(
          grid_j[[t_col]],
          by  = list(cell_id = grid_j$cell_id),
          FUN = function(x) mean(x[x > 0], na.rm = TRUE)
        )
        idx <- match(grid_active$cell_id, t_agg$cell_id)
        vals <- t_agg$x[idx]
        vals[is.na(vals) | vals <= 0] <- as.numeric(aquifer_thickness[1])
        cell_thick <- as.numeric(vals)
      }
    }
  }

  # ------------------------------------------------------------------
  # 5. Assign conductivity zones
  # ------------------------------------------------------------------
  message("  [gwflow] Assigning conductivity zones...")
  cell_zone <- rep(1L, nrow(grid_active))

  if (!is.null(conductivity_file) && file.exists(conductivity_file)) {
    k_sf <- sf::st_read(conductivity_file, quiet = TRUE)
    k_sf <- sf::st_transform(k_sf, crs = crs_obj)

    # Detect the K column
    if ("aquifer_k" %in% names(k_sf)) {
      k_col <- "aquifer_k"
    } else if ("logK_Ferr_" %in% names(k_sf)) {
      # GLHYMPS convention: logK_Ferr_ is logK * 100 → convert to m/day
      k_sf$aquifer_k <- (10^(k_sf$logK_Ferr_ / 100)) * 1000 * 9.81 / 0.001 * 86400
      k_col <- "aquifer_k"
    } else {
      message("  [gwflow] Warning: conductivity shapefile has no 'aquifer_k' ",
              "or 'logK_Ferr_' column; using single default zone.")
      k_col <- NULL
    }

    if (!is.null(k_col)) {
      # Assign sequential zone IDs to conductivity polygons
      k_sf$zone_id <- seq_len(nrow(k_sf))

      # Spatial join: assign zone to each active cell (take first match)
      grid_j <- sf::st_join(
        grid_active[, "cell_id"],
        k_sf[, c("zone_id", k_col, "geometry")],
        join = sf::st_intersects
      )
      # If a cell hits multiple zones take the one with the smallest zone_id
      z_agg <- stats::aggregate(
        grid_j$zone_id,
        by  = list(cell_id = grid_j$cell_id),
        FUN = function(x) min(x, na.rm = TRUE)
      )
      idx <- match(grid_active$cell_id, z_agg$cell_id)
      zone_vals <- z_agg$x[idx]
      zone_vals[is.na(zone_vals)] <- 1L
      cell_zone <- as.integer(zone_vals)

      # Build zone table
      k_agg <- stats::aggregate(
        k_sf[[k_col]],
        by  = list(zone_id = k_sf$zone_id),
        FUN = function(x) mean(x, na.rm = TRUE)
      )
      zone_df <- data.frame(
        zone_id             = k_agg$zone_id,
        aquifer_k           = k_agg$x,
        specific_yield      = cfg$init_sy,
        streambed_k         = cfg$streambed_k,
        streambed_thickness = cfg$streambed_thick,
        stringsAsFactors    = FALSE
      )
    } else {
      # No usable K column → fall through to single default zone below
      conductivity_file <- NULL
    }
  }

  if (is.null(conductivity_file)) {
    # Single default zone
    zone_df <- data.frame(
      zone_id            = 1L,
      aquifer_k          = default_aquifer_k,
      specific_yield     = cfg$init_sy,
      streambed_k        = cfg$streambed_k,
      streambed_thickness = cfg$streambed_thick,
      stringsAsFactors   = FALSE
    )
  }

  # ------------------------------------------------------------------
  # 6. Detect boundary cells
  # ------------------------------------------------------------------
  message("  [gwflow] Detecting boundary cells...")
  boundary_line <- sf::st_cast(
    sf::st_boundary(watershed_union), "MULTILINESTRING"
  )
  is_boundary <- as.integer(
    lengths(sf::st_intersects(grid_active, boundary_line)) > 0
  )

  # status = 1 (active interior) or 2 (active + boundary)
  cell_status <- 1L + is_boundary

  # ------------------------------------------------------------------
  # 7. Write gwflow_zone
  # ------------------------------------------------------------------
  message("  [gwflow] Writing gwflow_zone (", nrow(zone_df), " zone(s))...")
  DBI::dbWriteTable(con, "gwflow_zone", zone_df,
                    append = TRUE, row.names = FALSE)

  # ------------------------------------------------------------------
  # 8. Write gwflow_grid (active cells only, mirroring Python status logic)
  # ------------------------------------------------------------------
  message("  [gwflow] Writing gwflow_grid (", nrow(grid_active),
          " active cells)...")
  grid_df <- data.frame(
    cell_id           = grid_active$cell_id,
    status            = cell_status,
    zone              = cell_zone,
    elevation         = cell_elev,
    aquifer_thickness = cell_thick,
    extinction_depth  = cfg$exdp,
    initial_head      = cell_elev - cfg$wt_depth,
    tile              = 0L,
    stringsAsFactors  = FALSE
  )
  DBI::dbWriteTable(con, "gwflow_grid", grid_df,
                    append = TRUE, row.names = FALSE)

  # Update gwflow_base row/col counts
  DBI::dbExecute(
    con,
    sprintf("UPDATE gwflow_base SET row_count = %d, col_count = %d",
            as.integer(n_rows), as.integer(n_cols))
  )

  # ------------------------------------------------------------------
  # 9. gwflow_rivcell: channel–cell intersections
  # ------------------------------------------------------------------
  channels_sf <- project$channels_sf
  if (is.null(channels_sf) && !is.null(project$streams_sf)) {
    channels_sf <- project$streams_sf
  }

  if (!is.null(channels_sf) && inherits(channels_sf, "sf") &&
      nrow(channels_sf) > 0) {
    message("  [gwflow] Computing channel–cell intersections...")

    tryCatch({
      # Ensure matching CRS
      channels_sf <- sf::st_transform(channels_sf, crs = crs_obj)

      # Read gis_channels id / subbasin mapping from DB
      gis_ch <- DBI::dbGetQuery(con, "SELECT id, subbasin FROM gis_channels")

      # Intersect: keep only grid cells that a channel runs through
      # Build a minimal sf with just WSNO (= subbasin ID) and geometry
      wsno_vals <- if ("WSNO" %in% names(channels_sf)) channels_sf$WSNO
                   else if ("LINKNO" %in% names(channels_sf)) channels_sf$LINKNO
                   else seq_len(nrow(channels_sf))

      channels_work <- sf::st_sf(
        WSNO     = wsno_vals,
        geometry = sf::st_geometry(channels_sf),
        crs      = crs_obj
      )

      riv_inter <- sf::st_intersection(
        channels_work,
        grid_active[, c("cell_id", "geometry")]
      )

      if (nrow(riv_inter) > 0) {
        # Only keep line/multiline geometries (the channel segments)
        keep <- sf::st_geometry_type(riv_inter) %in%
          c("LINESTRING", "MULTILINESTRING")
        riv_inter <- riv_inter[keep, , drop = FALSE]
      }

      if (nrow(riv_inter) > 0) {
        riv_inter$length_m <- as.numeric(sf::st_length(riv_inter))

        # Map WSNO → gis_channels.id
        ch_id <- gis_ch$id[match(riv_inter$WSNO, gis_ch$subbasin)]

        riv_df <- data.frame(
          cell_id  = riv_inter$cell_id,
          channel  = ch_id,
          length_m = riv_inter$length_m,
          stringsAsFactors = FALSE
        )
        riv_df <- riv_df[!is.na(riv_df$channel) & riv_df$length_m > 0, ]

        if (nrow(riv_df) > 0) {
          message("  [gwflow] Writing gwflow_rivcell (",
                  nrow(riv_df), " row(s))...")
          DBI::dbWriteTable(con, "gwflow_rivcell", riv_df,
                            append = TRUE, row.names = FALSE)
        }
      }
    }, error = function(e) {
      message("  [gwflow] Warning: could not compute channel–cell ",
              "intersections: ", conditionMessage(e))
    })
  }

  # ------------------------------------------------------------------
  # 10. gwflow_lsucell: LSU–cell area intersections
  # ------------------------------------------------------------------
  message("  [gwflow] Computing LSU–cell intersections...")
  tryCatch({
    gis_lsus <- DBI::dbGetQuery(con, "SELECT id, subbasin FROM gis_lsus")

    lsu_inter <- sf::st_intersection(
      watershed_sf[, c("subbasin", "geometry")],
      grid_active[, c("cell_id", "geometry")]
    )

    if (nrow(lsu_inter) > 0) {
      lsu_inter$area_m2 <- as.numeric(sf::st_area(lsu_inter))
      lsu_id <- gis_lsus$id[match(lsu_inter$subbasin, gis_lsus$subbasin)]

      lsu_df <- data.frame(
        cell_id = lsu_inter$cell_id,
        lsu     = lsu_id,
        area_m2 = lsu_inter$area_m2,
        stringsAsFactors = FALSE
      )
      lsu_df <- lsu_df[!is.na(lsu_df$lsu) & lsu_df$area_m2 > 0, ]

      if (nrow(lsu_df) > 0) {
        message("  [gwflow] Writing gwflow_lsucell (",
                nrow(lsu_df), " row(s))...")
        DBI::dbWriteTable(con, "gwflow_lsucell", lsu_df,
                          append = TRUE, row.names = FALSE)
      }
    }
  }, error = function(e) {
    message("  [gwflow] Warning: could not compute LSU–cell intersections: ",
            conditionMessage(e))
  })

  # ------------------------------------------------------------------
  # 11. gwflow_hrucell: HRU–cell area intersections (HRU recharge mode)
  # ------------------------------------------------------------------
  hru_recharge <- cfg$hruorlsu_recharge == 1L || cfg$hruorlsu_recharge == 3L

  if (hru_recharge &&
      !is.null(project$hru_sf) && inherits(project$hru_sf, "sf") &&
      nrow(project$hru_sf) > 0) {
    message("  [gwflow] Computing HRU–cell intersections...")
    tryCatch({
      hru_inter <- sf::st_intersection(
        project$hru_sf[, c("hru_id", "geometry")],
        grid_active[, c("cell_id", "geometry")]
      )

      if (nrow(hru_inter) > 0) {
        hru_inter$area_m2 <- as.numeric(sf::st_area(hru_inter))

        hru_df <- data.frame(
          cell_id = hru_inter$cell_id,
          hru     = hru_inter$hru_id,
          area_m2 = hru_inter$area_m2,
          stringsAsFactors = FALSE
        )
        hru_df <- hru_df[!is.na(hru_df$hru) & hru_df$area_m2 > 0, ]

        if (nrow(hru_df) > 0) {
          message("  [gwflow] Writing gwflow_hrucell (",
                  nrow(hru_df), " row(s))...")
          DBI::dbWriteTable(con, "gwflow_hrucell", hru_df,
                            append = TRUE, row.names = FALSE)
        }
      }
    }, error = function(e) {
      message("  [gwflow] Warning: could not compute HRU–cell intersections: ",
              conditionMessage(e))
    })
  }

  # ------------------------------------------------------------------
  # 12. gwflow_rescell: reservoir–cell connections via gis_water join
  #     (mirrors the Python SQL: SELECT gwflow_lsucell.cell_id,
  #      gis_water.id, gis_water.elev FROM gis_water
  #      INNER JOIN gwflow_lsucell USING (lsu))
  # ------------------------------------------------------------------
  tryCatch({
    gis_water_n <- DBI::dbGetQuery(
      con, "SELECT COUNT(*) AS n FROM gis_water"
    )$n
    lsucell_n <- DBI::dbGetQuery(
      con, "SELECT COUNT(*) AS n FROM gwflow_lsucell"
    )$n

    if (gis_water_n > 0 && lsucell_n > 0) {
      res_rows <- DBI::dbGetQuery(con, "
        SELECT gwflow_lsucell.cell_id AS cell_id,
               gis_water.id          AS res,
               gis_water.elev        AS res_stage
        FROM gis_water
        INNER JOIN gwflow_lsucell ON gis_water.lsu = gwflow_lsucell.lsu
      ")
      if (nrow(res_rows) > 0) {
        message("  [gwflow] Writing gwflow_rescell (",
                nrow(res_rows), " row(s))...")
        DBI::dbWriteTable(con, "gwflow_rescell", res_rows,
                          append = TRUE, row.names = FALSE)
      }
    }
  }, error = function(e) {
    message("  [gwflow] Note: could not populate gwflow_rescell: ",
            conditionMessage(e))
  })

  invisible(NULL)
}
