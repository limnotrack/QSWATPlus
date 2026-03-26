#' Write SWAT+ Project Database
#'
#' Writes the HRU, subbasin, and routing data to a SWAT+ project
#' SQLite database. This database is used as input for the SWAT+
#' Editor.
#'
#' @param project A `qswat_project` object with HRU data from
#'   [qswat_create_hrus()].
#' @param db_file Character or NULL. Path for the output SQLite
#'   database. If NULL, creates the database in the project directory
#'   as `project.sqlite`.
#' @param overwrite Logical. If TRUE, overwrite existing database.
#'   Default is FALSE.
#'
#' @return The path to the created database file (invisibly).
#'
#' @details
#' Creates a SQLite database with the following tables:
#' \describe{
#'   \item{gis_subbasins}{Subbasin-level data including area, elevation,
#'     and slope statistics}
#'   \item{gis_hrus}{HRU data including subbasin, land use, soil,
#'     slope class, area, and elevation}
#'   \item{gis_routing}{Routing topology connecting subbasins}
#'   \item{gis_channels}{Channel/stream network data}
#'   \item{gis_lsus}{Landscape unit data}
#'   \item{gis_aquifers}{Aquifer data for each subbasin}
#'   \item{gis_deep_aquifers}{Deep aquifer data}
#'   \item{gis_water}{Water body data}
#'   \item{gis_points}{Point source data}
#' }
#'
#' The database format is compatible with the SWAT+ Editor for
#' further model parameterization.
#'
#' @examples
#' \dontrun{
#' db_path <- qswat_write_database(project)
#' }
#'
#' @export
qswat_write_database <- function(project,
                                  db_file = NULL,
                                  overwrite = FALSE) {

  if (!inherits(project, "qswat_project")) {
    stop("'project' must be a qswat_project object.", call. = FALSE)
  }
  if (is.null(project$hru_data)) {
    stop("No HRU data found. Run qswat_create_hrus() first.", call. = FALSE)
  }

  if (is.null(db_file)) {
    db_file <- file.path(project$project_dir, "project.sqlite")
  }

  if (file.exists(db_file) && !overwrite) {
    stop("Database already exists: ", db_file,
         ". Use overwrite = TRUE to replace.", call. = FALSE)
  }

  if (file.exists(db_file) && overwrite) {
    file.remove(db_file)
  }

  message("Writing SWAT+ project database...")

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Create tables
  .create_db_tables(con)

  # Write subbasin data
  .write_subbasin_table(con, project$basin_data)

  # Write HRU data
  .write_hru_table(con, project$hru_data, project$slope_classes)

  # Write routing topology
  .write_routing_table(con, project)

  # Write channel data
  .write_channel_table(con, project)

  # Write LSU data
  .write_lsu_table(con, project)

  # Write aquifer data
  .write_aquifer_table(con, project$basin_data)

  # Write water body data
  .write_water_table(con)

  # Write point source data
  .write_point_table(con)

  message("Database written to: ", db_file)
  invisible(db_file)
}


#' Create Database Tables
#' @noRd
.create_db_tables <- function(con) {
  # Subbasins table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_subbasins (
      id INTEGER PRIMARY KEY,
      area REAL,
      slo1 REAL,
      len1 REAL,
      sll REAL,
      lat REAL,
      lon REAL,
      elev REAL,
      elevmin REAL,
      elevmax REAL,
      waterid INTEGER DEFAULT 0
    )
  ")

  # HRUs table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_hrus (
      id INTEGER PRIMARY KEY,
      subbasin INTEGER,
      landuse TEXT,
      soil TEXT,
      slope_class INTEGER,
      area REAL,
      slope REAL,
      lat REAL,
      lon REAL,
      elev REAL,
      lu_pct REAL DEFAULT 0,
      soil_pct REAL DEFAULT 0,
      slope_pct REAL DEFAULT 0,
      cell_count INTEGER DEFAULT 0,
      FOREIGN KEY (subbasin) REFERENCES gis_subbasins(id)
    )
  ")

  # Routing table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_routing (
      sourceid INTEGER,
      sourcecat TEXT,
      sinkid INTEGER,
      sinkcat TEXT,
      percent REAL DEFAULT 100,
      hyd_typ TEXT DEFAULT 'tot'
    )
  ")

  # Channels table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_channels (
      id INTEGER PRIMARY KEY,
      subbasin INTEGER,
      order_val INTEGER DEFAULT 1,
      length REAL,
      slope REAL,
      wid2 REAL,
      dep2 REAL,
      lat REAL,
      lon REAL,
      elev REAL
    )
  ")

  # LSU table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_lsus (
      id INTEGER PRIMARY KEY,
      category INTEGER DEFAULT 0,
      channel INTEGER,
      area REAL,
      slope REAL,
      lat REAL,
      lon REAL,
      elev REAL
    )
  ")

  # Aquifer tables
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_aquifers (
      id INTEGER PRIMARY KEY,
      subbasin INTEGER,
      deep_aquifer INTEGER,
      area REAL,
      lat REAL,
      lon REAL,
      elev REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_deep_aquifers (
      id INTEGER PRIMARY KEY,
      area REAL,
      lat REAL,
      lon REAL,
      elev REAL
    )
  ")

  # Water body table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_water (
      id INTEGER PRIMARY KEY,
      wtype TEXT,
      subbasin INTEGER,
      area REAL,
      lat REAL,
      lon REAL,
      elev REAL
    )
  ")

  # Point source table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_points (
      id INTEGER PRIMARY KEY,
      subbasin INTEGER,
      ptype TEXT,
      lat REAL,
      lon REAL,
      elev REAL
    )
  ")
}


#' Write Subbasin Table
#' @noRd
.write_subbasin_table <- function(con, basin_data) {
  if (is.null(basin_data) || nrow(basin_data) == 0) return(invisible())

  df <- data.frame(
    id = basin_data$subbasin,
    area = basin_data$area_ha,
    slo1 = basin_data$mean_slope,
    len1 = sqrt(basin_data$area_ha * 10000),  # Approximate flow length
    sll = basin_data$mean_slope,
    lat = 0,
    lon = 0,
    elev = basin_data$mean_elevation,
    elevmin = basin_data$min_elevation,
    elevmax = basin_data$max_elevation,
    waterid = 0L,
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "gis_subbasins", df, append = TRUE, row.names = FALSE)
}


#' Write HRU Table
#' @noRd
.write_hru_table <- function(con, hru_data, slope_classes) {
  if (is.null(hru_data) || nrow(hru_data) == 0) return(invisible())

  # Compute percentages
  sub_areas <- tapply(hru_data$area_ha, hru_data$subbasin, sum)
  hru_data$sub_area <- sub_areas[as.character(hru_data$subbasin)]
  hru_data$lu_pct <- 0
  hru_data$soil_pct <- 0
  hru_data$slope_pct_val <- 0

  for (sub in unique(hru_data$subbasin)) {
    idx <- hru_data$subbasin == sub
    sub_area <- sum(hru_data$area_ha[idx])
    for (lu in unique(hru_data$landuse[idx])) {
      lu_idx <- idx & hru_data$landuse == lu
      lu_area <- sum(hru_data$area_ha[lu_idx])
      hru_data$lu_pct[lu_idx] <- lu_area / sub_area * 100
      for (soil in unique(hru_data$soil[lu_idx])) {
        soil_idx <- lu_idx & hru_data$soil == soil
        soil_area <- sum(hru_data$area_ha[soil_idx])
        hru_data$soil_pct[soil_idx] <- soil_area / lu_area * 100
        for (slp in unique(hru_data$slope_class[soil_idx])) {
          slp_idx <- soil_idx & hru_data$slope_class == slp
          slp_area <- sum(hru_data$area_ha[slp_idx])
          hru_data$slope_pct_val[slp_idx] <- slp_area / soil_area * 100
        }
      }
    }
  }

  df <- data.frame(
    id = hru_data$hru_id,
    subbasin = hru_data$subbasin,
    landuse = hru_data$landuse,
    soil = hru_data$soil,
    slope_class = hru_data$slope_class,
    area = hru_data$area_ha,
    slope = hru_data$mean_slope,
    lat = 0,
    lon = 0,
    elev = hru_data$mean_elevation,
    lu_pct = round(hru_data$lu_pct, 2),
    soil_pct = round(hru_data$soil_pct, 2),
    slope_pct = round(hru_data$slope_pct_val, 2),
    cell_count = hru_data$cell_count,
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "gis_hrus", df, append = TRUE, row.names = FALSE)
}


#' Write Routing Table
#' @noRd
.write_routing_table <- function(con, project) {
  if (is.null(project$stream_topology)) return(invisible())

  topo <- project$stream_topology
  routes <- list()

  for (i in seq_len(nrow(topo))) {
    link <- topo$LINKNO[i]
    ds_link <- topo$DSLINKNO[i]
    wsno <- topo$WSNO[i]

    if (!is.na(ds_link) && ds_link >= 0) {
      ds_wsno <- topo$WSNO[topo$LINKNO == ds_link]
      if (length(ds_wsno) > 0 && !is.na(ds_wsno[1])) {
        routes[[length(routes) + 1]] <- data.frame(
          sourceid = wsno,
          sourcecat = "sub",
          sinkid = ds_wsno[1],
          sinkcat = "sub",
          percent = 100,
          hyd_typ = "tot",
          stringsAsFactors = FALSE
        )
      }
    } else {
      # Outlet
      routes[[length(routes) + 1]] <- data.frame(
        sourceid = wsno,
        sourcecat = "sub",
        sinkid = 0L,
        sinkcat = "outlet",
        percent = 100,
        hyd_typ = "tot",
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(routes) > 0) {
    routing_df <- do.call(rbind, routes)
    DBI::dbWriteTable(con, "gis_routing", routing_df,
                       append = TRUE, row.names = FALSE)
  }
}


#' Write Channel Table
#' @noRd
.write_channel_table <- function(con, project) {
  if (is.null(project$stream_topology)) return(invisible())

  topo <- project$stream_topology
  valid <- !is.na(topo$WSNO)

  df <- data.frame(
    id = seq_len(sum(valid)),
    subbasin = topo$WSNO[valid],
    order_val = if (!all(is.na(topo$strmOrder))) topo$strmOrder[valid] else 1L,
    length = if (!all(is.na(topo$Length))) topo$Length[valid] else 0,
    slope = 0.01,  # Default slope
    wid2 = 1.0,    # Default width
    dep2 = 0.5,    # Default depth
    lat = 0,
    lon = 0,
    elev = 0,
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "gis_channels", df, append = TRUE, row.names = FALSE)
}


#' Write LSU Table
#' @noRd
.write_lsu_table <- function(con, project) {
  if (is.null(project$basin_data)) return(invisible())

  bd <- project$basin_data

  df <- data.frame(
    id = seq_len(nrow(bd)),
    category = 0L,
    channel = bd$subbasin,
    area = bd$area_ha,
    slope = bd$mean_slope,
    lat = 0,
    lon = 0,
    elev = bd$mean_elevation,
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "gis_lsus", df, append = TRUE, row.names = FALSE)
}


#' Write Aquifer Tables
#' @noRd
.write_aquifer_table <- function(con, basin_data) {
  if (is.null(basin_data)) return(invisible())

  # One shallow aquifer per subbasin
  aq_df <- data.frame(
    id = seq_len(nrow(basin_data)),
    subbasin = basin_data$subbasin,
    deep_aquifer = 1L,
    area = basin_data$area_ha,
    lat = 0,
    lon = 0,
    elev = basin_data$mean_elevation,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "gis_aquifers", aq_df,
                     append = TRUE, row.names = FALSE)

  # One deep aquifer for whole watershed
  deep_df <- data.frame(
    id = 1L,
    area = sum(basin_data$area_ha),
    lat = 0,
    lon = 0,
    elev = mean(basin_data$mean_elevation),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "gis_deep_aquifers", deep_df,
                     append = TRUE, row.names = FALSE)
}


#' Write Water Body Table (empty placeholder)
#' @noRd
.write_water_table <- function(con) {
  # Empty table - populated by user or SWAT+ Editor
  invisible()
}


#' Write Point Source Table (empty placeholder)
#' @noRd
.write_point_table <- function(con) {
  # Empty table - populated by user or SWAT+ Editor
  invisible()
}
