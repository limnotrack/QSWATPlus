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

  # Copy reference database to project folder
  ref_db_path <- .copy_reference_database(project$project_dir)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Create tables
  .create_db_tables(con)

  # Write project configuration
  .write_project_config(con, project, db_file, ref_db_path)

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

  # Write intermediate data tables
  .write_basinsdata_table(con, project$basin_data)
  .write_hrusdata_table(con, project$hru_data)
  .write_lsusdata_table(con, project$hru_data, project$basin_data)
  
  # Ensure all required tables exist with sensible defaults
  ensure_write_tables(con)

  message("Database written to: ", db_file)
  project$db_file <- db_file
  # invisible(db_file)
  return(project)
}


#' Create Database Tables
#'
#' Creates all tables required by the SWAT+ Editor, matching the
#' schema from the QSWATPlusProj.sqlite template database.
#' @noRd
.create_db_tables <- function(con) {

  # ---- project_config: critical for SWAT+ Editor ----
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS project_config (
      id                       INTEGER PRIMARY KEY NOT NULL DEFAULT (1),
      project_name             TEXT,
      project_directory        TEXT,
      editor_version           TEXT,
      gis_type                 TEXT,
      gis_version              TEXT,
      project_db               TEXT,
      reference_db             TEXT,
      wgn_db                   TEXT,
      wgn_table_name           TEXT,
      weather_data_dir         TEXT,
      weather_data_format      TEXT,
      input_files_dir          TEXT,
      input_files_last_written DATETIME,
      swat_last_run            DATETIME,
      delineation_done         BOOLEAN DEFAULT (0) NOT NULL,
      hrus_done                BOOLEAN DEFAULT (0) NOT NULL,
      soil_table               TEXT,
      soil_layer_table         TEXT,
      output_last_imported     DATETIME,
      imported_gis             BOOLEAN DEFAULT (0) NOT NULL,
      is_lte                   BOOLEAN NOT NULL DEFAULT (0),
      use_gwflow               BOOLEAN NOT NULL DEFAULT (0)
    )
  ")

  # ---- GIS tables ----
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_subbasins (
      id       INTEGER PRIMARY KEY UNIQUE NOT NULL,
      area     REAL,
      slo1     REAL,
      len1     REAL,
      sll      REAL,
      lat      REAL,
      lon      REAL,
      elev     REAL,
      elevmin  REAL,
      elevmax  REAL,
      waterid  INTEGER
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_hrus (
      id      INTEGER PRIMARY KEY UNIQUE NOT NULL,
      lsu     INTEGER,
      arsub   REAL,
      arlsu   REAL,
      landuse TEXT,
      arland  REAL,
      soil    TEXT,
      arso    REAL,
      slp     TEXT,
      arslp   REAL,
      slope   REAL,
      lat     REAL,
      lon     REAL,
      elev    REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_routing (
      sourceid  INTEGER,
      sourcecat TEXT,
      hyd_type  TEXT,
      sinkid    INTEGER,
      sinkcat   TEXT,
      percent   REAL
    )
  ")
  DBI::dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS source
      ON gis_routing (sourceid, sourcecat)
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_channels (
      id       INTEGER PRIMARY KEY UNIQUE NOT NULL,
      subbasin INTEGER,
      areac    REAL,
      strahler INTEGER,
      len2     REAL,
      slo2     REAL,
      wid2     REAL,
      dep2     REAL,
      elevmin  REAL,
      elevmax  REAL,
      midlat   REAL,
      midlon   REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_lsus (
      id       INTEGER PRIMARY KEY UNIQUE NOT NULL,
      category INTEGER,
      channel  INTEGER,
      subbasin INTEGER,
      area     REAL,
      slope    REAL,
      len1     REAL,
      csl      REAL,
      wid1     REAL,
      dep1     REAL,
      lat      REAL,
      lon      REAL,
      elev     REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_aquifers (
      id            INTEGER PRIMARY KEY,
      subbasin      INTEGER,
      deep_aquifer  INTEGER,
      area          REAL,
      lat           REAL,
      lon           REAL,
      elev          REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_deep_aquifers (
      id   INTEGER PRIMARY KEY,
      area REAL,
      lat  REAL,
      lon  REAL,
      elev REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_water (
      id    INTEGER PRIMARY KEY UNIQUE NOT NULL,
      wtype TEXT,
      lsu   INTEGER,
      area  REAL,
      lat   REAL,
      lon   REAL,
      elev  REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_points (
      id       INTEGER PRIMARY KEY UNIQUE NOT NULL,
      subbasin INTEGER,
      ptype    TEXT,
      xpr      REAL,
      ypr      REAL,
      lat      REAL,
      lon      REAL,
      elev     REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_elevationbands (
      subbasin   INTEGER PRIMARY KEY UNIQUE NOT NULL,
      elevb1     REAL, elevb2     REAL, elevb3     REAL,
      elevb4     REAL, elevb5     REAL, elevb6     REAL,
      elevb7     REAL, elevb8     REAL, elevb9     REAL,
      elevb10    REAL,
      elevb_fr1  REAL, elevb_fr2  REAL, elevb_fr3  REAL,
      elevb_fr4  REAL, elevb_fr5  REAL, elevb_fr6  REAL,
      elevb_frR7 REAL, elevb_fr8  REAL, elevb_fr9  REAL,
      elevb_fr10 REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_landexempt (landuse TEXT)
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_splithrus (
      landuse    TEXT,
      sublanduse TEXT,
      percent    REAL
    )
  ")

  # ---- Configuration tables ----
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS config_delin (
      DEM               TEXT,
      burn              TEXT,
      existingWshed     BOOLEAN DEFAULT (false),
      useSQLite         BOOLEAN DEFAULT (true),
      fromArc           INTEGER,
      fromGRASS         BOOLEAN DEFAULT (false),
      gridSize          INTEGER,
      isHAWQS           BOOLEAN DEFAULT (false),
      isHUC             INTEGER DEFAULT (false),
      net               STRING,
      outlets           TEXT,
      snapThreshold     INTEGER DEFAULT (300),
      thresholdCh       INTEGER,
      thresholdSt       INTEGER,
      useGridModel      BOOLEAN DEFAULT (false),
      useOutlets        BOOLEAN DEFAULT (false),
      verticalUnits     TEXT DEFAULT meters,
      wshed             TEXT,
      subbasins         TEXT,
      gridDrainage      BOOLEAN DEFAULT (false),
      streamDrainage    BOOLEAN DEFAULT (false),
      drainageTable     TEXT,
      lakes             TEXT,
      lakesDone         BOOLEAN DEFAULT (false),
      gridLakesAdded    BOOLEAN DEFAULT (false),
      lakePointsAdded   BOOLEAN DEFAULT (false),
      delinNet          TEXT,
      channels          TEXT,
      snapOutlets       TEXT,
      subsNoLakes       TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS config_hru (
      areaVal            INTEGER DEFAULT (0),
      elevBandsThreshold INTEGER DEFAULT (0),
      isArea             BOOLEAN DEFAULT (false),
      isDominantHRU      BOOLEAN DEFAULT (false),
      isMultiple         BOOLEAN DEFAULT (true),
      isTarget           BOOLEAN DEFAULT (false),
      landuseVal         INTEGER,
      numElevBands       INTEGER DEFAULT (0),
      slopeBands         TEXT,
      slopeBandsFile     TEXT,
      slopeVal           INTEGER,
      soilVal            INTEGER,
      targetVal          INTEGER,
      useArea            BOOLEAN DEFAULT (false),
      useGWFlow          BOOLEAN DEFAULT (false)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS config_landuse (
      file  TEXT,
      plant TEXT,
      tabl  TEXT,
      urban TEXT,
      water INTEGER
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS config_lsu (
      channelMergeByPercent  BOOLEAN DEFAULT (true),
      channelMergeVal        INTEGER,
      floodplainFile         TEXT,
      thresholdResFlood      INTEGER,
      thresholdResNoFlood    INTEGER,
      useLandscapes          BOOLEAN DEFAULT (false),
      useLeftRight           BOOLEAN DEFAULT (false)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS config_observed (observedFile TEXT)
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS config_params (
      burninDepth              REAL,
      channelWidthMultiplier   REAL,
      channelWidthExponent     REAL,
      channelDepthMultiplier   REAL,
      channelDepthExponent     REAL,
      reachSlopeMultiplier     REAL,
      tributarySlopeMultiplier  REAL,
      meanSlopeMultiplier      REAL,
      mainLengthMultiplier     REAL,
      tributaryLengthMultiplier REAL,
      upslopeHRUDrain          REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS config_soil (
      \"database\"  TEXT,
      databaseTable TEXT,
      file          TEXT,
      tabl          TEXT,
      useSSURGO     BOOLEAN DEFAULT (false),
      useSTATSGO    BOOLEAN DEFAULT (false)
    )
  ")

  # ---- Intermediate data tables ----
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS BASINSDATA (
      basin                   INTEGER,
      drainArea               REAL,
      outletCol               INTEGER,
      outletRow               INTEGER,
      outletElevation         REAL,
      startCol                INTEGER,
      startRow                INTEGER,
      startToOutletDistance    REAL,
      startToOutletDrop       REAL,
      farCol                  INTEGER,
      farRow                  INTEGER,
      farthest                INTEGER,
      farElevation            REAL,
      farDistance              REAL,
      maxElevation            REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS HRUSDATA (
      hru            INTEGER,
      lsu            INTEGER,
      basin          INTEGER,
      crop           INTEGER,
      soil           INTEGER,
      slope          INTEGER,
      cellCount      INTEGER,
      area           REAL,
      totalElevation REAL,
      totalSlope     REAL,
      totalLatitude  REAL,
      totalLongitude REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS LSUSDATA (
      lsu                INTEGER,
      basin              INTEGER,
      cellCount          INTEGER,
      area               REAL,
      totalElevation     REAL,
      totalSlope         REAL,
      totalLatitude      REAL,
      totalLongitude     REAL,
      cropSoilSlopeArea  REAL,
      hru                INTEGER
    )
  ")
  DBI::dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS basin_lu_index ON LSUSDATA (lsu, basin)
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS WATERDATA (
      id       INTEGER PRIMARY KEY,
      subbasin INTEGER,
      area     REAL,
      lat      REAL,
      lon      REAL,
      elev     REAL
    )
  ")

  # ---- Lookup / reference tables ----
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS global_landuses (
      LANDUSE_ID INTEGER PRIMARY KEY,
      SWAT_CODE  TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS global_soils (
      SOIL_ID INTEGER PRIMARY KEY,
      SNAM    TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS global_usersoil (
      OBJECTID INTEGER, MUID TEXT, SEQN TEXT, SNAM TEXT, S5ID TEXT,
      CMPPCT TEXT, NLAYERS REAL, HYDGRP TEXT, SOL_ZMX REAL,
      ANION_EXCL REAL, SOL_CRK REAL, TEXTURE TEXT,
      SOL_Z1 REAL, SOL_BD1 REAL, SOL_AWC1 REAL, SOL_K1 REAL,
      SOL_CBN1 REAL, CLAY1 REAL, SILT1 REAL, SAND1 REAL,
      ROCK1 REAL, SOL_ALB1 REAL, USLE_K1 REAL, SOL_EC1 REAL,
      SOL_Z2 REAL, SOL_BD2 REAL, SOL_AWC2 REAL, SOL_K2 REAL,
      SOL_CBN2 REAL, CLAY2 REAL, SILT2 REAL, SAND2 REAL,
      ROCK2 REAL, SOL_ALB2 REAL, USLE_K2 REAL, SOL_EC2 REAL,
      SOL_Z3 REAL, SOL_BD3 REAL, SOL_AWC3 REAL, SOL_K3 REAL,
      SOL_CBN3 REAL, CLAY3 REAL, SILT3 REAL, SAND3 REAL,
      ROCK3 REAL, SOL_ALB3 REAL, USLE_K3 REAL, SOL_EC3 REAL,
      SOL_Z4 REAL, SOL_BD4 REAL, SOL_AWC4 REAL, SOL_K4 REAL,
      SOL_CBN4 REAL, CLAY4 REAL, SILT4 REAL, SAND4 REAL,
      ROCK4 REAL, SOL_ALB4 REAL, USLE_K4 REAL, SOL_EC4 REAL,
      SOL_Z5 REAL, SOL_BD5 REAL, SOL_AWC5 REAL, SOL_K5 REAL,
      SOL_CBN5 REAL, CLAY5 REAL, SILT5 REAL, SAND5 REAL,
      ROCK5 REAL, SOL_ALB5 REAL, USLE_K5 REAL, SOL_EC5 REAL,
      SOL_Z6 REAL, SOL_BD6 REAL, SOL_AWC6 REAL, SOL_K6 REAL,
      SOL_CBN6 REAL, CLAY6 REAL, SILT6 REAL, SAND6 REAL,
      ROCK6 REAL, SOL_ALB6 REAL, USLE_K6 REAL, SOL_EC6 REAL,
      SOL_Z7 REAL, SOL_BD7 REAL, SOL_AWC7 REAL, SOL_K7 REAL,
      SOL_CBN7 REAL, CLAY7 REAL, SILT7 REAL, SAND7 REAL,
      ROCK7 REAL, SOL_ALB7 REAL, USLE_K7 REAL, SOL_EC7 REAL,
      SOL_Z8 REAL, SOL_BD8 REAL, SOL_AWC8 REAL, SOL_K8 REAL,
      SOL_CBN8 REAL, CLAY8 REAL, SILT8 REAL, SAND8 REAL,
      ROCK8 REAL, SOL_ALB8 REAL, USLE_K8 REAL, SOL_EC8 REAL,
      SOL_Z9 REAL, SOL_BD9 REAL, SOL_AWC9 REAL, SOL_K9 REAL,
      SOL_CBN9 REAL, CLAY9 REAL, SILT9 REAL, SAND9 REAL,
      ROCK9 REAL, SOL_ALB9 REAL, USLE_K9 REAL, SOL_EC9 REAL,
      SOL_Z10 REAL, SOL_BD10 REAL, SOL_AWC10 REAL, SOL_K10 REAL,
      SOL_CBN10 REAL, CLAY10 REAL, SILT10 REAL, SAND10 REAL,
      ROCK10 REAL, SOL_ALB10 REAL, USLE_K10 REAL, SOL_EC10 REAL,
      SOL_CAL1 REAL, SOL_CAL2 REAL, SOL_CAL3 REAL, SOL_CAL4 REAL,
      SOL_CAL5 REAL, SOL_CAL6 REAL, SOL_CAL7 REAL, SOL_CAL8 REAL,
      SOL_CAL9 REAL, SOL_CAL10 REAL,
      SOL_PH1 REAL, SOL_PH2 REAL, SOL_PH3 REAL, SOL_PH4 REAL,
      SOL_PH5 REAL, SOL_PH6 REAL, SOL_PH7 REAL, SOL_PH8 REAL,
      SOL_PH9 REAL, SOL_PH10 REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS plant (
      id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(255) NOT NULL,
      plnt_typ VARCHAR(255) NOT NULL, gro_trig VARCHAR(255) NOT NULL,
      nfix_co REAL NOT NULL, days_mat REAL NOT NULL,
      bm_e REAL NOT NULL, harv_idx REAL NOT NULL,
      lai_pot REAL NOT NULL, frac_hu1 REAL NOT NULL,
      lai_max1 REAL NOT NULL, frac_hu2 REAL NOT NULL,
      lai_max2 REAL NOT NULL, hu_lai_decl REAL NOT NULL,
      dlai_rate REAL NOT NULL, can_ht_max REAL NOT NULL,
      rt_dp_max REAL NOT NULL, tmp_opt REAL NOT NULL,
      tmp_base REAL NOT NULL, frac_n_yld REAL NOT NULL,
      frac_p_yld REAL NOT NULL, frac_n_em REAL NOT NULL,
      frac_n_50 REAL NOT NULL, frac_n_mat REAL NOT NULL,
      frac_p_em REAL NOT NULL, frac_p_50 REAL NOT NULL,
      frac_p_mat REAL NOT NULL, harv_idx_ws REAL NOT NULL,
      usle_c_min REAL NOT NULL, stcon_max REAL NOT NULL,
      vpd REAL NOT NULL, frac_stcon REAL NOT NULL,
      ru_vpd REAL NOT NULL, co2_hi REAL NOT NULL,
      bm_e_hi REAL NOT NULL, plnt_decomp REAL NOT NULL,
      lai_min REAL NOT NULL, bm_tree_acc REAL NOT NULL,
      yrs_mat REAL NOT NULL, bm_tree_max REAL NOT NULL,
      ext_co REAL NOT NULL, leaf_tov_mn REAL NOT NULL,
      leaf_tov_mx REAL NOT NULL, bm_dieoff REAL NOT NULL,
      rt_st_beg REAL NOT NULL, rt_st_end REAL NOT NULL,
      plnt_pop1 REAL NOT NULL, frac_lai1 REAL NOT NULL,
      plnt_pop2 REAL NOT NULL, frac_lai2 REAL NOT NULL,
      frac_sw_gro REAL NOT NULL, aeration REAL NOT NULL,
      rsd_pctcov REAL NOT NULL, rsd_covfac REAL NOT NULL,
      description TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS urban (
      id INTEGER NOT NULL PRIMARY KEY, name TEXT NOT NULL,
      frac_imp REAL NOT NULL, frac_dc_imp REAL NOT NULL,
      curb_den REAL NOT NULL, urb_wash REAL NOT NULL,
      dirt_max REAL NOT NULL, t_halfmax REAL NOT NULL,
      conc_totn REAL NOT NULL, conc_totp REAL NOT NULL,
      conc_no3n REAL NOT NULL, urb_cn REAL NOT NULL,
      description TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS WGEN_User (
      id INTEGER NOT NULL PRIMARY KEY,
      name VARCHAR(255) NOT NULL,
      lat REAL NOT NULL, lon REAL NOT NULL,
      elev REAL NOT NULL, rain_yrs INTEGER NOT NULL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS WGEN_User_mon (
      id INTEGER NOT NULL PRIMARY KEY,
      weather_wgn_cli_id INTEGER NOT NULL,
      month INTEGER NOT NULL,
      tmp_max_ave REAL NOT NULL, tmp_min_ave REAL NOT NULL,
      tmp_max_sd REAL NOT NULL, tmp_min_sd REAL NOT NULL,
      pcp_ave REAL NOT NULL, pcp_sd REAL NOT NULL,
      pcp_skew REAL NOT NULL, wet_dry REAL NOT NULL,
      wet_wet REAL NOT NULL, pcp_days REAL NOT NULL,
      pcp_hhr REAL NOT NULL, slr_ave REAL NOT NULL,
      dew_ave REAL NOT NULL, wnd_ave REAL NOT NULL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS weather_sta_cli (
      id       INTEGER       NOT NULL PRIMARY KEY,
      name     VARCHAR (255) NOT NULL,
      wgn_id   INTEGER,
      pcp      VARCHAR (255),
      tmp      VARCHAR (255),
      slr      VARCHAR (255),
      hmd      VARCHAR (255),
      wnd      VARCHAR (255),
      pet      VARCHAR (255),
      atmo_dep VARCHAR (255),
      lat      REAL,
      lon      REAL
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
    len1 = sqrt(basin_data$area_ha * 10000),
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

  # arsub = HRU area / subbasin area
  hru_data$arsub <- hru_data$area_ha / hru_data$sub_area * 100

  # arlsu = HRU area / LSU area (using subbasin as LSU)
  hru_data$arlsu <- hru_data$area_ha / hru_data$sub_area * 100

  # arland = landuse area / subbasin area
  hru_data$arland <- 0
  hru_data$arso <- 0
  hru_data$arslp <- 0

  for (sub in unique(hru_data$subbasin)) {
    idx <- hru_data$subbasin == sub
    sub_area <- sum(hru_data$area_ha[idx])
    for (lu in unique(hru_data$landuse[idx])) {
      lu_idx <- idx & hru_data$landuse == lu
      lu_area <- sum(hru_data$area_ha[lu_idx])
      hru_data$arland[lu_idx] <- lu_area / sub_area * 100
      for (soil in unique(hru_data$soil[lu_idx])) {
        soil_idx <- lu_idx & hru_data$soil == soil
        soil_area <- sum(hru_data$area_ha[soil_idx])
        hru_data$arso[soil_idx] <- soil_area / lu_area * 100
        for (slp in unique(hru_data$slope_class[soil_idx])) {
          slp_idx <- soil_idx & hru_data$slope_class == slp
          slp_area <- sum(hru_data$area_ha[slp_idx])
          hru_data$arslp[slp_idx] <- slp_area / soil_area * 100
        }
      }
    }
  }

  # Build slope class label
  slp_label <- as.character(hru_data$slope_class)
  if (!is.null(slope_classes) && nrow(slope_classes) > 0) {
    slp_label <- slope_classes$label[
      match(hru_data$slope_class, slope_classes$class_id)]
    slp_label[is.na(slp_label)] <- as.character(
      hru_data$slope_class[is.na(slp_label)])
  }

  df <- data.frame(
    id = hru_data$hru_id,
    lsu = hru_data$subbasin,
    arsub = round(hru_data$arsub, 4),
    arlsu = round(hru_data$arlsu, 4),
    landuse = hru_data$landuse,
    arland = round(hru_data$arland, 4),
    soil = hru_data$soil,
    arso = round(hru_data$arso, 4),
    slp = slp_label,
    arslp = round(hru_data$arslp, 4),
    slope = hru_data$mean_slope,
    lat = 0,
    lon = 0,
    elev = hru_data$mean_elevation,
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "gis_hrus", df, append = TRUE, row.names = FALSE)
}


#' Resolve the first valid downstream WSNO (> 0), following the chain past any
#' WSNO == 0 stream links.  Returns NA_integer_ if the chain reaches an outlet
#' without finding a valid subbasin.
#' @noRd
.resolve_downstream_wsno <- function(topo, link_no) {
  visited <- character(0)
  while (!is.na(link_no) && link_no >= 0) {
    key <- as.character(link_no)
    if (key %in% visited) break        # guard against topology loops
    visited <- c(visited, key)

    row <- topo[topo$LINKNO == link_no, , drop = FALSE]
    if (nrow(row) == 0) break

    wsno <- row$WSNO[1]
    if (!is.na(wsno) && wsno > 0) return(wsno)

    # WSNO == 0: follow further downstream
    ds_link <- row$DSLINKNO[1]
    if (is.na(ds_link) || ds_link < 0) break  # reached the outlet
    link_no <- ds_link
  }
  NA_integer_
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

    # Skip stream links that have no valid subbasin (WSNO == 0 or NA).
    # These are typically the most-downstream outlet reach generated by
    # TauDEM when an outlet point is provided; they have no cells in the
    # watershed raster and therefore no entry in gis_subbasins.
    if (is.na(wsno) || wsno == 0) next

    if (!is.na(ds_link) && ds_link >= 0) {
      # Follow the chain past any WSNO == 0 intermediate links.
      ds_wsno <- .resolve_downstream_wsno(topo, ds_link)

      if (!is.na(ds_wsno)) {
        routes[[length(routes) + 1]] <- data.frame(
          sourceid = wsno,
          sourcecat = "sub",
          hyd_type = "tot",
          sinkid = ds_wsno,
          sinkcat = "sub",
          percent = 100,
          stringsAsFactors = FALSE
        )
      } else {
        # Downstream chain ends at an outlet (WSNO == 0 or NA at outlet)
        routes[[length(routes) + 1]] <- data.frame(
          sourceid = wsno,
          sourcecat = "sub",
          hyd_type = "tot",
          sinkid = 0L,
          sinkcat = "outlet",
          percent = 100,
          stringsAsFactors = FALSE
        )
      }
    } else {
      # Direct outlet
      routes[[length(routes) + 1]] <- data.frame(
        sourceid = wsno,
        sourcecat = "sub",
        hyd_type = "tot",
        sinkid = 0L,
        sinkcat = "outlet",
        percent = 100,
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
  # Only include channels whose WSNO maps to a real subbasin (WSNO > 0).
  # WSNO == 0 is assigned by TauDEM to the outlet reach when an outlet point
  # is provided; it has no corresponding subbasin in gis_subbasins.
  valid <- !is.na(topo$WSNO) & topo$WSNO > 0

  # Estimate drainage area per channel from basin data if available
  areac_val <- rep(0, sum(valid))
  if (!is.null(project$basin_data)) {
    bd <- project$basin_data
    areac_val <- bd$area_ha[match(topo$WSNO[valid], bd$subbasin)]
    areac_val[is.na(areac_val)] <- 0
  }

  # Use Strahler stream order from topology if available
  strahler_val <- rep(1L, sum(valid))
  if ("strmOrder" %in% names(topo)) {
    strahler_val <- topo$strmOrder[valid]
    strahler_val[is.na(strahler_val)] <- 1L
  }

  df <- data.frame(
    id = seq_len(sum(valid)),
    subbasin = topo$WSNO[valid],
    areac = areac_val,
    strahler = strahler_val,
    len2 = if (!all(is.na(topo$Length))) topo$Length[valid] else 0,
    slo2 = 0.01,
    wid2 = 1.0,
    dep2 = 0.5,
    elevmin = 0,
    elevmax = 0,
    midlat = 0,
    midlon = 0,
    stringsAsFactors = FALSE
  )

  # Fill elevmin/elevmax from basin data if available
  if (!is.null(project$basin_data)) {
    bd <- project$basin_data
    idx <- match(df$subbasin, bd$subbasin)
    df$elevmin[!is.na(idx)] <- bd$min_elevation[idx[!is.na(idx)]]
    df$elevmax[!is.na(idx)] <- bd$max_elevation[idx[!is.na(idx)]]
  }

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
    subbasin = bd$subbasin,
    area = bd$area_ha,
    slope = bd$mean_slope,
    len1 = sqrt(bd$area_ha * 10000),
    csl = bd$mean_slope,
    wid1 = 1.0,
    dep1 = 0.5,
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


#' Copy Reference Databases to Project Folder
#'
#' Copies the bundled reference databases to the project folder:
#' \itemize{
#'   \item \file{QSWATPlusProj.sqlite} \rarr \file{swatplus_datasets.sqlite}
#'         (standard project template)
#'   \item \file{QSWATPlusRefHAWQS.sqlite} \rarr
#'         \file{swatplus_datasets_ref.sqlite} (HAWQS reference/parameter data)
#'   \item \file{QSWATPlusProjHAWQS.sqlite} \rarr
#'         \file{swatplus_datasets_proj_hawqs.sqlite} (HAWQS project template)
#' }
#' Returns the normalised path to the main copy (or \code{NA} if the bundled
#' source cannot be found).
#' @noRd
.copy_reference_database <- function(project_dir) {
  src <- system.file("extdata", "QSWATPlusProj.sqlite",
                     package = "rQSWATPlus")
  if (!nzchar(src) || !file.exists(src)) {
    message("Reference database not found; skipping copy.")
    return(NA_character_)
  }

  dest <- file.path(project_dir, "swatplus_datasets.sqlite")
  if (!file.exists(dest)) {
    file.copy(src, dest)
    message("Reference database copied to: ", dest)
  }

  # Copy HAWQS reference database (rich parameter data)
  ref_hawqs_src <- system.file("extdata", "QSWATPlusRefHAWQS.sqlite",
                               package = "rQSWATPlus")
  if (nzchar(ref_hawqs_src) && file.exists(ref_hawqs_src)) {
    ref_hawqs_dest <- file.path(project_dir, "swatplus_datasets_ref.sqlite")
    if (!file.exists(ref_hawqs_dest)) {
      file.copy(ref_hawqs_src, ref_hawqs_dest)
    }
  }

  # Copy HAWQS project template database
  proj_hawqs_src <- system.file("extdata", "QSWATPlusProjHAWQS.sqlite",
                                package = "rQSWATPlus")
  if (nzchar(proj_hawqs_src) && file.exists(proj_hawqs_src)) {
    proj_hawqs_dest <- file.path(project_dir,
                                 "swatplus_datasets_proj_hawqs.sqlite")
    if (!file.exists(proj_hawqs_dest)) {
      file.copy(proj_hawqs_src, proj_hawqs_dest)
    }
  }

  normalizePath(dest, mustWork = FALSE)
}


#' Write project_config Table
#'
#' Populates the project_config table with project metadata so that
#' SWAT+ Editor can recognize and open the database.
#' @noRd
.write_project_config <- function(con, project, db_file, ref_db_path = NA_character_) {
  project_name <- basename(project$project_dir)
  project_dir  <- normalizePath(project$project_dir, mustWork = FALSE)

  df <- data.frame(
    id = 1L,
    project_name = project_name,
    project_directory = project_dir,
    editor_version = NA_character_,
    gis_type = "qgis",
    gis_version = NA_character_,
    project_db = normalizePath(db_file, mustWork = FALSE),
    reference_db = ref_db_path,
    wgn_db = NA_character_,
    wgn_table_name = "wgn_cfsr_world",
    weather_data_dir = NA_character_,
    weather_data_format = NA_character_,
    input_files_dir = file.path(project_dir, "Scenarios",
                                "Default", "TxtInOut"),
    input_files_last_written = NA_character_,
    swat_last_run = NA_character_,
    delineation_done = 1L,
    hrus_done = 1L,
    soil_table = "soils_sol",
    soil_layer_table = "soils_sol_layer",
    output_last_imported = NA_character_,
    imported_gis = 1L,
    is_lte = 0L,
    use_gwflow = 0L,
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "project_config", df,
                     append = TRUE, row.names = FALSE)
}


#' Write BASINSDATA Table
#' @noRd
.write_basinsdata_table <- function(con, basin_data) {
  if (is.null(basin_data) || nrow(basin_data) == 0) return(invisible())

  df <- data.frame(
    basin = basin_data$subbasin,
    drainArea = basin_data$area_ha,
    outletCol = 0L,
    outletRow = 0L,
    outletElevation = basin_data$min_elevation,
    startCol = 0L,
    startRow = 0L,
    startToOutletDistance = sqrt(basin_data$area_ha * 10000),
    startToOutletDrop = basin_data$max_elevation - basin_data$min_elevation,
    farCol = 0L,
    farRow = 0L,
    farthest = 0L,
    farElevation = basin_data$max_elevation,
    farDistance = sqrt(basin_data$area_ha * 10000),
    maxElevation = basin_data$max_elevation,
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "BASINSDATA", df, append = TRUE, row.names = FALSE)
}


#' Write HRUSDATA Table
#' @noRd
.write_hrusdata_table <- function(con, hru_data) {
  if (is.null(hru_data) || nrow(hru_data) == 0) return(invisible())

  df <- data.frame(
    hru = hru_data$hru_id,
    lsu = hru_data$subbasin,
    basin = hru_data$subbasin,
    crop = as.integer(factor(hru_data$landuse)),
    soil = as.integer(factor(hru_data$soil)),
    slope = hru_data$slope_class,
    cellCount = hru_data$cell_count,
    area = hru_data$area_ha,
    totalElevation = hru_data$mean_elevation * hru_data$cell_count,
    totalSlope = hru_data$mean_slope * hru_data$cell_count,
    totalLatitude = 0,
    totalLongitude = 0,
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "HRUSDATA", df, append = TRUE, row.names = FALSE)
}


#' Write LSUSDATA Table
#' @noRd
.write_lsusdata_table <- function(con, hru_data, basin_data) {
  if (is.null(basin_data) || nrow(basin_data) == 0) return(invisible())

  n_hrus_per_sub <- tapply(hru_data$hru_id, hru_data$subbasin, length)

  df <- data.frame(
    lsu = basin_data$subbasin,
    basin = basin_data$subbasin,
    cellCount = 0L,
    area = basin_data$area_ha,
    totalElevation = basin_data$mean_elevation *
      round(basin_data$area_ha * 10000 / 900),
    totalSlope = basin_data$mean_slope *
      round(basin_data$area_ha * 10000 / 900),
    totalLatitude = 0,
    totalLongitude = 0,
    cropSoilSlopeArea = basin_data$area_ha,
    hru = as.integer(n_hrus_per_sub[as.character(basin_data$subbasin)]),
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "LSUSDATA", df, append = TRUE, row.names = FALSE)
}


#' Safe row count for a table
#'
#' Returns the number of rows in \code{tbl_name}, or \code{-1L} if the table
#' does not exist or an error occurs.
#' @noRd
.safe_table_count <- function(con, tbl_name) {
  tryCatch(
    DBI::dbGetQuery(con,
                    paste0("SELECT COUNT(*) AS n FROM main.", tbl_name))$n,
    error = function(e) -1L
  )
}


#' Populate reference/parameter tables from the SWAT+ datasets databases
#'
#' Uses SQLite ATTACH to copy reference data (plants, fertilizers, operations,
#' structural BMPs, land use, calibration parameters, soils, weather generator,
#' etc.) from the bundled reference databases into the project database.
#'
#' Data are sourced from two databases (in priority order):
#' \enumerate{
#'   \item \file{QSWATPlusRefHAWQS.sqlite} -- rich parameter/reference data
#'         (plants_plt, fertilizer_frt, management schedules, soil, wgn, …)
#'   \item \file{QSWATPlusProj.sqlite} -- standard project template (fallback)
#' }
#'
#' Additionally, HAWQS-specific tables (\code{plant_HAWQS}, \code{urban_HAWQS},
#' CDL landuse field tables, \code{statsgo_ssurgo_lkey*}) are copied from
#' \file{QSWATPlusProjHAWQS.sqlite}.
#'
#' Only empty or missing tables are populated; tables with existing data are
#' left untouched.
#'
#' @param con DBI connection to the project database.
#' @return Invisible \code{NULL}.
#' @keywords internal
populate_from_datasets <- function(con) {

  # ------------------------------------------------------------------
  # Locate bundled databases
  # ------------------------------------------------------------------
  ref_hawqs_db <- ""
  proj_hawqs_db <- ""
  proj_db <- ""
  if (requireNamespace("rQSWATPlus", quietly = TRUE)) {
    ref_hawqs_db <- system.file("extdata", "QSWATPlusRefHAWQS.sqlite",
                                package = "rQSWATPlus")
    proj_hawqs_db <- system.file("extdata", "QSWATPlusProjHAWQS.sqlite",
                                 package = "rQSWATPlus")
    proj_db <- system.file("extdata", "QSWATPlusProj.sqlite",
                           package = "rQSWATPlus")
  }

  has_ref_hawqs <- nzchar(ref_hawqs_db) && file.exists(ref_hawqs_db)
  has_proj_hawqs <- nzchar(proj_hawqs_db) && file.exists(proj_hawqs_db)
  has_proj <- nzchar(proj_db) && file.exists(proj_db)

  if (!has_ref_hawqs && !has_proj) return(invisible(NULL))

  # ------------------------------------------------------------------
  # 1.  HAWQS Reference database  (primary source for parameter data)
  # ------------------------------------------------------------------
  if (has_ref_hawqs) {
    con2 <- DBI::dbConnect(RSQLite::SQLite(), ref_hawqs_db)
    ref_tables_avail <- DBI::dbListTables(con2)
    DBI::dbDisconnect(con2)

    DBI::dbExecute(con, paste0(
      "ATTACH DATABASE '", ref_hawqs_db, "' AS ref_hawqs"))
    on.exit(tryCatch(DBI::dbExecute(con, "DETACH DATABASE ref_hawqs"),
                     error = function(e) NULL), add = TRUE)

    # Reference tables to copy (ordered for FK dependencies)
    ref_tables <- c(
      # Parameter database
      "plants_plt", "fertilizer_frt", "tillage_til", "pesticide_pst",
      "pathogens_pth", "urban_urb", "septic_sep", "snow_sno",
      # LUM lookup tables
      "cntable_lum", "ovn_table_lum", "cons_prac_lum",
      # Operations (graze_ops depends on fertilizer_frt)
      "harv_ops", "fire_ops", "irr_ops", "sweep_ops", "chem_app_ops",
      "graze_ops",
      # Structural BMPs
      "bmpuser_str", "filterstrip_str", "grassedww_str",
      "septic_str", "tiledrain_str",
      # Calibration
      "cal_parms_cal",
      # Soils LTE
      "soils_lte_sol",
      # Land use (depends on cntable, cons_prac, ovn_table, etc.)
      "landuse_lum",
      # Management schedules (HAWQS-specific rich data)
      "management_sch", "management_sch_auto", "management_sch_op",
      # Basin parameters
      "codes_bsn", "parameters_bsn",
      # Initial conditions
      "plant_ini", "plant_ini_item",
      # Print settings
      "print_prt", "print_prt_object",
      # File CIO
      "file_cio", "file_cio_classification",
      # Soil and weather generator data from HAWQS ref
      "soil", "soil_layer", "wgn", "wgn_mon",
      # HAWQS-specific reference tables
      "urban", "NLCD_CDL_color_scheme", "tropical_bounds", "version"
    )

    for (tbl in ref_tables) {
      if (!(tbl %in% ref_tables_avail)) next
      n <- .safe_table_count(con, tbl)
      if (n <= 0L) {
        tryCatch(
          DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS main.", tbl)),
          error = function(e) NULL
        )
        DBI::dbExecute(con, paste0(
          "CREATE TABLE main.", tbl,
          " AS SELECT * FROM ref_hawqs.", tbl
        ))
      }
    }

    # Decision tables from HAWQS ref
    dtl_tables_available <- all(c("d_table_dtl", "d_table_dtl_cond",
      "d_table_dtl_cond_alt", "d_table_dtl_act", "d_table_dtl_act_out") %in%
      ref_tables_avail)
    if (dtl_tables_available && .safe_table_count(con, "d_table_dtl") <= 0L) {
      .copy_decision_tables(con, "ref_hawqs")
    }

    DBI::dbExecute(con, "DETACH DATABASE ref_hawqs")
  }

  # ------------------------------------------------------------------
  # 2.  Standard project template (fallback for anything not in HAWQS ref)
  # ------------------------------------------------------------------
  if (has_proj) {
    con2 <- DBI::dbConnect(RSQLite::SQLite(), proj_db)
    ds_tables <- DBI::dbListTables(con2)
    DBI::dbDisconnect(con2)

    DBI::dbExecute(con, paste0(
      "ATTACH DATABASE '", proj_db, "' AS datasets"))
    on.exit(tryCatch(DBI::dbExecute(con, "DETACH DATABASE datasets"),
                     error = function(e) NULL), add = TRUE)

    # Fallback reference tables (same list as before)
    fallback_tables <- c(
      "plants_plt", "fertilizer_frt", "tillage_til", "pesticide_pst",
      "pathogens_pth", "urban_urb", "septic_sep", "snow_sno",
      "cntable_lum", "ovn_table_lum", "cons_prac_lum",
      "harv_ops", "fire_ops", "irr_ops", "sweep_ops", "chem_app_ops",
      "graze_ops",
      "bmpuser_str", "filterstrip_str", "grassedww_str",
      "septic_str", "tiledrain_str",
      "cal_parms_cal", "soils_lte_sol", "landuse_lum"
    )

    for (tbl in fallback_tables) {
      if (!(tbl %in% ds_tables)) next
      n <- .safe_table_count(con, tbl)
      if (n <= 0L) {
        tryCatch(
          DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS main.", tbl)),
          error = function(e) NULL
        )
        DBI::dbExecute(con, paste0(
          "CREATE TABLE main.", tbl,
          " AS SELECT * FROM datasets.", tbl
        ))
      }
    }

    # Decision tables fallback
    dtl_fb_available <- all(c("d_table_dtl", "d_table_dtl_cond",
      "d_table_dtl_cond_alt", "d_table_dtl_act", "d_table_dtl_act_out") %in%
      ds_tables)
    if (dtl_fb_available && .safe_table_count(con, "d_table_dtl") <= 0L) {
      .copy_decision_tables(con, "datasets")
    }

    DBI::dbExecute(con, "DETACH DATABASE datasets")
  }

  # ------------------------------------------------------------------
  # 3.  HAWQS project template (HAWQS-specific tables)
  # ------------------------------------------------------------------
  if (has_proj_hawqs) {
    con2 <- DBI::dbConnect(RSQLite::SQLite(), proj_hawqs_db)
    hawqs_proj_tables <- DBI::dbListTables(con2)
    DBI::dbDisconnect(con2)

    DBI::dbExecute(con, paste0(
      "ATTACH DATABASE '", proj_hawqs_db, "' AS proj_hawqs"))
    on.exit(tryCatch(DBI::dbExecute(con, "DETACH DATABASE proj_hawqs"),
                     error = function(e) NULL), add = TRUE)

    # HAWQS-specific tables to copy
    hawqs_tables <- c(
      "plant_HAWQS", "urban_HAWQS",
      "statsgo_ssurgo_lkey", "statsgo_ssurgo_lkey1"
    )
    # CDL landuse field tables
    cdl_tables <- paste0("landuse_fields_CDL_",
                         sprintf("%02d", seq_len(18)))
    hawqs_tables <- c(hawqs_tables, cdl_tables)

    for (tbl in hawqs_tables) {
      if (!(tbl %in% hawqs_proj_tables)) next
      n <- .safe_table_count(con, tbl)
      if (n <= 0L) {
        tryCatch(
          DBI::dbExecute(con,
            paste0("DROP TABLE IF EXISTS main.[", tbl, "]")),
          error = function(e) NULL
        )
        DBI::dbExecute(con, paste0(
          "CREATE TABLE main.[", tbl,
          "] AS SELECT * FROM proj_hawqs.[", tbl, "]"
        ))
      }
    }

    DBI::dbExecute(con, "DETACH DATABASE proj_hawqs")
  }

  invisible(NULL)
}


#' Copy decision tables from an ATTACHed database
#'
#' @param con DBI connection (with database already ATTACHed).
#' @param alias Character alias of the ATTACHed database
#'   (e.g. \code{"ref_hawqs"} or \code{"datasets"}).
#' @noRd
.copy_decision_tables <- function(con, alias) {
  tryCatch(
    DBI::dbExecute(con, "DROP TABLE IF EXISTS main.d_table_dtl"),
    error = function(e) NULL
  )
  DBI::dbExecute(con, paste0("
    CREATE TABLE main.d_table_dtl AS
    SELECT * FROM ", alias, ".d_table_dtl
    WHERE file_name IN ('lum.dtl', 'res_rel.dtl')
      AND (file_name != 'res_rel.dtl'
           OR name IN ('corps_med_res1','corps_med_res',
                       'wetland','drawdown_days','flood_season'))"))

  for (sub_tbl in c("d_table_dtl_cond", "d_table_dtl_cond_alt",
                    "d_table_dtl_act", "d_table_dtl_act_out")) {
    tryCatch(
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS main.", sub_tbl)),
      error = function(e) NULL
    )
  }

  DBI::dbExecute(con, paste0("
    CREATE TABLE main.d_table_dtl_cond AS
    SELECT c.* FROM ", alias, ".d_table_dtl_cond c
    INNER JOIN main.d_table_dtl d ON c.d_table_id = d.id"))

  DBI::dbExecute(con, paste0("
    CREATE TABLE main.d_table_dtl_cond_alt AS
    SELECT ca.* FROM ", alias, ".d_table_dtl_cond_alt ca
    INNER JOIN main.d_table_dtl_cond c ON ca.cond_id = c.id"))

  DBI::dbExecute(con, paste0("
    CREATE TABLE main.d_table_dtl_act AS
    SELECT a.* FROM ", alias, ".d_table_dtl_act a
    INNER JOIN main.d_table_dtl d ON a.d_table_id = d.id"))

  DBI::dbExecute(con, paste0("
    CREATE TABLE main.d_table_dtl_act_out AS
    SELECT ao.* FROM ", alias, ".d_table_dtl_act_out ao
    INNER JOIN main.d_table_dtl_act a ON ao.act_id = a.id"))
}

#' Ensure all required SWAT+ tables exist before writing files
#'
#' Creates any missing tables that \code{\link{write_config_files}} needs and
#' populates mandatory tables with sensible defaults (mirroring the Python
#' SWAT+ Editor \code{setup.py} initialisation).  Tables that already exist
#' are left untouched.
#'
#' @param con DBI connection to the project database.
#' @return Invisible \code{NULL}.
#' @keywords internal
ensure_write_tables <- function(con) {
  
  # ---- Populate reference tables from the bundled datasets database ----
  populate_from_datasets(con)
  
  # ---- helper: create a table only if it does not exist ----
  create_if_missing <- function(sql) {
    DBI::dbExecute(con, sql)
  }
  
  # ---- helper: insert a default row when a table is empty ----
  insert_if_empty <- function(tbl, sql) {
    n <- tryCatch(
      DBI::dbGetQuery(con,
                      paste0("SELECT COUNT(*) AS n FROM ", tbl))$n,
      error = function(e) 0L)
    if (n == 0L) DBI::dbExecute(con, sql)
  }
  
  existing <- DBI::dbListTables(con)
  
  # ==================================================================
  # 1. Simulation tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS time_sim (
      id INTEGER PRIMARY KEY,
      day_start INTEGER, yrc_start INTEGER,
      day_end   INTEGER, yrc_end   INTEGER,
      step INTEGER DEFAULT 0
    )")
  insert_if_empty("time_sim",
                  "INSERT INTO time_sim (day_start, yrc_start, day_end, yrc_end, step)
     VALUES (0, 1980, 0, 1985, 0)")
  
  create_if_missing("
    CREATE TABLE IF NOT EXISTS print_prt (
      id INTEGER PRIMARY KEY,
      nyskip INTEGER, day_start INTEGER, yrc_start INTEGER,
      day_end INTEGER, yrc_end INTEGER, interval INTEGER,
      csvout INTEGER, dbout INTEGER, cdfout INTEGER,
      crop_yld TEXT DEFAULT 'b', mgtout INTEGER, hydcon INTEGER,
      fdcout INTEGER
    )")
  insert_if_empty("print_prt",
                  "INSERT INTO print_prt
       (nyskip, day_start, yrc_start, day_end, yrc_end, interval,
        csvout, dbout, cdfout, crop_yld, mgtout, hydcon, fdcout)
     VALUES (1, 0, 0, 0, 0, 1, 0, 0, 0, 'b', 0, 0, 0)")
  
  create_if_missing("
    CREATE TABLE IF NOT EXISTS print_prt_object (
      id INTEGER PRIMARY KEY,
      print_prt_id INTEGER,
      name TEXT, daily INTEGER, monthly INTEGER,
      yearly INTEGER, avann INTEGER,
      FOREIGN KEY (print_prt_id) REFERENCES print_prt(id)
    )")
  
  # Default print objects (52 rows) --------------------------------
  prt_id <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM print_prt ORDER BY id LIMIT 1")$id,
    error = function(e) 1L)
  
  if (.safe_table_count(con, "print_prt_object") == 0L) {
    # Names where yearly=1, avann=1
    active <- c(
      "basin_wb", "basin_nb", "basin_ls", "basin_pw", "basin_aqu",
      "basin_res", "basin_cha", "basin_sd_cha", "basin_psc",
      "region_wb", "region_nb", "region_ls", "region_pw", "region_aqu",
      "region_res", "region_sd_cha", "region_psc", "water_allo",
      "lsunit_wb", "lsunit_nb", "lsunit_ls", "lsunit_pw",
      "hru_wb", "hru_nb", "hru_ls", "hru_pw",
      "hru-lte_wb", "hru-lte_nb", "hru-lte_ls", "hru-lte_pw",
      "channel", "channel_sd", "aquifer", "reservoir", "recall",
      "hyd", "ru", "pest")
    # Names where all flags = 0
    inactive <- c(
      "basin_salt", "hru_salt", "ru_salt", "aqu_salt",
      "channel_salt", "res_salt", "wetland_salt",
      "basin_cs", "hru_cs", "ru_cs", "aqu_cs",
      "channel_cs", "res_cs", "wetland_cs")
    
    rows <- data.frame(
      print_prt_id = prt_id,
      name     = c(active, inactive),
      daily    = 0L,
      monthly  = 0L,
      yearly   = c(rep(1L, length(active)),  rep(0L, length(inactive))),
      avann    = c(rep(1L, length(active)),  rep(0L, length(inactive))),
      stringsAsFactors = FALSE)
    DBI::dbWriteTable(con, "print_prt_object", rows, append = TRUE)
  }
  
  create_if_missing("
    CREATE TABLE IF NOT EXISTS object_prt (
      id INTEGER PRIMARY KEY,
      ob_typ TEXT, ob_typ_no INTEGER, hyd_typ TEXT, filename TEXT
    )")
  
  create_if_missing("
    CREATE TABLE IF NOT EXISTS object_cnt (
      id INTEGER PRIMARY KEY,
      name TEXT,
      obj INTEGER DEFAULT 0, hru INTEGER DEFAULT 0,
      lhru INTEGER DEFAULT 0, rtu INTEGER DEFAULT 0,
      mfl INTEGER DEFAULT 0, aqu INTEGER DEFAULT 0,
      cha INTEGER DEFAULT 0, res INTEGER DEFAULT 0,
      rec INTEGER DEFAULT 0, exco INTEGER DEFAULT 0,
      dlr INTEGER DEFAULT 0, can INTEGER DEFAULT 0,
      pmp INTEGER DEFAULT 0, out INTEGER DEFAULT 0,
      lcha INTEGER DEFAULT 0, aqu2d INTEGER DEFAULT 0,
      hrd INTEGER DEFAULT 0, wro INTEGER DEFAULT 0
    )")
  
  cfg_name <- tryCatch(
    DBI::dbGetQuery(con, "SELECT project_name FROM project_config LIMIT 1")$project_name,
    error = function(e) "default")
  if (is.null(cfg_name) || is.na(cfg_name) || cfg_name == "") cfg_name <- "default"
  insert_if_empty("object_cnt", paste0(
    "INSERT INTO object_cnt (name) VALUES ('", cfg_name, "')"))
  
  create_if_missing("
    CREATE TABLE IF NOT EXISTS constituents_cs (
      id INTEGER PRIMARY KEY,
      name TEXT, pest_coms TEXT, path_coms TEXT,
      hmet_coms TEXT, salt_coms TEXT
    )")
  
  # ==================================================================
  # 2. Basin tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS codes_bsn (
      id INTEGER PRIMARY KEY,
      pet_file TEXT, wq_file TEXT,
      pet INTEGER, event INTEGER, crack INTEGER, swift_out INTEGER,
      sed_det INTEGER, rte_cha INTEGER, deg_cha INTEGER, wq_cha INTEGER,
      nostress INTEGER, cn INTEGER, c_fact INTEGER, carbon INTEGER,
      lapse INTEGER, uhyd INTEGER, sed_cha INTEGER, tiledrain INTEGER,
      wtable INTEGER, soil_p INTEGER, gampt INTEGER,
      atmo_dep TEXT, stor_max INTEGER, i_fpwet INTEGER,
      gwflow INTEGER DEFAULT 0
    )")
  insert_if_empty("codes_bsn",
                  "INSERT INTO codes_bsn
       (pet_file, wq_file,
        pet, event, crack, swift_out, sed_det, rte_cha, deg_cha, wq_cha,
        nostress, cn, c_fact, carbon, lapse, uhyd, sed_cha, tiledrain,
        wtable, soil_p, gampt, atmo_dep, stor_max, i_fpwet, gwflow)
     VALUES (NULL, NULL,
             1, 0, 0, 1, 0, 0, 0, 1,
             0, 0, 0, 0, 0, 1, 0, 0,
             0, 0, 0, 'a', 0, 1, 0)")
  
  create_if_missing("
    CREATE TABLE IF NOT EXISTS parameters_bsn (
      id INTEGER PRIMARY KEY,
      lai_noevap REAL, sw_init REAL, surq_lag REAL,
      adj_pkrt REAL, adj_pkrt_sed REAL, lin_sed REAL, exp_sed REAL,
      orgn_min REAL, n_uptake REAL, p_uptake REAL,
      n_perc REAL, p_perc REAL, p_soil REAL, p_avail REAL,
      rsd_decomp REAL, pest_perc REAL,
      msk_co1 REAL, msk_co2 REAL, msk_x REAL,
      nperco_lchtile REAL, evap_adj REAL, scoef REAL,
      denit_exp REAL, denit_frac REAL, man_bact REAL,
      adj_uhyd REAL, cn_froz REAL, dorm_hr REAL,
      plaps REAL, tlaps REAL, n_fix_max REAL,
      rsd_decay REAL, rsd_cover REAL, urb_init_abst REAL,
      petco_pmpt REAL, uhyd_alpha REAL,
      splash REAL, rill REAL, surq_exp REAL, cov_mgt REAL,
      cha_d50 REAL, co2 REAL, day_lag_max REAL,
      igen INTEGER
    )")
  insert_if_empty("parameters_bsn",
                  "INSERT INTO parameters_bsn
       (lai_noevap, sw_init, surq_lag, adj_pkrt, adj_pkrt_sed,
        lin_sed, exp_sed, orgn_min, n_uptake, p_uptake,
        n_perc, p_perc, p_soil, p_avail, rsd_decomp, pest_perc,
        msk_co1, msk_co2, msk_x, nperco_lchtile, evap_adj, scoef,
        denit_exp, denit_frac, man_bact, adj_uhyd, cn_froz, dorm_hr,
        plaps, tlaps, n_fix_max, rsd_decay, rsd_cover, urb_init_abst,
        petco_pmpt, uhyd_alpha, splash, rill, surq_exp, cov_mgt,
        cha_d50, co2, day_lag_max, igen)
     VALUES (3.0, 0.0, 4.0, 1.0, 484.0,
             0.0, 0.0, 0.0, 20.0, 20.0,
             0.1, 10.0, 175.0, 0.4, 0.05, 0.5,
             0.75, 0.25, 0.2, 0.5, 0.6, 1.0,
             1.4, 1.3, 0.15, 0.0, 0.0, 0.0,
             0.0, 0.0, 20.0, 0.01, 0.3, 1.0,
             1.0, 5.0, 1.0, 0.7, 1.2, 0.03,
             50.0, 400.0, 0.0, 0)")
  
  # ==================================================================
  # 3. Climate tables (weather_sta_cli / weather_wgn_cli already in
  #    create_project_db; just make sure they exist)
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS weather_sta_cli (
      id INTEGER PRIMARY KEY,
      name TEXT UNIQUE NOT NULL,
      lat REAL, lon REAL, elev REAL,
      pcp TEXT, tmp TEXT, slr TEXT, hmd TEXT, wnd TEXT, pet TEXT,
      atmo_dep TEXT, wgn_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS weather_wgn_cli (
      id INTEGER PRIMARY KEY,
      name TEXT UNIQUE NOT NULL,
      lat REAL, lon REAL, elev REAL, rain_yrs REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS weather_wgn_cli_mon (
      id INTEGER PRIMARY KEY,
      weather_wgn_cli_id INTEGER NOT NULL,
      month INTEGER NOT NULL,
      tmp_max_ave REAL, tmp_min_ave REAL,
      tmp_max_sd REAL, tmp_min_sd REAL,
      pcp_ave REAL, pcp_sd REAL, pcp_skew REAL,
      wet_dry REAL, wet_wet REAL, pcp_days REAL, pcp_hhr REAL,
      slr_ave REAL, dew_ave REAL, wnd_ave REAL,
      FOREIGN KEY (weather_wgn_cli_id) REFERENCES weather_wgn_cli(id)
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS weather_file (
      id INTEGER PRIMARY KEY,
      filename TEXT, type TEXT, lat REAL, lon REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS atmo_cli (
      id INTEGER PRIMARY KEY,
      filename TEXT, sim_sta_id INTEGER
    )")
  
  # ==================================================================
  # 4. Connection tables (13 types + their _con_out partners)
  # ==================================================================
  con_types <- c("hru", "hru_lte", "rout_unit", "modflow",
                 "aquifer", "aquifer2d", "channel", "reservoir",
                 "recall", "exco", "delratio", "outlet", "chandeg")
  for (ct in con_types) {
    tbl <- paste0(ct, "_con")
    create_if_missing(paste0("
      CREATE TABLE IF NOT EXISTS ", tbl, " (
        id INTEGER PRIMARY KEY,
        name TEXT, gis_id INTEGER, area REAL, lat REAL, lon REAL,
        elev REAL, wst_id INTEGER, cst_id INTEGER,
        ovfl INTEGER DEFAULT 0, rule INTEGER DEFAULT 0,
        ob_typ TEXT, obj_id INTEGER
      )"))
    create_if_missing(paste0("
      CREATE TABLE IF NOT EXISTS ", tbl, "_out (
        id INTEGER PRIMARY KEY,
        ", tbl, "_id INTEGER,
        order_id INTEGER, obj_typ TEXT, obj_id INTEGER,
        hyd_typ TEXT, frac REAL,
        FOREIGN KEY (", tbl, "_id) REFERENCES ", tbl, "(id)
      )"))
  }
  
  # ==================================================================
  # 5. Channel tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS initial_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      org_min_id INTEGER, pest_id INTEGER, path_id INTEGER,
      hmet_id INTEGER, salt_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS channel_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      init_id INTEGER, hyd_id INTEGER, sed_id INTEGER, nut_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS hydrology_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      wd REAL, dp REAL, slp REAL, len REAL, mann REAL,
      k REAL, erod_fact REAL, cov_fact REAL,
      hc_cov REAL, eq_slp REAL, d50 REAL,
      clay REAL, carbon REAL, dry_bd REAL,
      side_slp REAL, bed_load REAL,
      fps REAL, fpn REAL, n_conc REAL, p_conc REAL, p_bio REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS sediment_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      cha_cov INTEGER, bed_load REAL,
      bank_erode REAL, channel_erode REAL,
      shear_bank REAL, shear_bed REAL,
      hc_erod REAL, hc_ht REAL, hc_len REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS nutrients_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      ptl_n REAL, ptl_p REAL, algae REAL,
      bod REAL, cbod REAL, dis_ox REAL,
      nh3_n REAL, no2_n REAL, no3_n REAL,
      sol_p REAL, ch_onp REAL, ch_onn REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS channel_lte_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      hyd_id INTEGER, init_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS hyd_sed_lte_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      wd REAL, dp REAL, slp REAL, len REAL, mann REAL,
      k REAL, cov_fact REAL, wd_rto REAL, eq_slp REAL, d50 REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS temperature_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      lat REAL, lon REAL, elev REAL
    )")
  
  # ==================================================================
  # 6. Reservoir tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS initial_res (
      id INTEGER PRIMARY KEY, name TEXT,
      org_min_id INTEGER, pest_id INTEGER, path_id INTEGER,
      hmet_id INTEGER, salt_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS reservoir_res (
      id INTEGER PRIMARY KEY, name TEXT,
      init_id INTEGER, hyd_id INTEGER, sed_id INTEGER, nut_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS hydrology_res (
      id INTEGER PRIMARY KEY, name TEXT,
      yr_op REAL, mon_op REAL, area_ps REAL,
      vol_ps REAL, area_es REAL, vol_es REAL,
      k REAL, evap_co REAL, shp_co1 REAL, shp_co2 REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS sediment_res (
      id INTEGER PRIMARY KEY, name TEXT,
      sed_stl REAL, velsetl_d50 REAL, velsetl_stl REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS nutrients_res (
      id INTEGER PRIMARY KEY, name TEXT,
      mid_start INTEGER, mid_end INTEGER,
      mid_n_stl REAL, n_stl REAL, mid_p_stl REAL, p_stl REAL,
      chla_co REAL, secchi_co REAL, theta_n REAL,
      theta_p REAL, n_min_stl REAL, p_min_stl REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS weir_res (
      id INTEGER PRIMARY KEY, name TEXT,
      num_steps INTEGER, hyd_flo TEXT, hyd_flo2 TEXT
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS wetland_wet (
      id INTEGER PRIMARY KEY, name TEXT,
      init_id INTEGER, hyd_id INTEGER, sed_id INTEGER, nut_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS hydrology_wet (
      id INTEGER PRIMARY KEY, name TEXT,
      psa REAL, pdep REAL, esa REAL, edep REAL,
      k REAL, evap REAL
    )")
  
  # ==================================================================
  # 7. Routing unit tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS rout_unit_def_con (
      id INTEGER PRIMARY KEY, name TEXT, rtu_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS rout_unit_ele (
      id INTEGER PRIMARY KEY, name TEXT,
      rtu_id INTEGER, obj_id INTEGER, obj_typ TEXT, frac REAL,
      dlr_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS rout_unit_rtu (
      id INTEGER PRIMARY KEY, name TEXT,
      topo_id INTEGER, field_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS rout_unit_dr (
      id INTEGER PRIMARY KEY, name TEXT, dr_id INTEGER
    )")
  
  # ==================================================================
  # 8. HRU tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS hru_data_hru (
      id INTEGER PRIMARY KEY, name TEXT,
      topo_id INTEGER, hydro_id INTEGER, soil_id INTEGER,
      lu_mgt_id INTEGER, soil_plant_ini_id INTEGER,
      surf_stor TEXT, snow_id INTEGER, field_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS hru_lte_hru (
      id INTEGER PRIMARY KEY, name TEXT,
      cn2 REAL, usle_k REAL, usle_ls REAL, usle_p REAL,
      ovn REAL, elev REAL, slope REAL, slope_len REAL,
      lat REAL, perco REAL, eta REAL, pet REAL,
      strsol REAL, strtmp REAL, sw REAL, awc REAL
    )")
  
  # ==================================================================
  # 9. DR (delivery ratio) tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS delratio_del (
    id INTEGER PRIMARY KEY, name TEXT, om_id INTEGER)")
  create_if_missing("CREATE TABLE IF NOT EXISTS dr_om_del (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS dr_pest_del (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS dr_path_del (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS dr_hmet_del (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS dr_salt_del (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # ==================================================================
  # 10. Aquifer tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS initial_aqu (
      id INTEGER PRIMARY KEY, name TEXT,
      org_min_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS aquifer_aqu (
      id INTEGER PRIMARY KEY, name TEXT,
      init_id INTEGER, gw_flo REAL, dep_bot REAL,
      dep_wt REAL, no3_n REAL, sol_p REAL,
      ptl_n REAL, ptl_p REAL,
      bf_max REAL, alpha_bf REAL, revap REAL,
      rchg_dp REAL, spec_yld REAL, hl_no3n REAL,
      flo_min REAL, revap_min REAL
    )")
  
  # ==================================================================
  # 11. Water rights
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS water_allocation_wro (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS element_wro (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS define_wro (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # ==================================================================
  # 12. Link tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS chan_surf_lin (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS chan_aqu_lin (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # ==================================================================
  # 13. Basin tables (already created above)
  # ==================================================================
  
  # ==================================================================
  # 14. Hydrology tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS hydrology_hyd (
      id INTEGER PRIMARY KEY, name TEXT,
      lat_ttime REAL, lat_sed REAL, can_max REAL,
      esco REAL, epco REAL, orgn_enrich REAL,
      orgp_enrich REAL, cn3_swf REAL,
      bio_mix REAL, perco REAL, lat_orgn REAL,
      lat_orgp REAL, harg_pet REAL,
      latq_co REAL, cn2 REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS topography_hyd (
      id INTEGER PRIMARY KEY, name TEXT,
      slp REAL, slp_len REAL, lat_len REAL,
      dist_cha REAL, depos REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS field_fld (
      id INTEGER PRIMARY KEY, name TEXT,
      len REAL, wd REAL, ang REAL
    )")
  
  # ==================================================================
  # 15. EXCO tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS exco_exc (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS exco_om_exc (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS exco_pest_exc (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS exco_path_exc (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS exco_hmet_exc (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS exco_salt_exc (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # ==================================================================
  # 16. Recall tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS recall_rec (
    id INTEGER PRIMARY KEY, name TEXT, rec_typ INTEGER)")
  create_if_missing("CREATE TABLE IF NOT EXISTS recall_dat (
    id INTEGER PRIMARY KEY, recall_rec_id INTEGER,
    yr INTEGER, t_step INTEGER,
    flo REAL, sed REAL, orgn REAL, sedp REAL,
    no3 REAL, solp REAL, chla REAL, nh3 REAL,
    no2 REAL, cbod REAL, dox REAL, sand REAL,
    silt REAL, clay REAL, sag REAL, lag REAL,
    gravel REAL, tmp REAL)")
  
  # ==================================================================
  # 17. Structural tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS tiledrain_str (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS septic_str (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS filterstrip_str (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS grassedww_str (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS bmpuser_str (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # ==================================================================
  # 18. HRU parameter database tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS plants_plt (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS fertilizer_frt (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS tillage_til (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS pesticide_pst (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS pathogens_pth (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS metals_mtl (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS salts_slt (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS urban_urb (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS septic_sep (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS snow_sno (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # ==================================================================
  # 19. Operations tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS harv_ops (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS graze_ops (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS irr_ops (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS chem_app_ops (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS fire_ops (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS sweep_ops (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # ==================================================================
  # 20. Land use management tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS landuse_lum (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS management_sch (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS cntable_lum (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS cons_prac_lum (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS ovn_table_lum (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # ==================================================================
  # 21. Calibration / change tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS cal_parms_cal (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS calibration_cal (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS codes_sft (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS wb_parms_sft (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS water_balance_sft (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS ch_sed_budget_sft (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS ch_sed_parms_sft (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS plant_parms_sft (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS plant_gro_sft (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # ==================================================================
  # 22. Initial condition tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS plant_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS soil_plant_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS om_water_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS pest_hru_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS pest_water_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS path_hru_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS path_water_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS hmet_hru_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS hmet_water_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS salt_hru_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS salt_water_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # ==================================================================
  # 23. Soils tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS soils_sol (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS nutrients_sol (
    id INTEGER PRIMARY KEY, name TEXT, exp_co REAL)")
  create_if_missing("CREATE TABLE IF NOT EXISTS soils_lte_sol (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # ==================================================================
  # 24. Decision table tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS d_table_dtl (
      id INTEGER PRIMARY KEY, name TEXT, file_name TEXT,
      description TEXT
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS d_table_dtl_cond (
      id INTEGER PRIMARY KEY, d_table_dtl_id INTEGER,
      var TEXT, obj TEXT, obj_num INTEGER,
      lim_var TEXT, lim_op TEXT, lim_const REAL,
      alt TEXT
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS d_table_dtl_act (
      id INTEGER PRIMARY KEY, d_table_dtl_id INTEGER,
      act_typ TEXT, obj TEXT, obj_num INTEGER,
      name TEXT, option TEXT, const1 REAL,
      const2 REAL, fp TEXT
    )")
  
  # ==================================================================
  # 25. Region tables
  # ==================================================================
  region_tbls <- c("ls_unit_ele", "ls_unit_def", "ls_reg_ele", "ls_reg_def",
                   "ch_catunit_ele", "ch_catunit_def", "ch_reg_def",
                   "aqu_catunit_ele", "aqu_catunit_def", "aqu_reg_def",
                   "res_catunit_ele", "res_catunit_def", "res_reg_def",
                   "rec_catunit_ele", "rec_catunit_def", "rec_reg_def")
  for (rt in region_tbls) {
    create_if_missing(paste0(
      "CREATE TABLE IF NOT EXISTS ", rt, " (
         id INTEGER PRIMARY KEY, name TEXT
       )"))
  }
  
  # ==================================================================
  # 26. Herd tables (stub - matching Python write_herd() which is pass)
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS animal_hrd (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS herd_hrd (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS ranch_hrd (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # ==================================================================
  # 27. gwflow tables (groundwater flow module)
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_base (
      id INTEGER PRIMARY KEY,
      cell_size REAL, row_count INTEGER, col_count INTEGER,
      boundary_conditions INTEGER DEFAULT 2,
      recharge INTEGER DEFAULT 1, soil_transfer INTEGER DEFAULT 0,
      saturation_excess INTEGER DEFAULT 0, external_pumping INTEGER DEFAULT 0,
      tile_drainage INTEGER DEFAULT 0, reservoir_exchange INTEGER DEFAULT 0,
      wetland_exchange INTEGER DEFAULT 0, floodplain_exchange INTEGER DEFAULT 0,
      canal_seepage INTEGER DEFAULT 0, solute_transport INTEGER DEFAULT 0,
      timestep_balance REAL DEFAULT 1.0,
      daily_output INTEGER DEFAULT 0, annual_output INTEGER DEFAULT 0,
      aa_output INTEGER DEFAULT 0,
      recharge_delay REAL DEFAULT 0,
      river_depth REAL DEFAULT 0,
      daily_output_row INTEGER DEFAULT 0, daily_output_col INTEGER DEFAULT 0,
      resbed_thickness REAL DEFAULT 0, resbed_k REAL DEFAULT 0,
      wet_thickness REAL DEFAULT 0,
      tile_depth REAL DEFAULT 0, tile_area REAL DEFAULT 0,
      tile_k REAL DEFAULT 0, tile_groups INTEGER DEFAULT 0,
      transport_steps INTEGER DEFAULT 1, disp_coef REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_zone (
      id INTEGER PRIMARY KEY, zone_id INTEGER,
      aquifer_k REAL, specific_yield REAL,
      streambed_k REAL, streambed_thickness REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_grid (
      id INTEGER PRIMARY KEY, cell_id INTEGER,
      status INTEGER DEFAULT 0, elevation REAL DEFAULT 0,
      aquifer_thickness REAL DEFAULT 0, zone INTEGER DEFAULT 0,
      extinction_depth REAL DEFAULT 0, initial_head REAL DEFAULT 0,
      tile INTEGER DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_out_days (
      id INTEGER PRIMARY KEY, year INTEGER, jday INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_obs_locs (
      id INTEGER PRIMARY KEY, cell_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_solutes (
      id INTEGER PRIMARY KEY, solute_name TEXT,
      sorption REAL DEFAULT 0, rate_const REAL DEFAULT 0,
      canal_irr REAL DEFAULT 0,
      init_data TEXT DEFAULT 'single', init_conc REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_init_conc (
      id INTEGER PRIMARY KEY, cell_id INTEGER,
      init_no3 REAL DEFAULT 0, init_p REAL DEFAULT 0,
      init_so4 REAL DEFAULT 0, init_ca REAL DEFAULT 0,
      init_mg REAL DEFAULT 0, init_na REAL DEFAULT 0,
      init_k REAL DEFAULT 0, init_cl REAL DEFAULT 0,
      init_co3 REAL DEFAULT 0, init_hco3 REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_hrucell (
      id INTEGER PRIMARY KEY, cell_id INTEGER, hru INTEGER,
      area_m2 REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_fpcell (
      id INTEGER PRIMARY KEY, cell_id INTEGER, channel_id INTEGER,
      area_m2 REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_rivcell (
      id INTEGER PRIMARY KEY, cell_id INTEGER, channel INTEGER,
      length_m REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_lsucell (
      id INTEGER PRIMARY KEY, cell_id INTEGER, lsu INTEGER,
      area_m2 REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_rescell (
      id INTEGER PRIMARY KEY, cell_id INTEGER, res_id INTEGER,
      res_stage REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_wetland (
      id INTEGER PRIMARY KEY, wet_id INTEGER, thickness REAL DEFAULT 0
    )")
  
  # ==================================================================
  # 26. file_cio_classification + file_cio
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS file_cio_classification (
      id INTEGER PRIMARY KEY, name TEXT
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS file_cio (
      id INTEGER PRIMARY KEY,
      classification_id INTEGER,
      order_in_class INTEGER,
      file_name TEXT,
      FOREIGN KEY (classification_id)
        REFERENCES file_cio_classification(id)
    )")
  
  if (.safe_table_count(con, "file_cio_classification") == 0L) {
    cls_names <- c(
      "simulation", "basin", "climate", "connect", "channel",
      "reservoir", "routing_unit", "hru", "exco", "recall",
      "dr", "aquifer", "herd", "water_rights", "link",
      "hydrology", "structural", "hru_parm_db", "ops", "lum",
      "chg", "init", "soils", "decision_table", "regions",
      "pcp_path", "tmp_path", "slr_path", "hmd_path", "wnd_path",
      "out_path")
    
    cls_df <- data.frame(name = cls_names, stringsAsFactors = FALSE)
    DBI::dbWriteTable(con, "file_cio_classification", cls_df, append = TRUE)
    
    # Build file_cio records  (classification_id, order_in_class, file_name)
    # ------ classification_id values map to cls_names insertion order ------
    cls_lookup <- DBI::dbGetQuery(con,
                                  "SELECT id, name FROM file_cio_classification ORDER BY id")
    cid <- function(name) cls_lookup$id[cls_lookup$name == name]
    
    file_rows <- data.frame(
      classification_id = integer(0),
      order_in_class    = integer(0),
      file_name         = character(0),
      stringsAsFactors  = FALSE)
    
    add_files <- function(section, files) {
      for (i in seq_along(files)) {
        file_rows[nrow(file_rows) + 1L, ] <<- list(cid(section), i, files[i])
      }
    }
    
    add_files("simulation", c("time.sim", "print.prt", "object.prt",
                              "object.cnt", "constituents.cs"))
    add_files("basin", c("codes.bsn", "parameters.bsn"))
    add_files("climate", c("weather-sta.cli", "weather-wgn.cli",
                           "pet.cli", "pcp.cli", "tmp.cli",
                           "slr.cli", "hmd.cli", "wnd.cli", "atmodep.cli"))
    add_files("connect", c("hru.con", "hru-lte.con", "rout_unit.con",
                           "gwflow.con", "aquifer.con", "aquifer2d.con",
                           "channel.con", "reservoir.con", "recall.con",
                           "exco.con", "delratio.con", "outlet.con",
                           "chandeg.con"))
    add_files("channel", c("initial.cha", "channel.cha", "hydrology.cha",
                           "sediment.cha", "nutrients.cha",
                           "channel-lte.cha", "hyd-sed-lte.cha",
                           "temperature.cha"))
    add_files("reservoir", c("initial.res", "reservoir.res", "hydrology.res",
                             "sediment.res", "nutrients.res", "weir.res",
                             "wetland.wet", "hydrology.wet"))
    add_files("routing_unit", c("rout_unit.def", "rout_unit.ele",
                                "rout_unit.rtu", "rout_unit.dr"))
    add_files("hru", c("hru-data.hru", "hru-lte.hru"))
    add_files("exco", c("exco.exc", "exco_om.exc", "exco_pest.exc",
                        "exco_path.exc", "exco_hmet.exc", "exco_salt.exc"))
    add_files("recall", "recall.rec")
    add_files("dr", c("delratio.del", "dr_om.del", "dr_pest.del",
                      "dr_path.del", "dr_hmet.del", "dr_salt.del"))
    add_files("aquifer", c("initial.aqu", "aquifer.aqu"))
    add_files("herd", c("animal.hrd", "herd.hrd", "ranch.hrd"))
    add_files("water_rights", c("water_allocation.wro", "element.wro",
                                "define.wro"))
    add_files("link", c("chan-surf.lin", "chan-aqu.lin"))
    add_files("hydrology", c("hydrology.hyd", "topography.hyd", "field.fld"))
    add_files("structural", c("tiledrain.str", "septic.str",
                              "filterstrip.str", "grassedww.str",
                              "bmpuser.str"))
    add_files("hru_parm_db", c("plants.plt", "fertilizer.frt", "tillage.til",
                               "pesticide.pst", "pathogens.pth",
                               "metals.mtl", "salts.slt", "urban.urb",
                               "septic.sep", "snow.sno"))
    add_files("ops", c("harv.ops", "graze.ops", "irr.ops",
                       "chem_app.ops", "fire.ops", "sweep.ops"))
    add_files("lum", c("landuse.lum", "management.sch", "cntable.lum",
                       "cons_prac.lum", "ovn_table.lum"))
    add_files("chg", c("cal_parms.cal", "calibration.cal", "codes.sft",
                       "wb_parms.sft", "water_balance.sft",
                       "ch_sed_budget.sft", "ch_sed_parms.sft",
                       "plant_parms.sft", "plant_gro.sft"))
    add_files("init", c("plant.ini", "soil_plant.ini", "om_water.ini",
                        "pest_hru.ini", "pest_water.ini",
                        "path_hru.ini", "path_water.ini",
                        "hmet_hru.ini", "hmet_water.ini",
                        "salt_hru.ini", "salt_water.ini"))
    add_files("soils", c("soils.sol", "nutrients.sol", "soils_lte.sol"))
    add_files("decision_table", c("lum.dtl", "res_rel.dtl",
                                  "scen_lu.dtl", "flo_con.dtl"))
    add_files("regions", c(
      "ls_unit.ele", "ls_unit.def", "ls_reg.ele", "ls_reg.def",
      "ls_cal.reg",
      "ch_catunit.ele", "ch_catunit.def", "ch_reg.def",
      "aqu_catunit.ele", "aqu_catunit.def", "aqu_reg.def",
      "res_catunit.ele", "res_catunit.def", "res_reg.def",
      "rec_catunit.ele", "rec_catunit.def", "rec_reg.def"))
    
    DBI::dbWriteTable(con, "file_cio", file_rows, append = TRUE)
  }
  
  # ==================================================================
  # 28. Missing _item / _elem / sub-tables required by Python QSWAT+
  # ==================================================================
  
  # -- Initial condition _item tables --
  ini_item_tables <- c(
    "plant_ini_item", "pest_hru_ini_item", "pest_water_ini_item",
    "path_hru_ini_item", "path_water_ini_item",
    "hmet_hru_ini_item", "hmet_water_ini_item",
    "salt_hru_ini_item", "salt_water_ini_item"
  )
  for (tbl in ini_item_tables) {
    create_if_missing(paste0(
      "CREATE TABLE IF NOT EXISTS ", tbl, " (
         id INTEGER PRIMARY KEY, name TEXT
       )"))
  }
  
  # -- Region _elem tables --
  region_elem_tables <- c(
    "aqu_catunit_def_elem", "aqu_reg_def_elem",
    "ch_catunit_def_elem", "ch_reg_def_elem",
    "res_catunit_def_elem", "res_reg_def_elem",
    "rec_catunit_def_elem", "rec_reg_def_elem"
  )
  for (tbl in region_elem_tables) {
    create_if_missing(paste0(
      "CREATE TABLE IF NOT EXISTS ", tbl, " (
         id INTEGER PRIMARY KEY, name TEXT
       )"))
  }
  
  # -- Decision table sub-tables --
  create_if_missing("
    CREATE TABLE IF NOT EXISTS d_table_dtl_cond_alt (
      id INTEGER PRIMARY KEY, cond_id INTEGER, name TEXT
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS d_table_dtl_act_out (
      id INTEGER PRIMARY KEY, act_id INTEGER, name TEXT
    )")
  
  # -- Soft calibration _item tables --
  sft_item_tables <- c(
    "ch_sed_budget_sft_item", "plant_gro_sft_item",
    "plant_parms_sft_item", "water_balance_sft_item"
  )
  for (tbl in sft_item_tables) {
    create_if_missing(paste0(
      "CREATE TABLE IF NOT EXISTS ", tbl, " (
         id INTEGER PRIMARY KEY, name TEXT
       )"))
  }
  
  # -- Calibration sub-tables --
  create_if_missing("CREATE TABLE IF NOT EXISTS calibration_cal_cond (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS calibration_cal_elem (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # -- Climate sub-tables --
  create_if_missing("
    CREATE TABLE IF NOT EXISTS atmo_cli_sta (
      id INTEGER PRIMARY KEY, atmo_cli_id INTEGER,
      weather_sta_cli_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS atmo_cli_sta_value (
      id INTEGER PRIMARY KEY, sta_id INTEGER,
      timestep INTEGER, nh4_wet REAL, no3_wet REAL,
      nh4_dry REAL, no3_dry REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS weather_sta_cli_scale (
      id INTEGER PRIMARY KEY, weather_sta_cli_id INTEGER,
      name TEXT
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS wind_dir_cli (
      id INTEGER PRIMARY KEY, name TEXT
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS print_prt_aa_int (
      id INTEGER PRIMARY KEY, print_prt_id INTEGER, aa_int_cnt INTEGER
    )")
  
  # -- DR (delivery ratio) _col and _val tables --
  dr_types <- c("pest", "path", "hmet", "salt")
  for (dt in dr_types) {
    create_if_missing(paste0(
      "CREATE TABLE IF NOT EXISTS dr_", dt, "_col (
         id INTEGER PRIMARY KEY, name TEXT
       )"))
    create_if_missing(paste0(
      "CREATE TABLE IF NOT EXISTS dr_", dt, "_val (
         id INTEGER PRIMARY KEY, name TEXT
       )"))
  }
  
  # -- EXCO _col and _val tables --
  exco_types <- c("pest", "path", "hmet", "salt")
  for (et in exco_types) {
    create_if_missing(paste0(
      "CREATE TABLE IF NOT EXISTS exco_", et, "_col (
         id INTEGER PRIMARY KEY, name TEXT
       )"))
    create_if_missing(paste0(
      "CREATE TABLE IF NOT EXISTS exco_", et, "_val (
         id INTEGER PRIMARY KEY, name TEXT
       )"))
  }
  
  # -- Link _ob tables --
  create_if_missing("CREATE TABLE IF NOT EXISTS chan_aqu_lin_ob (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS chan_surf_lin_ob (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # -- Management sub-tables --
  create_if_missing("CREATE TABLE IF NOT EXISTS management_sch_auto (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS management_sch_op (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # -- Soils layer table --
  create_if_missing("CREATE TABLE IF NOT EXISTS soils_sol_layer (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # -- Landuse lookup table --
  create_if_missing("CREATE TABLE IF NOT EXISTS landuse_lookup (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  # -- Salt module tables --
  salt_tables <- c(
    "salt_module", "salt_aqu_ini", "salt_atmo_cli",
    "salt_channel_ini", "salt_fertilizer_frt", "salt_hru_ini_cs",
    "salt_irrigation", "salt_plants", "salt_plants_flags",
    "salt_recall_dat", "salt_recall_rec", "salt_res_ini",
    "salt_road", "salt_urban"
  )
  for (tbl in salt_tables) {
    create_if_missing(paste0(
      "CREATE TABLE IF NOT EXISTS ", tbl, " (
         id INTEGER PRIMARY KEY, name TEXT
       )"))
  }
  
  # -- Water allocation sub-tables --
  create_if_missing("CREATE TABLE IF NOT EXISTS water_allocation_dmd_ob (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS water_allocation_dmd_ob_src (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS water_allocation_src_ob (
    id INTEGER PRIMARY KEY, name TEXT)")
  
  invisible(NULL)
}
