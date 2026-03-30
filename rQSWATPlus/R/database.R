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

  # Write project configuration
  .write_project_config(con, project, db_file)

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
      wnd_dir  VARCHAR (255),
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
          hyd_type = "tot",
          sinkid = ds_wsno[1],
          sinkcat = "sub",
          percent = 100,
          stringsAsFactors = FALSE
        )
      }
    } else {
      # Outlet
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
  valid <- !is.na(topo$WSNO)

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


#' Write project_config Table
#'
#' Populates the project_config table with project metadata so that
#' SWAT+ Editor can recognize and open the database.
#' @noRd
.write_project_config <- function(con, project, db_file) {
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
    reference_db = NA_character_,
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
