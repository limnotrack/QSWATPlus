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

# Test that the full database write produces all tables matching the Python
# QSWAT+ plugin output (excluding template-specific tables like Example1_*).
test_that("full database includes all Python QSWAT+ plugin tables", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

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
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  actual_tables <- DBI::dbListTables(con)

  # Tables from a Python QSWAT+ plugin database (excluding template-only
  # tables like Example1_*, FAO_*, "Name AutoCorrect Save Failures")
  python_tables <- c(
    "BASINSDATA", "HRUSDATA", "LSUSDATA", "WATERDATA",
    "WGEN_User", "WGEN_User_mon",
    "aquifer_aqu", "aquifer_con", "aquifer_con_out",
    "aquifer2d_con", "aquifer2d_con_out",
    "atmo_cli", "atmo_cli_sta", "atmo_cli_sta_value",
    "bmpuser_str", "cal_parms_cal", "calibration_cal",
    "calibration_cal_cond", "calibration_cal_elem",
    "ch_catunit_def", "ch_catunit_def_elem", "ch_catunit_ele",
    "ch_reg_def", "ch_reg_def_elem",
    "ch_sed_budget_sft", "ch_sed_budget_sft_item", "ch_sed_parms_sft",
    "chan_aqu_lin", "chan_aqu_lin_ob", "chan_surf_lin", "chan_surf_lin_ob",
    "chandeg_con", "chandeg_con_out",
    "channel_cha", "channel_con", "channel_con_out", "channel_lte_cha",
    "chem_app_ops", "cntable_lum", "codes_bsn", "codes_sft",
    "config_delin", "config_hru", "config_landuse", "config_lsu",
    "config_observed", "config_params", "config_soil",
    "cons_prac_lum", "constituents_cs",
    "d_table_dtl", "d_table_dtl_act", "d_table_dtl_act_out",
    "d_table_dtl_cond", "d_table_dtl_cond_alt",
    "delratio_con", "delratio_con_out", "delratio_del",
    "dr_hmet_col", "dr_hmet_del", "dr_hmet_val",
    "dr_om_del", "dr_path_col", "dr_path_del", "dr_path_val",
    "dr_pest_col", "dr_pest_del", "dr_pest_val",
    "dr_salt_col", "dr_salt_del", "dr_salt_val",
    "exco_con", "exco_con_out", "exco_exc",
    "exco_hmet_col", "exco_hmet_exc", "exco_hmet_val",
    "exco_om_exc", "exco_path_col", "exco_path_exc", "exco_path_val",
    "exco_pest_col", "exco_pest_exc", "exco_pest_val",
    "exco_salt_col", "exco_salt_exc", "exco_salt_val",
    "fertilizer_frt", "field_fld", "file_cio", "file_cio_classification",
    "filterstrip_str", "fire_ops",
    "gis_aquifers", "gis_channels", "gis_deep_aquifers",
    "gis_elevationbands", "gis_hrus", "gis_landexempt", "gis_lsus",
    "gis_points", "gis_routing", "gis_splithrus", "gis_subbasins", "gis_water",
    "global_landuses", "global_soils", "global_usersoil",
    "grassedww_str", "graze_ops",
    "gwflow_base", "gwflow_fpcell", "gwflow_grid", "gwflow_hrucell",
    "gwflow_init_conc", "gwflow_lsucell", "gwflow_obs_locs",
    "gwflow_out_days", "gwflow_rescell", "gwflow_rivcell",
    "gwflow_solutes", "gwflow_wetland", "gwflow_zone",
    "harv_ops", "hmet_hru_ini", "hmet_hru_ini_item",
    "hmet_water_ini", "hmet_water_ini_item",
    "hru_con", "hru_con_out", "hru_data_hru",
    "hru_lte_con", "hru_lte_con_out", "hru_lte_hru",
    "hyd_sed_lte_cha", "hydrology_cha", "hydrology_hyd",
    "hydrology_res", "hydrology_wet",
    "initial_aqu", "initial_cha", "initial_res", "irr_ops",
    "landuse_lookup", "landuse_lum",
    "ls_reg_def", "ls_reg_ele", "ls_unit_def", "ls_unit_ele",
    "management_sch", "management_sch_auto", "management_sch_op",
    "metals_mtl", "modflow_con", "modflow_con_out",
    "nutrients_cha", "nutrients_res", "nutrients_sol",
    "object_cnt", "object_prt", "om_water_ini",
    "outlet_con", "outlet_con_out", "ovn_table_lum", "parameters_bsn",
    "path_hru_ini", "path_hru_ini_item",
    "path_water_ini", "path_water_ini_item",
    "pathogens_pth", "pest_hru_ini", "pest_hru_ini_item",
    "pest_water_ini", "pest_water_ini_item", "pesticide_pst",
    "plant", "plant_gro_sft", "plant_gro_sft_item",
    "plant_ini", "plant_ini_item",
    "plant_parms_sft", "plant_parms_sft_item", "plants_plt",
    "print_prt", "print_prt_aa_int", "print_prt_object",
    "project_config",
    "rec_catunit_def", "rec_catunit_def_elem", "rec_catunit_ele",
    "rec_reg_def", "rec_reg_def_elem",
    "recall_con", "recall_con_out", "recall_dat", "recall_rec",
    "res_catunit_def", "res_catunit_def_elem", "res_catunit_ele",
    "res_reg_def", "res_reg_def_elem",
    "reservoir_con", "reservoir_con_out", "reservoir_res",
    "rout_unit_con", "rout_unit_con_out",
    "rout_unit_dr", "rout_unit_ele", "rout_unit_rtu",
    "salt_aqu_ini", "salt_atmo_cli", "salt_channel_ini",
    "salt_fertilizer_frt", "salt_hru_ini", "salt_hru_ini_cs",
    "salt_hru_ini_item", "salt_irrigation", "salt_module",
    "salt_plants", "salt_plants_flags", "salt_recall_dat",
    "salt_recall_rec", "salt_res_ini", "salt_road", "salt_urban",
    "salt_water_ini", "salt_water_ini_item", "salts_slt",
    "sediment_cha", "sediment_res", "septic_sep", "septic_str",
    "snow_sno", "soil_plant_ini", "soils_lte_sol", "soils_sol",
    "soils_sol_layer", "sweep_ops",
    "tiledrain_str", "tillage_til", "time_sim", "topography_hyd",
    "urban", "urban_urb",
    "water_allocation_dmd_ob", "water_allocation_dmd_ob_src",
    "water_allocation_src_ob", "water_allocation_wro",
    "water_balance_sft", "water_balance_sft_item", "wb_parms_sft",
    "weather_file", "weather_sta_cli", "weather_sta_cli_scale",
    "weather_wgn_cli", "weather_wgn_cli_mon",
    "weir_res", "wetland_wet", "wind_dir_cli"
  )

  missing <- setdiff(python_tables, actual_tables)
  expect_true(
    length(missing) == 0,
    info = paste0("Missing tables: ", paste(missing, collapse = ", "))
  )
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
  expect_true("hyd_type" %in% names(routing))
  expect_true(all(routing$hyd_type == "tot"))

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
