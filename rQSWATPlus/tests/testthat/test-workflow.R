test_that("stepped workflow works", {
  dem <- system.file("extdata", "ravn_dem.tif", package = "rQSWATPlus")
  landuse <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
  soil <- system.file("extdata", "ravn_soil.tif", package = "rQSWATPlus")
  lu_lookup <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
  soil_lookup <- system.file("extdata", "ravn_soil.csv", package = "rQSWATPlus")
  outlet <- system.file("extdata", "ravn_outlet.shp", package = "rQSWATPlus")
  
  project_dir <- file.path(tempdir(), "ravn_swatplus")
  
  # Initialize the project
  project <- qswat_setup(
    project_dir = project_dir,
    dem_file = dem,
    landuse_file = landuse,
    soil_file = soil,
    landuse_lookup = lu_lookup,
    soil_lookup = soil_lookup,
    outlet_file = outlet
  )
  
  project <- qswat_delineate(
    project,
    threshold = 500,         # Stream threshold (cells)
    channel_threshold = 50,  # Channel threshold (finer network)
    quiet = TRUE            # Show TauDEM progress messages
  )
  # expect_true(!file.exists(project$db_file))
  project <- qswat_create_streams(project)
  
  # Read lookup tables
  lu <- qswat_read_landuse_lookup(lu_lookup)
  soil_lkp <- qswat_read_soil_lookup(soil_lookup)
  
  # Define slope classes: 0-5%, 5-15%, and >15%
  slopes <- qswat_create_slope_classes(c(0, 5, 15, 9999))

  # Create HRUs with area thresholds to filter small units
  project <- qswat_create_hrus(
    project,
    landuse_lookup = lu,
    soil_lookup = soil_lkp,
    slope_classes = slopes,
    landuse_threshold = 5,   # Land use must be >5% of subbasin
    soil_threshold = 5,      # Soil must be >5% of land use area
    slope_threshold = 0       # No slope filtering
  )
  
  project <- qswat_write_database(project, overwrite = TRUE)
  expect_true(file.exists(project$db_file))
  
  
})

test_that("qswat_run works", {
  dem <- system.file("extdata", "ravn_dem.tif", package = "rQSWATPlus")
  landuse <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
  soil <- system.file("extdata", "ravn_soil.tif", package = "rQSWATPlus")
  lu_lookup <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
  soil_lookup <- system.file("extdata", "ravn_soil.csv", package = "rQSWATPlus")
  outlet <- system.file("extdata", "ravn_outlet.shp", package = "rQSWATPlus")
  
  project <- qswat_run(
    project_dir = file.path(tempdir(), "ravn_quick"),
    dem_file = dem,
    landuse_file = landuse,
    soil_file = soil,
    landuse_lookup = lu_lookup,
    soil_lookup = soil_lookup,
    outlet_file = outlet,
    threshold = 500,
    slope_breaks = c(0, 5, 15, 9999),
    landuse_threshold = 5,
    soil_threshold = 5, 
    db_file = "swat.db"
  )
  
  expect_true(file.exists(project$db_file))
  
})


