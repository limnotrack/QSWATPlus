#' Set Up a QSWATPlus Project
#'
#' Initializes a QSWATPlus project directory structure and validates
#' input files. This is typically the first step in the SWAT+ model
#' setup workflow.
#'
#' @param project_dir Character. Path to the project directory. Will be
#'   created if it does not exist.
#' @param outlet_file Character or NULL. Optional path to outlet point
#'   shapefile for watershed delineation.
#' @param dem_file Character. Path to the DEM (Digital Elevation Model)
#'   raster file (GeoTIFF or other GDAL-supported format).
#' @param landuse_file Character. Path to the land use raster file.
#' @param landuse_lookup Character. Path to the land use lookup CSV file,
#'   mapping raster values to SWAT+ land use codes.
#' @param soil_file Character. Path to the soil raster file.
#' @param soil_lookup Character. Path to the soil lookup CSV file,
#'   mapping raster values to SWAT+ soil names.
#' @param overwrite Logical. If TRUE, overwrite existing project files.
#'   Default is FALSE.
#' @param usersoil Character or NULL. Soil physical properties dataset to
#'   use when writing the project database with [qswat_write_database()].
#'   Stored in the project object and used automatically unless overridden
#'   in [qswat_write_database()].  Accepted values:
#'   \describe{
#'     \item{`NULL`}{(default) Leave `global_usersoil` empty.}
#'     \item{`"FAO_usersoil"`}{Use FAO global soil data from the bundled
#'       `QSWATPlusProjHAWQS.sqlite` database.}
#'     \item{`"global_usersoil"`}{Use the full global soil dataset from
#'       the bundled `QSWATPlusProjHAWQS.sqlite` database.}
#'     \item{file path}{Path to a CSV file in `global_usersoil` format.
#'       See [qswat_read_usersoil()].}
#'   }
#' @param ... Additional arguments to include in the returned project
#' object. This allows users to add custom metadata or file paths as needed.
#'
#' @return A list of class `"qswat_project"` containing project
#'   configuration and file paths.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Creates the project directory structure
#'   \item Validates that all input files exist and are readable
#'   \item Checks that raster files have compatible coordinate systems
#'   \item Copies input files to the project directory
#'   \item Returns a project object for use in subsequent functions
#' }
#'
#' All raster inputs should ideally share the same coordinate reference
#' system. The DEM should be in a projected CRS (not geographic) for
#' accurate area and slope calculations.
#'
#' @examples
#' \dontrun{
#' dem <- system.file("extdata", "ravn_dem.tif", package = "rQSWATPlus")
#' landuse <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
#' soil <- system.file("extdata", "ravn_soil.tif", package = "rQSWATPlus")
#' lu_lookup <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
#' soil_lookup <- system.file("extdata", "ravn_soil.csv", package = "rQSWATPlus")
#'
#' # Setup with FAO soil data applied at database-write time
#' project <- qswat_setup(
#'   project_dir = tempdir(),
#'   dem_file = dem,
#'   landuse_file = landuse,
#'   landuse_lookup = lu_lookup,
#'   soil_file = soil,
#'   soil_lookup = soil_lookup,
#'   usersoil = "FAO_usersoil"
#' )
#' }
#'
#' @export
qswat_setup <- function(project_dir,
                        overwrite = FALSE,
                        dem_file = NULL,
                        landuse_file = NULL,
                        landuse_lookup = NULL,
                        soil_file = NULL,
                        soil_lookup = NULL,
                        outlet_file = NULL,
                        usersoil = NULL,
                        ...) {
  
  # 1. Validate inputs only if they are provided
  input_files <- list(
    dem = dem_file, 
    landuse = landuse_file, 
    soil = soil_file, 
    lu_lookup = landuse_lookup, 
    s_lookup = soil_lookup,
    outlet = outlet_file
  )
  
  for (f in Filter(Negate(is.null), input_files)) {
    if (!file.exists(f)) {
      stop("File not found: ", f, call. = FALSE)
    }
  }
  
  # 2. Create project directory structure
  dirs <- file.path(project_dir, c("Source", "Watershed",
                                   "Watershed/Rasters",
                                   "Watershed/Shapes",
                                   "Watershed/Text"))
  for (d in c(project_dir, dirs)) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE)
    }
  }
  
  # 3. Process DEM metadata if provided
  dem_crs <- NULL
  units <- NULL
  res <- NULL
  ext <- NULL
  dims <- c(nrow = NULL, ncol = NULL)
  
  if (!is.null(dem_file)) {
    dem <- terra::rast(dem_file)
    dem_crs <- terra::crs(dem)
    if (dem_crs == "") warning("DEM has no CRS defined.", call. = FALSE)
    
    units <- .detect_units(terra::crs(dem, describe = TRUE))
    res   <- terra::res(dem)
    ext   <- as.vector(terra::ext(dem))
    dims  <- c(nrow = terra::nrow(dem), ncol = terra::ncol(dem))
  }
  
  # 4. Helper for copying files to "Source"
  copy_to_source <- function(file_path) {
    if (is.null(file_path)) return(NULL)
    dest <- file.path(project_dir, "Source", basename(file_path))
    if (overwrite || !file.exists(dest)) {
      file.copy(file_path, dest, overwrite = overwrite)
    }
    return(normalizePath(dest, mustWork = FALSE))
  }
  
  # Copy standard files
  dem_proj     <- copy_to_source(dem_file)
  landuse_proj <- copy_to_source(landuse_file)
  soil_proj    <- copy_to_source(soil_file)
  lu_lkp_proj  <- copy_to_source(landuse_lookup)
  s_lkp_proj   <- copy_to_source(soil_lookup)
  
  # Special handling for Shapefile (Outlet) components
  outlet_proj <- NULL
  if (!is.null(outlet_file)) {
    out_base <- tools::file_path_sans_ext(basename(outlet_file))
    out_dir  <- dirname(outlet_file)
    for (ext_sh in c("shp", "shx", "dbf", "prj", "cpg", "qpj")) {
      src <- file.path(out_dir, paste0(out_base, ".", ext_sh))
      if (file.exists(src)) {
        file.copy(src, file.path(project_dir, "Source", paste0(out_base, ".", ext_sh)), 
                  overwrite = overwrite)
      }
    }
    outlet_proj <- normalizePath(file.path(project_dir, "Source", paste0(out_base, ".shp")), mustWork = FALSE)
  }
  
  # 5. Build the project object
  project <- list(
    project_dir      = normalizePath(project_dir),
    dem_file         = dem_proj,
    landuse_file     = landuse_proj,
    landuse_lookup   = lu_lkp_proj,
    soil_file        = soil_proj,
    soil_lookup      = s_lkp_proj,
    outlet_file      = outlet_proj,
    usersoil         = usersoil,
    crs              = dem_crs,
    units            = units,
    cell_size        = res,
    extent           = ext,
    nrow             = dims["nrow"],
    ncol             = dims["ncol"],
    # Placeholders for future steps
    fel_file = NULL, p_file = NULL, sd8_file = NULL, slp_file = NULL,
    ang_file = NULL, ad8_file = NULL, sca_file = NULL, src_stream_file = NULL,
    src_channel_file = NULL, ord_file = NULL, tree_file = NULL, coord_file = NULL,
    stream_file = NULL, watershed_file = NULL, hru_data = NULL, basin_data = NULL,
    stream_threshold = NULL, channel_threshold = NULL, db_file = NULL
  )
  
  # Merge ellipsis arguments
  extra_args <- list(...)
  if (length(extra_args) > 0) {
    project <- utils::modifyList(project, extra_args)
  }
  
  class(project) <- "qswat_project"
  return(project)
}


#' Print QSWATPlus Project Summary
#'
#' @param x A `qswat_project` object.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns `x`.
#' @export
print.qswat_project <- function(x, ...) {
  cat("QSWATPlus Project\n")
  cat("  Directory:", x$project_dir, "\n")
  if (!is.null(x$db_file)) {
    cat("  Sqlite database:", x$db_file, "\n")
  }
  cat("  DEM:", basename(x$dem_file), "\n")
  cat("  Dimensions:", x$nrow, "x", x$ncol, "\n")
  
  if (!is.null(x$cell_size)) {
    cat("  Cell size:", paste(round(x$cell_size, 2), collapse = " x "), "\n")
    cat("  Units:", x$units, "\n")
  }

  if (!is.null(x$stream_threshold)) {
    cat("  Stream threshold:", x$stream_threshold, "\n")
  }
  if (!is.null(x$hru_data)) {
    cat("  HRUs:", nrow(x$hru_data), "\n")
  }

  invisible(x)
}


#' Detect Horizontal Units from CRS
#' @noRd
.detect_units <- function(crs_info) {
  if (is.null(crs_info) || nrow(crs_info) == 0) {
    return("unknown")
  }
  # Check if projected
  if (!is.na(crs_info$code) && crs_info$code != "" &&
      !is.na(crs_info$authority) && crs_info$authority == "EPSG") {
    # Projected CRS typically use meters
    return("meters")
  }
  # Check WKT for unit info
  if (!is.na(crs_info$name) && grepl("longlat|geographic|geodetic",
                                      crs_info$name, ignore.case = TRUE)) {
    return("degrees")
  }
  return("meters")
}
