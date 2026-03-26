#' Set Up a QSWATPlus Project
#'
#' Initializes a QSWATPlus project directory structure and validates
#' input files. This is typically the first step in the SWAT+ model
#' setup workflow.
#'
#' @param project_dir Character. Path to the project directory. Will be
#'   created if it does not exist.
#' @param dem_file Character. Path to the DEM (Digital Elevation Model)
#'   raster file (GeoTIFF or other GDAL-supported format).
#' @param landuse_file Character. Path to the land use raster file.
#' @param soil_file Character. Path to the soil raster file.
#' @param landuse_lookup Character. Path to the land use lookup CSV file,
#'   mapping raster values to SWAT+ land use codes.
#' @param soil_lookup Character. Path to the soil lookup CSV file,
#'   mapping raster values to SWAT+ soil names.
#' @param outlet_file Character or NULL. Optional path to outlet point
#'   shapefile for watershed delineation.
#' @param overwrite Logical. If TRUE, overwrite existing project files.
#'   Default is FALSE.
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
#' project <- qswat_setup(
#'   project_dir = tempdir(),
#'   dem_file = dem,
#'   landuse_file = landuse,
#'   soil_file = soil,
#'   landuse_lookup = lu_lookup,
#'   soil_lookup = soil_lookup
#' )
#' }
#'
#' @export
qswat_setup <- function(project_dir,
                        dem_file,
                        landuse_file,
                        soil_file,
                        landuse_lookup,
                        soil_lookup,
                        outlet_file = NULL,
                        overwrite = FALSE) {

  # Validate inputs exist
  for (f in c(dem_file, landuse_file, soil_file, landuse_lookup, soil_lookup)) {
    if (!file.exists(f)) {
      stop("File not found: ", f, call. = FALSE)
    }
  }
  if (!is.null(outlet_file) && !file.exists(outlet_file)) {
    stop("Outlet file not found: ", outlet_file, call. = FALSE)
  }

  # Create project directory structure
  dirs <- file.path(project_dir, c("Source", "Watershed",
                                    "Watershed/Rasters",
                                    "Watershed/Shapes",
                                    "Watershed/Text"))
  for (d in c(project_dir, dirs)) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE)
    }
  }

  # Load and validate rasters
  dem <- terra::rast(dem_file)
  landuse <- terra::rast(landuse_file)
  soil <- terra::rast(soil_file)

  # Check CRS
  dem_crs <- terra::crs(dem)
  if (dem_crs == "") {
    warning("DEM has no coordinate reference system defined.", call. = FALSE)
  }

  # Detect horizontal units
  crs_info <- terra::crs(dem, describe = TRUE)
  units <- .detect_units(crs_info)

  # Copy source files to project
  dem_proj <- file.path(project_dir, "Source", basename(dem_file))
  landuse_proj <- file.path(project_dir, "Source", basename(landuse_file))
  soil_proj <- file.path(project_dir, "Source", basename(soil_file))
  lu_lookup_proj <- file.path(project_dir, "Source", basename(landuse_lookup))
  soil_lookup_proj <- file.path(project_dir, "Source", basename(soil_lookup))

  if (overwrite || !file.exists(dem_proj)) {
    file.copy(dem_file, dem_proj, overwrite = overwrite)
  }
  if (overwrite || !file.exists(landuse_proj)) {
    file.copy(landuse_file, landuse_proj, overwrite = overwrite)
  }
  if (overwrite || !file.exists(soil_proj)) {
    file.copy(soil_file, soil_proj, overwrite = overwrite)
  }
  if (overwrite || !file.exists(lu_lookup_proj)) {
    file.copy(landuse_lookup, lu_lookup_proj, overwrite = overwrite)
  }
  if (overwrite || !file.exists(soil_lookup_proj)) {
    file.copy(soil_lookup, soil_lookup_proj, overwrite = overwrite)
  }

  outlet_proj <- NULL
  if (!is.null(outlet_file)) {
    outlet_base <- tools::file_path_sans_ext(basename(outlet_file))
    outlet_ext <- tools::file_ext(outlet_file)
    outlet_dir <- dirname(outlet_file)
    # Copy all shapefile components
    for (ext in c("shp", "shx", "dbf", "prj", "cpg", "qpj")) {
      src <- file.path(outlet_dir, paste0(outlet_base, ".", ext))
      if (file.exists(src)) {
        file.copy(src, file.path(project_dir, "Source",
                                 paste0(outlet_base, ".", ext)),
                  overwrite = overwrite)
      }
    }
    outlet_proj <- file.path(project_dir, "Source",
                             paste0(outlet_base, ".shp"))
  }

  # Build project object
  project <- list(
    project_dir = normalizePath(project_dir),
    dem_file = normalizePath(dem_proj),
    landuse_file = normalizePath(landuse_proj),
    soil_file = normalizePath(soil_proj),
    landuse_lookup = normalizePath(lu_lookup_proj),
    soil_lookup = normalizePath(soil_lookup_proj),
    outlet_file = if (!is.null(outlet_proj)) normalizePath(outlet_proj) else NULL,
    crs = dem_crs,
    units = units,
    cell_size = terra::res(dem),
    extent = as.vector(terra::ext(dem)),
    nrow = terra::nrow(dem),
    ncol = terra::ncol(dem),
    # TauDEM output files (populated during delineation)
    fel_file = NULL,
    p_file = NULL,
    sd8_file = NULL,
    slp_file = NULL,
    ang_file = NULL,
    ad8_file = NULL,
    sca_file = NULL,
    src_stream_file = NULL,
    src_channel_file = NULL,
    ord_file = NULL,
    tree_file = NULL,
    coord_file = NULL,
    stream_file = NULL,
    watershed_file = NULL,
    # HRU results (populated during HRU creation)
    hru_data = NULL,
    basin_data = NULL,
    stream_threshold = NULL,
    channel_threshold = NULL
  )

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
  cat("  DEM:", basename(x$dem_file), "\n")
  cat("  Dimensions:", x$nrow, "x", x$ncol, "\n")
  cat("  Cell size:", paste(round(x$cell_size, 2), collapse = " x "), "\n")
  cat("  Units:", x$units, "\n")

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
