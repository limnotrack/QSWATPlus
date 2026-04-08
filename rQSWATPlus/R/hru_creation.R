#' Create Hydrologic Response Units (HRUs)
#'
#' Creates HRUs by overlaying land use, soil, and slope rasters on the
#' delineated watershed. Each unique combination of subbasin, land use,
#' soil type, and slope class forms an HRU.
#'
#' @param project A `qswat_project` object that has been through
#'   [qswat_delineate()] and [qswat_create_streams()].
#' @param landuse_lookup A data frame from [qswat_read_landuse_lookup()]
#'   mapping raster values to SWAT+ land use codes.
#' @param soil_lookup A data frame from [qswat_read_soil_lookup()]
#'   mapping raster values to soil names.
#' @param slope_classes A data frame from [qswat_create_slope_classes()]
#'   defining slope percentage classes. Default creates a single class.
#' @param hru_method Character. Method for selecting HRUs. One of:
#'   \describe{
#'     \item{`"dominant_hru"`}{Single HRU per subbasin: the combination
#'       of land use, soil, and slope class with the largest area. This
#'       corresponds to the "Dominant HRU" option in the QSWATPlus GUI.}
#'     \item{`"dominant_luse"`}{Single HRU per subbasin formed from the
#'       dominant land use, the dominant soil within that land use, and
#'       the dominant slope class within that soil. This corresponds to
#'       the "Dominant landuse, soil and slope" option.}
#'     \item{`"filter_threshold"`}{Multiple HRUs retained after applying
#'       hierarchical percentage or area thresholds (default). This
#'       corresponds to the "Filter by landuse, soil, slope" option.}
#'     \item{`"filter_area"`}{Multiple HRUs retained by applying a
#'       single minimum-area threshold. This corresponds to the "Filter
#'       by area" option.}
#'     \item{`"target"`}{Multiple HRUs reduced to approximately
#'       `target_hrus` per subbasin by removing the smallest HRUs. This
#'       corresponds to the "Target number of HRUs" option.}
#'   }
#'   Default is `"filter_threshold"`.
#' @param threshold_type Character. Whether threshold values are
#'   percentages (`"percent"`, the default) or absolute areas in
#'   hectares (`"area"`). Only used when `hru_method` is
#'   `"filter_threshold"`.
#' @param landuse_threshold Numeric. When `hru_method = "filter_threshold"`
#'   and `threshold_type = "percent"`: minimum percentage of subbasin
#'   area that a land use must occupy to be kept.  When
#'   `threshold_type = "area"`: minimum area in hectares.  Default 0
#'   (keep all).
#' @param soil_threshold Numeric. When `hru_method = "filter_threshold"`
#'   and `threshold_type = "percent"`: minimum percentage of land-use
#'   area that a soil must occupy.  When `threshold_type = "area"`:
#'   minimum area in hectares.  Default 0 (keep all).
#' @param slope_threshold Numeric. When `hru_method = "filter_threshold"`
#'   and `threshold_type = "percent"`: minimum percentage of soil area
#'   that a slope class must occupy.  When `threshold_type = "area"`:
#'   minimum area in hectares.  Default 0 (keep all).
#' @param area_threshold Numeric. Minimum area in hectares used when
#'   `hru_method = "filter_area"`. Default 0 (keep all).
#' @param target_hrus Integer or NULL. Target number of HRUs per
#'   subbasin used when `hru_method = "target"`. NULL means no target
#'   filtering.
#' @param use_gwflow Logical. If `TRUE`, gwflow groundwater modelling
#'   will be used (equivalent to checking "Use gwflow" in the
#'   QSWATPlus GUI). The project database `use_gwflow` flag will be set
#'   to 1. Call [qswat_setup_gwflow()] after [qswat_write_database()]
#'   to initialise the gwflow tables. Default `FALSE`.
#' @param use_aquifers Logical. If `TRUE` (the default), SWAT+ aquifer
#'   objects are created for each subbasin (equivalent to "Use SWAT+
#'   aquifers" in the GUI). Set to `FALSE` to omit aquifer tables.
#'
#' @return An updated `qswat_project` object with HRU data and the
#'   groundwater settings stored in `project$use_gwflow` and
#'   `project$use_aquifers`.
#'
#' @details
#' The HRU creation process follows the QSWATPlus approach:
#' \enumerate{
#'   \item Read the watershed/subbasin raster from delineation
#'   \item For each cell in the watershed, extract the corresponding
#'     land use, soil, and slope values
#'   \item Classify slope into the specified slope bands
#'   \item Group cells by subbasin-landuse-soil-slope combinations
#'   \item Apply the selected HRU method to filter/reduce HRUs
#'   \item Compute statistics (area, mean elevation, mean slope) for
#'     each retained HRU
#' }
#'
#' The `"filter_threshold"` method applies thresholds hierarchically:
#' land use threshold within each subbasin, then soil threshold within
#' each remaining land use, then slope threshold within each remaining
#' soil. Areas from eliminated HRUs are redistributed proportionally to
#' the surviving HRUs.
#'
#' @examples
#' \dontrun{
#' lu_lookup <- qswat_read_landuse_lookup("landuse_lookup.csv")
#' soil_lookup <- qswat_read_soil_lookup("soil_lookup.csv")
#' slope_classes <- qswat_create_slope_classes(c(0, 5, 15, 9999))
#'
#' # Multiple HRUs with percentage thresholds (default)
#' project <- qswat_create_hrus(project, lu_lookup, soil_lookup,
#'                               slope_classes,
#'                               hru_method = "filter_threshold",
#'                               landuse_threshold = 20,
#'                               soil_threshold = 10,
#'                               slope_threshold = 10)
#'
#' # Single dominant HRU per subbasin
#' project <- qswat_create_hrus(project, lu_lookup, soil_lookup,
#'                               hru_method = "dominant_hru")
#'
#' # Target number of HRUs with gwflow enabled
#' project <- qswat_create_hrus(project, lu_lookup, soil_lookup,
#'                               hru_method = "target",
#'                               target_hrus = 5L,
#'                               use_gwflow = TRUE)
#' }
#'
#' @export
qswat_create_hrus <- function(project,
                              landuse_lookup,
                              soil_lookup,
                              slope_classes = qswat_create_slope_classes(),
                              hru_method = c("filter_threshold", "dominant_hru",
                                             "dominant_luse", "filter_area",
                                             "target"),
                              threshold_type = c("percent", "area"),
                              landuse_threshold = 0,
                              soil_threshold = 0,
                              slope_threshold = 0,
                              area_threshold = 0,
                              target_hrus = NULL,
                              use_gwflow = FALSE,
                              use_aquifers = TRUE) {
  hru_method    <- match.arg(hru_method)
  threshold_type <- match.arg(threshold_type)
  
  if (!inherits(project, "qswat_project")) {
    stop("'project' must be a qswat_project object.", call. = FALSE)
  }
  if (missing(landuse_lookup)) {
    if (file.exists(project$landuse_lookup)) {
      landuse_lookup <- qswat_read_landuse_lookup(project$landuse_lookup)
    } else {
      stop("Land use lookup not provided and not found in project.",
           call. = FALSE)
    }
  }

  if (missing(soil_lookup)) {
    if (file.exists(project$soil_lookup)) {
      soil_lookup <- qswat_read_soil_lookup(project$soil_lookup)
    } else {
      stop("Soil lookup not provided and not found in project.",
           call. = FALSE)
    }
  }

  # Validate hru_method-specific requirements
  if (hru_method == "target" &&
      (is.null(target_hrus) || !is.numeric(target_hrus) || target_hrus < 1)) {
    stop("'target_hrus' must be a positive integer when hru_method = 'target'.",
         call. = FALSE)
  }
  if (hru_method == "filter_area" &&
      (!is.numeric(area_threshold) || area_threshold < 0)) {
    stop("'area_threshold' must be a non-negative numeric value when hru_method = 'filter_area'.",
         call. = FALSE)
  }
  
  # Load rasters
  wshed_rast <- .load_watershed_raster(project)
  landuse_rast <- terra::rast(project$landuse_file)
  soil_rast <- terra::rast(project$soil_file)
  
  # Calculate slope from DEM or use sd8 from delineation
  slope_rast <- .get_slope_raster(project)
  
  # Get DEM for elevation stats
  
  dem_rast <- terra::rast(project$dem_file)
  
  message("Computing HRUs from raster overlays...")
  
  # Align all rasters to watershed grid
  landuse_aligned <- terra::project(landuse_rast, wshed_rast, method = "near")
  soil_aligned <- terra::project(soil_rast, wshed_rast, method = "near")
  slope_aligned <- terra::project(slope_rast, wshed_rast, method = "near")
  dem_aligned <- terra::project(dem_rast, wshed_rast, method = "bilinear")
  
  # Read all rasters into memory
  wshed_vals <- terra::values(wshed_rast, mat = FALSE)
  landuse_vals <- terra::values(landuse_aligned, mat = FALSE)
  soil_vals <- terra::values(soil_aligned, mat = FALSE)
  slope_vals <- terra::values(slope_aligned, mat = FALSE)
  dem_vals <- terra::values(dem_aligned, mat = FALSE)
  
  # Get valid cells (non-NA in watershed)
  valid <- !is.na(wshed_vals) & wshed_vals > 0
  if (sum(valid) == 0) {
    stop("No valid watershed cells found.", call. = FALSE)
  }
  
  # Calculate cell area
  cell_area <- .compute_cell_area(wshed_rast, project$units)
  
  # Build cell data frame
  cell_data <- data.frame(
    subbasin = wshed_vals[valid],
    landuse_val = landuse_vals[valid],
    soil_val = soil_vals[valid],
    slope_pct = slope_vals[valid],
    elevation = dem_vals[valid],
    stringsAsFactors = FALSE
  )
  
  # Map raster values to names using lookups
  cell_data$landuse <- .map_lookup(cell_data$landuse_val, landuse_lookup$value,
                                   landuse_lookup$landuse)
  cell_data$soil <- .map_lookup(cell_data$soil_val, soil_lookup$value,
                                soil_lookup$soil)
  
  # Classify slope into bands
  cell_data$slope_class <- .classify_slope(cell_data$slope_pct, slope_classes)
  
  # Remove cells with unmapped values
  unmapped_lu <- is.na(cell_data$landuse)
  unmapped_soil <- is.na(cell_data$soil)
  unmapped_slope <- is.na(cell_data$slope_class)
  
  if (sum(unmapped_lu) > 0) {
    pct <- round(100 * sum(unmapped_lu) / nrow(cell_data), 1)
    message("  ", sum(unmapped_lu), " cells (", pct,
            "%) had unmapped land use values - excluded")
  }
  if (sum(unmapped_soil) > 0) {
    pct <- round(100 * sum(unmapped_soil) / nrow(cell_data), 1)
    message("  ", sum(unmapped_soil), " cells (", pct,
            "%) had unmapped soil values - excluded")
  }
  
  valid_cells <- !unmapped_lu & !unmapped_soil & !unmapped_slope
  cell_data <- cell_data[valid_cells, ]
  
  if (nrow(cell_data) == 0) {
    stop("No valid cells after lookup mapping. Check lookup tables.",
         call. = FALSE)
  }
  
  # Aggregate cells into HRUs
  hru_data <- stats::aggregate(
    cbind(cell_count = 1, elevation = elevation,
          slope_pct = slope_pct) ~ subbasin + landuse + soil + slope_class,
    data = cell_data,
    FUN = function(x) {
      if (length(x) == 1 && is.numeric(x)) return(x)
      return(sum(x))
    }
  )
  
  # Re-aggregate properly
  hru_data <- .aggregate_hrus(cell_data, cell_area)
  
  # Apply HRU method to filter/reduce HRUs
  hru_data <- switch(hru_method,
    dominant_hru = .apply_dominant_hru(hru_data),
    dominant_luse = .apply_dominant_luse(hru_data),
    filter_threshold = {
      if (landuse_threshold > 0 || soil_threshold > 0 || slope_threshold > 0) {
        .apply_thresholds(hru_data, landuse_threshold,
                          soil_threshold, slope_threshold,
                          by_area = (threshold_type == "area"))
      } else {
        hru_data
      }
    },
    filter_area = {
      if (area_threshold > 0) {
        .apply_area_filter(hru_data, area_threshold)
      } else {
        hru_data
      }
    },
    target = .apply_target_filter(hru_data, target_hrus)
  )

  # Apply target HRU filtering (legacy path kept for backward compat when
  # hru_method is not "target" but target_hrus is still supplied)
  # NOTE: prefer hru_method = "target" for intentional target-based selection.
  if (hru_method != "target" && !is.null(target_hrus) && target_hrus > 0) {
    hru_data <- .apply_target_filter(hru_data, target_hrus)
  }
  
  # Assign HRU IDs
  hru_data$hru_id <- seq_len(nrow(hru_data))
  
  # Compute basin-level statistics
  basin_data <- .compute_basin_stats(hru_data, cell_data, cell_area)
  
  project$hru_data <- hru_data
  project$basin_data <- basin_data
  project$slope_classes <- slope_classes
  project$use_gwflow <- isTRUE(use_gwflow)
  project$use_aquifers <- isTRUE(use_aquifers)

  message("Created ", nrow(hru_data), " HRUs across ",
          length(unique(hru_data$subbasin)), " subbasins")
  
  return(project)
}


#' Load Watershed Raster
#' @noRd
.load_watershed_raster <- function(project) {
  if (!is.null(project$watershed_file) && file.exists(project$watershed_file)) {
    return(terra::rast(project$watershed_file))
  }
  stop("Watershed raster not found. Run qswat_delineate() first.",
       call. = FALSE)
}


#' Get or Compute Slope Raster
#' @noRd
.get_slope_raster <- function(project) {
  # Use D8 slope from TauDEM if available
  if (!is.null(project$sd8_file) && file.exists(project$sd8_file)) {
    slp <- terra::rast(project$sd8_file)
    # Convert from radians to percent if needed
    # TauDEM sd8 outputs slope as drop/distance (tangent), convert to percent
    return(slp * 100)
  }
  # Compute from DEM
  dem <- terra::rast(project$dem_file)
  slp <- terra::terrain(dem, v = "slope", unit = "degrees")
  # Convert degrees to percent
  return(tan(slp * pi / 180) * 100)
}


#' Compute Cell Area
#' @noRd
.compute_cell_area <- function(rast, units = "meters") {
  res <- terra::res(rast)
  area <- res[1] * res[2]  # cell area in map units^2
  
  # Convert to hectares
  if (units == "meters") {
    area_ha <- area / 10000
  } else if (units == "degrees") {
    # Approximate using center latitude
    ext <- terra::ext(rast)
    center_lat <- (ext$ymin + ext$ymax) / 2
    m_per_deg_lat <- 111320
    m_per_deg_lon <- 111320 * cos(center_lat * pi / 180)
    area_m2 <- res[1] * m_per_deg_lon * res[2] * m_per_deg_lat
    area_ha <- area_m2 / 10000
  } else {
    # Assume feet
    area_ha <- area * 0.3048^2 / 10000
  }
  
  return(area_ha)
}


#' Map Raster Values to Names via Lookup
#' @noRd
.map_lookup <- function(values, lookup_values, lookup_names) {
  idx <- match(values, lookup_values)
  return(lookup_names[idx])
}


#' Classify Slope into Bands
#' @noRd
.classify_slope <- function(slope_pct, slope_classes) {
  classes <- rep(NA_integer_, length(slope_pct))
  for (i in seq_len(nrow(slope_classes))) {
    in_class <- slope_pct >= slope_classes$min_slope[i] &
      slope_pct < slope_classes$max_slope[i]
    in_class[is.na(in_class)] <- FALSE
    classes[in_class] <- slope_classes$class_id[i]
  }
  return(classes)
}


#' Aggregate Cell Data into HRUs
#' @noRd
.aggregate_hrus <- function(cell_data, cell_area) {
  # Group by subbasin, landuse, soil, slope_class
  groups <- paste(cell_data$subbasin, cell_data$landuse,
                  cell_data$soil, cell_data$slope_class, sep = "|")
  
  unique_groups <- unique(groups)
  n_hrus <- length(unique_groups)
  
  hru_list <- lapply(unique_groups, function(g) {
    idx <- groups == g
    parts <- strsplit(g, "\\|")[[1]]
    data.frame(
      subbasin = as.integer(parts[1]),
      landuse = parts[2],
      soil = parts[3],
      slope_class = as.integer(parts[4]),
      cell_count = sum(idx),
      area_ha = sum(idx) * cell_area,
      mean_elevation = mean(cell_data$elevation[idx], na.rm = TRUE),
      mean_slope = mean(cell_data$slope_pct[idx], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, hru_list)
}


#' Apply Dominant Area Thresholds
#' @noRd
.apply_thresholds <- function(hru_data, lu_thresh, soil_thresh, slope_thresh,
                              by_area = FALSE) {
  subbasins <- unique(hru_data$subbasin)
  result_list <- list()

  for (sub in subbasins) {
    sub_data <- hru_data[hru_data$subbasin == sub, ]
    sub_area <- sum(sub_data$area_ha)

    if (sub_area == 0) next

    # Apply landuse threshold
    if (lu_thresh > 0) {
      lu_areas <- tapply(sub_data$area_ha, sub_data$landuse, sum)
      if (by_area) {
        keep_lu <- names(lu_areas[lu_areas >= lu_thresh])
      } else {
        lu_pct <- lu_areas / sub_area * 100
        keep_lu <- names(lu_pct[lu_pct >= lu_thresh])
      }
      sub_data <- sub_data[sub_data$landuse %in% keep_lu, ]
    }

    # Apply soil threshold (within each landuse)
    if (soil_thresh > 0 && nrow(sub_data) > 0) {
      keep_rows <- logical(nrow(sub_data))
      for (lu in unique(sub_data$landuse)) {
        lu_idx <- sub_data$landuse == lu
        lu_area <- sum(sub_data$area_ha[lu_idx])
        soil_areas <- tapply(sub_data$area_ha[lu_idx],
                             sub_data$soil[lu_idx], sum)
        if (by_area) {
          keep_soil <- names(soil_areas[soil_areas >= soil_thresh])
        } else {
          soil_pct <- soil_areas / lu_area * 100
          keep_soil <- names(soil_pct[soil_pct >= soil_thresh])
        }
        keep_rows[lu_idx] <- sub_data$soil[lu_idx] %in% keep_soil
      }
      sub_data <- sub_data[keep_rows, ]
    }

    # Apply slope threshold (within each landuse-soil combo)
    if (slope_thresh > 0 && nrow(sub_data) > 0) {
      keep_rows <- logical(nrow(sub_data))
      combos <- paste(sub_data$landuse, sub_data$soil, sep = "|")
      for (combo in unique(combos)) {
        c_idx <- combos == combo
        c_area <- sum(sub_data$area_ha[c_idx])
        slope_areas <- tapply(sub_data$area_ha[c_idx],
                              sub_data$slope_class[c_idx], sum)
        if (by_area) {
          keep_slope <- names(slope_areas[slope_areas >= slope_thresh])
        } else {
          slope_pct <- slope_areas / c_area * 100
          keep_slope <- names(slope_pct[slope_pct >= slope_thresh])
        }
        keep_rows[c_idx] <- as.character(sub_data$slope_class[c_idx]) %in%
          keep_slope
      }
      sub_data <- sub_data[keep_rows, ]
    }

    if (nrow(sub_data) > 0) {
      result_list[[length(result_list) + 1]] <- sub_data
    }
  }

  if (length(result_list) == 0) {
    stop("All HRUs were eliminated by thresholds. Use lower threshold values.",
         call. = FALSE)
  }

  do.call(rbind, result_list)
}


#' Apply Minimum-Area Filter (filter_area method)
#' @noRd
.apply_area_filter <- function(hru_data, area_threshold) {
  kept <- hru_data[hru_data$area_ha >= area_threshold, ]
  if (nrow(kept) == 0) {
    stop(sprintf(
      "All HRUs were eliminated by area_threshold = %g ha. Use a smaller value.",
      area_threshold), call. = FALSE)
  }
  kept
}


#' Select Single Dominant HRU Per Subbasin
#'
#' Keeps only the HRU with the largest area in each subbasin.
#' @noRd
.apply_dominant_hru <- function(hru_data) {
  subbasins <- unique(hru_data$subbasin)
  result_list <- lapply(subbasins, function(sub) {
    sub_data <- hru_data[hru_data$subbasin == sub, ]
    sub_data[which.max(sub_data$area_ha), , drop = FALSE]
  })
  do.call(rbind, result_list)
}


#' Select Single HRU from Dominant Landuse/Soil/Slope Per Subbasin
#'
#' For each subbasin selects the dominant land use, then within that
#' land use the dominant soil, and within that soil the dominant slope
#' class.  The resulting three-way combination forms a single HRU.
#' @noRd
.apply_dominant_luse <- function(hru_data) {
  subbasins <- unique(hru_data$subbasin)
  result_list <- lapply(subbasins, function(sub) {
    sub_data <- hru_data[hru_data$subbasin == sub, ]

    # Dominant landuse by area
    lu_areas <- tapply(sub_data$area_ha, sub_data$landuse, sum)
    dom_lu <- names(which.max(lu_areas))
    lu_data <- sub_data[sub_data$landuse == dom_lu, ]

    # Dominant soil within dominant landuse
    soil_areas <- tapply(lu_data$area_ha, lu_data$soil, sum)
    dom_soil <- names(which.max(soil_areas))
    soil_data <- lu_data[lu_data$soil == dom_soil, ]

    # Dominant slope class within dominant soil
    soil_data[which.max(soil_data$area_ha), , drop = FALSE]
  })
  do.call(rbind, result_list)
}


#' Apply Target Number of HRUs Filter
#' @noRd
.apply_target_filter <- function(hru_data, target_hrus) {
  subbasins <- unique(hru_data$subbasin)
  result_list <- list()
  
  for (sub in subbasins) {
    sub_data <- hru_data[hru_data$subbasin == sub, ]
    
    if (nrow(sub_data) <= target_hrus) {
      result_list[[length(result_list) + 1]] <- sub_data
      next
    }
    
    # Keep the largest HRUs
    sub_data <- sub_data[order(-sub_data$area_ha), ]
    result_list[[length(result_list) + 1]] <- sub_data[seq_len(target_hrus), ]
  }
  
  do.call(rbind, result_list)
}


#' Compute Basin-Level Statistics
#' @noRd
.compute_basin_stats <- function(hru_data, cell_data, cell_area) {
  subbasins <- unique(hru_data$subbasin)
  
  basin_list <- lapply(subbasins, function(sub) {
    sub_cells <- cell_data[cell_data$subbasin == sub, ]
    sub_hrus <- hru_data[hru_data$subbasin == sub, ]
    
    data.frame(
      subbasin = sub,
      area_ha = nrow(sub_cells) * cell_area,
      mean_elevation = mean(sub_cells$elevation, na.rm = TRUE),
      min_elevation = min(sub_cells$elevation, na.rm = TRUE),
      max_elevation = max(sub_cells$elevation, na.rm = TRUE),
      mean_slope = mean(sub_cells$slope_pct, na.rm = TRUE),
      n_hrus = nrow(sub_hrus),
      n_landuses = length(unique(sub_hrus$landuse)),
      n_soils = length(unique(sub_hrus$soil)),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, basin_list)
}
