#' Plot DEM Elevation Map
#'
#' Creates a spatial map of the Digital Elevation Model using `tmap`.
#'
#' @param project A `qswat_project` object.
#' @param title Character. Map title. Default `"DEM Elevation"`.
#' @param palette Character. Colour palette name passed to tmap.
#'   Default `"terrain"`.
#' @param ... Additional arguments passed to `tmap::tm_raster()`.
#'
#' @return A `tmap` object that can be printed or further customised.
#'
#' @examples
#' \dontrun{
#' project <- qswat_setup(...)
#' qswat_plot_dem(project)
#' }
#'
#' @export
qswat_plot_dem <- function(project, title = "DEM Elevation",
                           palette = "terrain", ...) {
  .check_tmap()
  dem <- terra::rast(project$dem_file)
  tmap::tm_shape(dem) +
    tmap::tm_raster(
      col.scale =  tmap::tm_scale_continuous(values = palette),
      col.legend = tmap::tm_legend(title = "Elevation (m)", 
                                   position = tmap::tm_pos_out("right",
                                                               "center"))
      ) +
    tmap::tm_title(text = title)
}


#' Plot Land Use Map
#'
#' Creates a spatial map of the land use raster with categorical
#' labels from the lookup table.
#'
#' @param project A `qswat_project` object.
#' @param landuse_lookup Data frame from [qswat_read_landuse_lookup()],
#'   or `NULL` to read from the project.
#' @param title Character. Map title.
#' @param ... Additional arguments passed to `tmap::tm_raster()`.
#'
#' @return A `tmap` object.
#'
#' @examples
#' \dontrun{
#' project <- qswat_setup(...)
#' qswat_plot_landuse(project)
#' }
#'
#' @export
qswat_plot_landuse <- function(project, landuse_lookup = NULL,
                               title = "Land Use", ...) {
  .check_tmap()
  lu_rast <- terra::rast(project$landuse_file)

  if (is.null(landuse_lookup)) {
    landuse_lookup <- qswat_read_landuse_lookup(project$landuse_lookup)
  }

  # Create a categorical raster with labels
  lu_vals <- terra::values(lu_rast, mat = FALSE)
  labels <- .map_lookup(lu_vals, landuse_lookup$value, landuse_lookup$landuse)
  label_rast <- terra::rast(lu_rast)
  terra::values(label_rast) <- as.integer(factor(labels))
  levels(label_rast) <- data.frame(
    id = seq_along(levels(factor(labels))),
    landuse = levels(factor(labels))
  )

  tmap::tm_shape(label_rast) +
    tmap::tm_raster(col.scale = tmap::tm_scale_categorical(),
                    col.legend = tmap::tm_legend(title = "Land Use", 
                                                 position = tmap::tm_pos_out("right",
                                                                             "center")))
}


#' Plot Soil Map
#'
#' Creates a spatial map of the soil raster with categorical labels
#' from the lookup table.
#'
#' @param project A `qswat_project` object.
#' @param soil_lookup Data frame from [qswat_read_soil_lookup()],
#'   or `NULL` to read from the project.
#' @param title Character. Map title.
#' @param ... Additional arguments passed to `tmap::tm_raster()`.
#'
#' @return A `tmap` object.
#'
#' @examples
#' \dontrun{
#' project <- qswat_setup(...)
#' qswat_plot_soil(project)
#' }
#'
#' @export
qswat_plot_soil <- function(project, soil_lookup = NULL,
                            title = "Soil Types", ...) {
  .check_tmap()
  soil_rast <- terra::rast(project$soil_file)

  if (is.null(soil_lookup)) {
    soil_lookup <- qswat_read_soil_lookup(project$soil_lookup)
  }

  soil_vals <- terra::values(soil_rast, mat = FALSE)
  labels <- .map_lookup(soil_vals, soil_lookup$value, soil_lookup$soil)
  label_rast <- terra::rast(soil_rast)
  terra::values(label_rast) <- as.integer(factor(labels))
  levels(label_rast) <- data.frame(
    id = seq_along(levels(factor(labels))),
    soil = levels(factor(labels))
  )

  tmap::tm_shape(label_rast) +
    tmap::tm_raster(col.scale = tmap::tm_scale_categorical(),
                    col.legend = tmap::tm_legend(title = "Soil", 
                                                 position = tmap::tm_pos_out("right",
                                                                             "center")))
}


#' Plot Stream Network
#'
#' Creates a spatial map of the stream network overlaid on the DEM
#' or watershed raster.
#'
#' @param project A `qswat_project` object that has been through
#'   [qswat_delineate()] and [qswat_create_streams()].
#' @param show_dem Logical. If `TRUE`, display the DEM as background.
#'   Default `TRUE`.
#' @param title Character. Map title.
#' @param ... Additional arguments passed to `tmap::tm_lines()`.
#'
#' @return A `tmap` object.
#'
#' @examples
#' \dontrun{
#' project <- qswat_delineate(project, threshold = 500)
#' project <- qswat_create_streams(project)
#' qswat_plot_streams(project)
#' }
#'
#' @export
qswat_plot_streams <- function(project, show_dem = TRUE,
                               title = "Stream Network", ...) {
  .check_tmap()

  if (is.null(project$streams_sf)) {
    stop("Stream data not available. Run qswat_create_streams() first.",
         call. = FALSE)
  }

  m <- if (show_dem) {
    dem <- terra::rast(project$dem_file)
    tmap::tm_shape(dem) +
      # tmap::tm_raster(palette = "Greys", title = "Elevation (m)",
      #                 alpha = 0.6)
      tmap::tm_raster(
        col.scale =  tmap::tm_scale_continuous(values = "brewer.greys"), 
        col_alpha = 0.6,
        col.legend = tmap::tm_legend(title = "Elevation (m)", 
                                     position = tmap::tm_pos_out("right",
                                                                 "center")))
  } else {
    NULL
  }

  m <- m +
    tmap::tm_shape(project$streams_sf) +
    tmap::tm_lines(col = "blue", lwd = 2) +
    tmap::tm_title(text = title)
  m
}


#' Plot Watershed / Subbasins
#'
#' Creates a spatial map of the delineated watershed showing
#' subbasin boundaries.
#'
#' @param project A `qswat_project` object that has been through
#'   [qswat_delineate()].
#' @param title Character. Map title.
#' @param ... Additional arguments passed to `tmap::tm_raster()`.
#'
#' @return A `tmap` object.
#'
#' @examples
#' \dontrun{
#' project <- qswat_delineate(project, threshold = 500)
#' qswat_plot_watershed(project)
#' }
#'
#' @export
qswat_plot_watershed <- function(project,
                                 title = "Watershed Subbasins", ...) {
  .check_tmap()

  if (is.null(project$watershed_file) ||
      !file.exists(project$watershed_file)) {
    stop("Watershed raster not available. Run qswat_delineate() first.",
         call. = FALSE)
  }

  wshed <- terra::rast(project$watershed_file)
  tmap::tm_shape(wshed) +
    tmap::tm_raster(
      col.scale =  tmap::tm_scale_categorical(values = "brewer.set3"),
      col.legend = tmap::tm_legend(title = "Subbasin", 
                                   position = tmap::tm_pos_out("right",
                                                               "center"))
    ) +
    tmap::tm_title(text  = title)
}


#' Summary Bar Chart of Land Use Distribution
#'
#' Creates a bar chart showing the area or percentage of each land use
#' type across the watershed, using `ggplot2`.
#'
#' @param project A `qswat_project` object with HRU data from
#'   [qswat_create_hrus()].
#' @param type Character. One of `"area"` (hectares) or `"percent"`.
#'   Default `"percent"`.
#' @param title Character. Plot title.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' project <- qswat_create_hrus(project, lu_lookup, soil_lookup)
#' qswat_plot_landuse_summary(project)
#' }
#'
#' @export
qswat_plot_landuse_summary <- function(project, type = "percent",
                                       title = "Land Use Distribution") {
  .check_ggplot2()
  .check_hru_data(project)

  hru <- project$hru_data
  lu_area <- stats::aggregate(area_ha ~ landuse, data = hru, FUN = sum)
  total <- sum(lu_area$area_ha)
  lu_area$pct <- lu_area$area_ha / total * 100
  lu_area <- lu_area[order(-lu_area$area_ha), ]
  lu_area$landuse <- factor(lu_area$landuse,
                            levels = lu_area$landuse)

  yvar <- if (type == "percent") "pct" else "area_ha"
  ylab <- if (type == "percent") "Watershed Area (%)" else "Area (ha)"

  ggplot2::ggplot(lu_area, ggplot2::aes(x = .data$landuse,
                                         y = .data[[yvar]])) +
    ggplot2::geom_col(fill = "forestgreen", colour = "grey30") +
    ggplot2::labs(title = title, x = "Land Use", y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                        hjust = 1))
}


#' Summary Bar Chart of Soil Distribution
#'
#' Creates a bar chart showing the area or percentage of each soil
#' type across the watershed, using `ggplot2`.
#'
#' @param project A `qswat_project` object with HRU data from
#'   [qswat_create_hrus()].
#' @param type Character. One of `"area"` (hectares) or `"percent"`.
#'   Default `"percent"`.
#' @param title Character. Plot title.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' project <- qswat_create_hrus(project, lu_lookup, soil_lookup)
#' qswat_plot_soil_summary(project)
#' }
#'
#' @export
qswat_plot_soil_summary <- function(project, type = "percent",
                                     title = "Soil Distribution") {
  .check_ggplot2()
  .check_hru_data(project)

  hru <- project$hru_data
  soil_area <- stats::aggregate(area_ha ~ soil, data = hru, FUN = sum)
  total <- sum(soil_area$area_ha)
  soil_area$pct <- soil_area$area_ha / total * 100
  soil_area <- soil_area[order(-soil_area$area_ha), ]
  soil_area$soil <- factor(soil_area$soil, levels = soil_area$soil)

  yvar <- if (type == "percent") "pct" else "area_ha"
  ylab <- if (type == "percent") "Watershed Area (%)" else "Area (ha)"

  ggplot2::ggplot(soil_area, ggplot2::aes(x = .data$soil,
                                           y = .data[[yvar]])) +
    ggplot2::geom_col(fill = "sienna", colour = "grey30") +
    ggplot2::labs(title = title, x = "Soil", y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                        hjust = 1))
}


#' Summary Plot of HRU Breakdown
#'
#' Creates a multi-panel summary of HRU composition showing the
#' distribution of land use, soil, and slope classes, and optionally
#' a per-subbasin breakdown.
#'
#' @param project A `qswat_project` object with HRU data from
#'   [qswat_create_hrus()].
#' @param by_subbasin Logical. If `TRUE`, show a stacked bar chart of
#'   HRU composition per subbasin. Default `FALSE`.
#' @param title Character. Overall plot title.
#'
#' @return A `ggplot` object. When `by_subbasin = FALSE` a faceted
#'   plot of land use, soil, and slope breakdowns is returned.
#'   When `by_subbasin = TRUE` a stacked bar chart per subbasin is
#'   returned.
#'
#' @examples
#' \dontrun{
#' project <- qswat_create_hrus(project, lu_lookup, soil_lookup)
#' qswat_plot_hru_summary(project)
#' qswat_plot_hru_summary(project, by_subbasin = TRUE)
#' }
#'
#' @export
qswat_plot_hru_summary <- function(project, by_subbasin = FALSE,
                                    title = "HRU Composition") {
  .check_ggplot2()
  .check_hru_data(project)

  hru <- project$hru_data

  if (by_subbasin) {
    # Stacked bar chart: area by landuse per subbasin
    hru$subbasin_f <- factor(hru$subbasin)
    ggplot2::ggplot(hru, ggplot2::aes(x = .data$subbasin_f,
                                       y = .data$area_ha,
                                       fill = .data$landuse)) +
      ggplot2::geom_col() +
      ggplot2::labs(title = title, x = "Subbasin", y = "Area (ha)",
                    fill = "Land Use") +
      ggplot2::theme_minimal()
  } else {
    # Faceted summary: landuse, soil, slope
    lu <- stats::aggregate(area_ha ~ landuse, data = hru, FUN = sum)
    lu$category <- "Land Use"
    names(lu) <- c("class", "area_ha", "category")

    sl <- stats::aggregate(area_ha ~ soil, data = hru, FUN = sum)
    sl$category <- "Soil"
    names(sl) <- c("class", "area_ha", "category")

    slope_classes <- project$slope_classes
    slp <- stats::aggregate(area_ha ~ slope_class, data = hru, FUN = sum)
    if (!is.null(slope_classes) && nrow(slope_classes) > 0) {
      slp$slope_class <- slope_classes$label[
        match(slp$slope_class, slope_classes$class_id)]
    }
    slp$category <- "Slope Class"
    names(slp) <- c("class", "area_ha", "category")

    combined <- rbind(lu, sl, slp)
    combined$category <- factor(combined$category,
                                levels = c("Land Use", "Soil",
                                           "Slope Class"))

    ggplot2::ggplot(combined, ggplot2::aes(x = .data$class,
                                            y = .data$area_ha)) +
      ggplot2::geom_col(fill = "steelblue", colour = "grey30") +
      ggplot2::facet_wrap(~ category, scales = "free_x") +
      ggplot2::labs(title = title, x = NULL, y = "Area (ha)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                          hjust = 1))
  }
}


#' Plot HRU Map
#'
#' Creates a spatial map of HRU polygons, coloured by a chosen attribute.
#' Requires [qswat_create_hrus()] to have been run.
#'
#' @param project A `qswat_project` object.
#' @param color_by Character. Column in `project$hru_sf` used to colour the
#'   polygons. Typical choices: `"landuse"`, `"soil"`, `"slope_class"`,
#'   `"hru_id"`. Default `"landuse"`.
#' @param title Character. Map title.
#'
#' @return A `tmap` object.
#'
#' @examples
#' \dontrun{
#' project <- qswat_create_hrus(project, lu_lookup, soil_lookup)
#' qswat_plot_hrus(project)
#' qswat_plot_hrus(project, color_by = "soil")
#' }
#'
#' @export
qswat_plot_hrus <- function(project, color_by = "landuse",
                             title = "HRUs") {
  .check_tmap()

  if (is.null(project$hru_sf)) {
    stop("HRU spatial data not available. Run qswat_create_hrus() first.",
         call. = FALSE)
  }
  if (!color_by %in% names(project$hru_sf)) {
    stop("Column '", color_by, "' not found in project$hru_sf.",
         call. = FALSE)
  }

  tmap::tm_shape(project$hru_sf) +
    tmap::tm_polygons(
      fill        = color_by,
      fill.scale  = tmap::tm_scale_categorical(),
      col         = "grey30",
      lwd         = 0.5,
      fill.legend = tmap::tm_legend(
        title    = color_by,
        position = tmap::tm_pos_out("right", "center")
      )
    ) +
    tmap::tm_title(text = title)
}


#' Plot LSU (Landscape Unit / Subbasin) Map
#'
#' Creates a spatial map of Landscape Units (LSUs).  In rQSWATPlus, LSUs and
#' subbasins are in a 1:1 relationship; each polygon represents one subbasin /
#' LSU.  Requires [qswat_create_hrus()] to have been run (or at least
#' [qswat_delineate()]).
#'
#' @param project A `qswat_project` object.
#' @param title Character. Map title.
#'
#' @return A `tmap` object.
#'
#' @examples
#' \dontrun{
#' project <- qswat_create_hrus(project, lu_lookup, soil_lookup)
#' qswat_plot_lsus(project)
#' }
#'
#' @export
qswat_plot_lsus <- function(project, title = "Landscape Units (LSUs)") {
  .check_tmap()

  lsu_sf <- project$lsu_sf
  if (is.null(lsu_sf)) {
    if (is.null(project$watershed_file) ||
        !file.exists(project$watershed_file)) {
      stop("LSU data not available. Run qswat_delineate() first.",
           call. = FALSE)
    }
    wshed  <- terra::rast(project$watershed_file)
    lsu_sf <- .build_lsu_sf(wshed)
  }

  tmap::tm_shape(lsu_sf) +
    tmap::tm_polygons(
      fill        = "subbasin",
      fill.scale  = tmap::tm_scale_categorical(values = "brewer.set3"),
      col         = "black",
      lwd         = 1.5,
      fill.legend = tmap::tm_legend(
        title    = "LSU / Subbasin",
        position = tmap::tm_pos_out("right", "center")
      )
    ) +
    tmap::tm_title(text = title)
}


#' Plot Watershed Overview
#'
#' Creates a combined overlay map that can include any subset of: DEM,
#' land use, soil, LSU/subbasin polygons, HRU polygons, stream network, and
#' outlet points.
#'
#' @param project A `qswat_project` object.
#' @param layers Character vector of layers to include. Any combination of:
#'   \describe{
#'     \item{`"dem"`}{DEM raster shown as a greyscale background.}
#'     \item{`"landuse"`}{Land-use raster (semi-transparent categorical fill).}
#'     \item{`"soil"`}{Soil raster (semi-transparent categorical fill).}
#'     \item{`"lsu"`}{LSU / subbasin polygons coloured by subbasin ID.}
#'     \item{`"hru"`}{HRU polygons coloured by land use.}
#'     \item{`"streams"`}{Stream-network lines in blue.}
#'     \item{`"outlets"`}{Outlet points in red.}
#'   }
#'   Default: `c("dem", "lsu", "hru", "streams", "outlets")`.
#' @param landuse_lookup Data frame from [qswat_read_landuse_lookup()], or
#'   `NULL` to read from the project.  Only used when `"landuse"` is in
#'   `layers`.
#' @param soil_lookup Data frame from [qswat_read_soil_lookup()], or `NULL`
#'   to read from the project.  Only used when `"soil"` is in `layers`.
#' @param title Character. Map title.
#'
#' @return A `tmap` object.
#'
#' @examples
#' \dontrun{
#' project <- qswat_run(...)
#'
#' # Default layers
#' qswat_plot_overview(project)
#'
#' # All available layers
#' qswat_plot_overview(project,
#'   layers = c("dem", "landuse", "soil", "lsu", "hru", "streams", "outlets"))
#' }
#'
#' @export
qswat_plot_overview <- function(project,
                                layers = c("dem", "lsu", "hru",
                                           "streams", "outlets"),
                                landuse_lookup = NULL,
                                soil_lookup    = NULL,
                                title          = "Watershed Overview") {
  .check_tmap()

  m <- NULL

  # 1. DEM greyscale background ----------------------------------------
  if ("dem" %in% layers &&
      !is.null(project$dem_file) && file.exists(project$dem_file)) {
    dem <- terra::rast(project$dem_file)
    m <- tmap::tm_shape(dem) +
      tmap::tm_raster(
        col.scale  = tmap::tm_scale_continuous(values = "brewer.greys"),
        col_alpha  = 0.7,
        col.legend = tmap::tm_legend(
          title    = "Elevation (m)",
          position = tmap::tm_pos_out("right", "center")
        )
      )
  }

  # 2. Land-use raster (semi-transparent) --------------------------------
  if ("landuse" %in% layers &&
      !is.null(project$landuse_file) && file.exists(project$landuse_file)) {
    if (is.null(landuse_lookup)) {
      landuse_lookup <- qswat_read_landuse_lookup(project$landuse_lookup)
    }
    lu_rast   <- terra::rast(project$landuse_file)
    lu_vals   <- terra::values(lu_rast, mat = FALSE)
    lu_labels <- .map_lookup(lu_vals, landuse_lookup$value,
                             landuse_lookup$landuse)
    lu_cat    <- terra::rast(lu_rast)
    terra::values(lu_cat) <- as.integer(factor(lu_labels))
    levels(lu_cat) <- data.frame(
      id      = seq_along(levels(factor(lu_labels))),
      landuse = levels(factor(lu_labels))
    )
    m <- m +
      tmap::tm_shape(lu_cat) +
      tmap::tm_raster(
        # col.scale / col.legend are the tmap v4 API for tm_raster (rasters
        # use col; polygons use fill).
        col.scale  = tmap::tm_scale_categorical(values = "brewer.pastel1"),
        col_alpha  = 0.5,
        col.legend = tmap::tm_legend(
          title    = "Land Use",
          position = tmap::tm_pos_out("right", "center")
        )
      )
  }

  # 3. Soil raster (semi-transparent) ------------------------------------
  if ("soil" %in% layers &&
      !is.null(project$soil_file) && file.exists(project$soil_file)) {
    if (is.null(soil_lookup)) {
      soil_lookup <- qswat_read_soil_lookup(project$soil_lookup)
    }
    soil_rast   <- terra::rast(project$soil_file)
    soil_vals   <- terra::values(soil_rast, mat = FALSE)
    soil_labels <- .map_lookup(soil_vals, soil_lookup$value, soil_lookup$soil)
    soil_cat    <- terra::rast(soil_rast)
    terra::values(soil_cat) <- as.integer(factor(soil_labels))
    levels(soil_cat) <- data.frame(
      id   = seq_along(levels(factor(soil_labels))),
      soil = levels(factor(soil_labels))
    )
    m <- m +
      tmap::tm_shape(soil_cat) +
      tmap::tm_raster(
        # Use a different palette from landuse so both can be shown together.
        col.scale  = tmap::tm_scale_categorical(values = "brewer.pastel2"),
        col_alpha  = 0.5,
        col.legend = tmap::tm_legend(
          title    = "Soil",
          position = tmap::tm_pos_out("right", "center")
        )
      )
  }

  # 4. LSU / subbasin polygons -------------------------------------------
  if ("lsu" %in% layers) {
    lsu_sf <- project$lsu_sf
    if (is.null(lsu_sf) &&
        !is.null(project$watershed_file) &&
        file.exists(project$watershed_file)) {
      wshed  <- terra::rast(project$watershed_file)
      lsu_sf <- .build_lsu_sf(wshed)
    }
    if (!is.null(lsu_sf)) {
      m <- m +
        tmap::tm_shape(lsu_sf) +
        tmap::tm_polygons(
          fill        = "subbasin",
          fill.scale  = tmap::tm_scale_categorical(values = "brewer.set3"),
          fill_alpha  = 0.3,
          col         = "black",
          lwd         = 1.5,
          fill.legend = tmap::tm_legend(
            title    = "LSU / Subbasin",
            position = tmap::tm_pos_out("right", "center")
          )
        )
    }
  }

  # 5. HRU polygons coloured by land use ---------------------------------
  if ("hru" %in% layers && !is.null(project$hru_sf)) {
    m <- m +
      tmap::tm_shape(project$hru_sf) +
      tmap::tm_polygons(
        fill        = "landuse",
        fill.scale  = tmap::tm_scale_categorical(),
        fill_alpha  = 0.6,
        col         = "grey40",
        lwd         = 0.4,
        fill.legend = tmap::tm_legend(
          title    = "HRU Land Use",
          position = tmap::tm_pos_out("right", "center")
        )
      )
  }

  # 6. Stream network ----------------------------------------------------
  if ("streams" %in% layers && !is.null(project$streams_sf)) {
    m <- m +
      tmap::tm_shape(project$streams_sf) +
      tmap::tm_lines(col = "blue", lwd = 1.5)
  }

  # 7. Outlet points -----------------------------------------------------
  if ("outlets" %in% layers &&
      !is.null(project$outlet_file) &&
      file.exists(project$outlet_file)) {
    outlets_sf <- sf::st_read(project$outlet_file, quiet = TRUE)
    m <- m +
      tmap::tm_shape(outlets_sf) +
      tmap::tm_dots(fill = "red", size = 0.3)
  }

  if (is.null(m)) {
    stop("No valid layers could be drawn. ",
         "Check that the requested layers exist in the project.",
         call. = FALSE)
  }

  m + tmap::tm_title(text = title)
}


#' Check tmap availability
#' @noRd
.check_tmap <- function() {
  if (!requireNamespace("tmap", quietly = TRUE)) {
    stop(
      "Package 'tmap' is required for spatial map plots.\n",
      "Install it with: install.packages('tmap')",
      call. = FALSE
    )
  }
}

#' Check ggplot2 availability
#' @noRd
.check_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for summary plots.\n",
      "Install it with: install.packages('ggplot2')",
      call. = FALSE
    )
  }
}

#' Check HRU data exists in project
#' @noRd
.check_hru_data <- function(project) {
  if (!inherits(project, "qswat_project")) {
    stop("'project' must be a qswat_project object.", call. = FALSE)
  }
  if (is.null(project$hru_data) || nrow(project$hru_data) == 0) {
    stop("No HRU data available. Run qswat_create_hrus() first.",
         call. = FALSE)
  }
}
