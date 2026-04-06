#' Read Land Use Lookup Table
#'
#' Reads a CSV file that maps land use raster values to SWAT+ land use
#' codes. The lookup table is used during HRU creation to assign SWAT+
#' land use categories.
#'
#' @param lookup_file Character. Path to the CSV file. The file should
#'   have at least two columns: the raster value and the SWAT+ land use
#'   code.
#' @param value_col Character or integer. Name or index of the column
#'   containing raster values. Default is 1 (first column).
#' @param name_col Character or integer. Name or index of the column
#'   containing SWAT+ land use codes. Default is 2 (second column).
#'
#' @return A data frame with columns `value` (integer) and `landuse`
#'   (character).
#'
#' @details
#' The lookup CSV should map each unique value in the land use raster to
#' a SWAT+ land use code (e.g., AGRL, FRSD, PAST, URBN, WATR). The
#' format is flexible - the function will detect columns based on
#' position or name.
#'
#' Common SWAT+ land use codes include:
#' \itemize{
#'   \item AGRL - Agricultural land (generic)
#'   \item AGRR - Agricultural row crops
#'   \item FRSD - Forest (deciduous)
#'   \item FRSE - Forest (evergreen)
#'   \item FRST - Forest (mixed)
#'   \item PAST - Pasture
#'   \item RNGE - Range (grass)
#'   \item URBN - Urban (generic)
#'   \item URHD - Urban high density
#'   \item URMD - Urban medium density
#'   \item WATR - Water
#'   \item WETF - Wetland (forested)
#'   \item WETL - Wetland
#' }
#'
#' @examples
#' lu_file <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
#' lu_lookup <- qswat_read_landuse_lookup(lu_file)
#' head(lu_lookup)
#'
#' @export
qswat_read_landuse_lookup <- function(lookup_file,
                                       value_col = 1,
                                       name_col = 2) {
  if (!file.exists(lookup_file)) {
    stop("Lookup file not found: ", lookup_file, call. = FALSE)
  }

  df <- utils::read.csv(lookup_file, stringsAsFactors = FALSE)

  if (ncol(df) < 2) {
    stop("Lookup file must have at least 2 columns.", call. = FALSE)
  }

  result <- data.frame(
    value = as.integer(df[[value_col]]),
    landuse = trimws(as.character(df[[name_col]])),
    stringsAsFactors = FALSE
  )

  # Remove rows with NA values
  result <- result[!is.na(result$value) & result$landuse != "", ]

  if (nrow(result) == 0) {
    stop("No valid entries found in lookup file.", call. = FALSE)
  }

  return(result)
}


#' Read Soil Lookup Table
#'
#' Reads a CSV file that maps soil raster values to SWAT+ soil names.
#' The lookup table is used during HRU creation to assign soil types.
#'
#' @param lookup_file Character. Path to the CSV file.
#' @param value_col Character or integer. Name or index of the column
#'   containing raster values. Default is 1 (first column).
#' @param name_col Character or integer. Name or index of the column
#'   containing soil names. Default is 2 (second column).
#'
#' @return A data frame with columns `value` (integer) and `soil`
#'   (character).
#'
#' @details
#' The lookup CSV maps each unique value in the soil raster to a SWAT+
#' soil name. Soil names should correspond to entries in the SWAT+
#' soils database (e.g., STATSGO or SSURGO soil names).
#'
#' @examples
#' soil_file <- system.file("extdata", "ravn_soil.csv", package = "rQSWATPlus")
#' soil_lookup <- qswat_read_soil_lookup(soil_file)
#' head(soil_lookup)
#'
#' @export
qswat_read_soil_lookup <- function(lookup_file,
                                    value_col = 1,
                                    name_col = 2) {
  if (!file.exists(lookup_file)) {
    stop("Lookup file not found: ", lookup_file, call. = FALSE)
  }

  df <- utils::read.csv(lookup_file, stringsAsFactors = FALSE)

  if (ncol(df) < 2) {
    stop("Lookup file must have at least 2 columns.", call. = FALSE)
  }

  result <- data.frame(
    value = as.integer(df[[value_col]]),
    soil = trimws(as.character(df[[name_col]])),
    stringsAsFactors = FALSE
  )

  result <- result[!is.na(result$value) & result$soil != "", ]

  if (nrow(result) == 0) {
    stop("No valid entries found in lookup file.", call. = FALSE)
  }

  return(result)
}


#' Read User Soil Data from CSV
#'
#' Reads a CSV file containing soil physical parameters in the SWAT+
#' `global_usersoil` table format. The resulting data frame can be passed
#' directly to [qswat_write_database()] via the `usersoil` argument to
#' populate soil physical properties in the project database.
#'
#' @param csv_file Character. Path to the CSV file containing soil parameters.
#'   Must have at minimum a column named `SNAM` (soil name). Additional
#'   columns should match the `global_usersoil` table schema:
#'   `NLAYERS`, `HYDGRP`, `SOL_ZMX`, `ANION_EXCL`, `SOL_CRK`, `TEXTURE`,
#'   and for each layer \emph{n} = 1--10: `SOL_Zn`, `SOL_BDn`, `SOL_AWCn`,
#'   `SOL_Kn`, `SOL_CBNn`, `CLAYn`, `SILTn`, `SANDn`, `ROCKn`,
#'   `SOL_ALBn`, `USLE_Kn`, `SOL_ECn`.
#'
#' @return A data frame with column names normalised to uppercase, suitable
#'   for passing to [qswat_write_database()] as the `usersoil` argument.
#'   Rows with missing `SNAM` values are removed.
#'
#' @details
#' Column names are converted to uppercase on reading so the data frame is
#' compatible with the SWAT+ project database schema regardless of the case
#' used in the CSV file.
#'
#' Instead of passing the data frame directly to \code{qswat_write_database},
#' you can also pass the file path as a string; [qswat_write_database()] will
#' call this function internally.
#'
#' @examples
#' csv_file <- tempfile(fileext = ".csv")
#' write.csv(data.frame(
#'   SNAM       = c("MySoil1", "MySoil2"),
#'   NLAYERS    = c(2L, 3L),
#'   HYDGRP     = c("B", "C"),
#'   SOL_ZMX    = c(1000, 1500),
#'   ANION_EXCL = c(0.5, 0.5),
#'   SOL_CRK    = c(0.5, 0.5),
#'   SOL_Z1     = c(300, 200),
#'   SOL_BD1    = c(1.4, 1.5)
#' ), csv_file, row.names = FALSE)
#'
#' soil_data <- qswat_read_usersoil(csv_file)
#' head(soil_data)
#'
#' @seealso [qswat_write_database()]
#' @export
qswat_read_usersoil <- function(csv_file) {
  if (!file.exists(csv_file)) {
    stop("CSV file not found: ", csv_file, call. = FALSE)
  }

  df <- utils::read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)

  if (nrow(df) == 0L) {
    stop("No data found in usersoil CSV file.", call. = FALSE)
  }

  names(df) <- toupper(names(df))

  if (!"SNAM" %in% names(df)) {
    stop(
      "usersoil CSV must have an 'SNAM' column with soil names.",
      call. = FALSE
    )
  }

  df <- df[!is.na(df$SNAM) & nzchar(df$SNAM), , drop = FALSE]

  if (nrow(df) == 0L) {
    stop(
      "No valid soil entries found (all SNAM values are missing or empty).",
      call. = FALSE
    )
  }

  return(df)
}



#' Create Slope Classification Bands
#'
#' Defines slope percentage classes for HRU creation. Slope bands
#' are used to further subdivide HRUs by terrain steepness.
#'
#' @param breaks Numeric vector. Slope percentage breakpoints defining
#'   the class boundaries. For example, `c(0, 5, 15, 9999)` creates
#'   three classes: 0-5%, 5-15%, and >15%.
#' @param labels Character vector or NULL. Labels for each slope class.
#'   If NULL, labels are auto-generated from the breaks.
#'
#' @return A data frame with columns `min_slope`, `max_slope`, `label`,
#'   and `class_id`.
#'
#' @examples
#' # Three slope classes
#' slopes <- qswat_create_slope_classes(c(0, 5, 15, 9999))
#' slopes
#'
#' # Single slope class (no subdivision)
#' slopes <- qswat_create_slope_classes(c(0, 9999))
#' slopes
#'
#' @export
qswat_create_slope_classes <- function(breaks = c(0, 9999), labels = NULL) {
  if (length(breaks) < 2) {
    stop("At least two breakpoints are required.", call. = FALSE)
  }
  if (any(diff(breaks) <= 0)) {
    stop("Breaks must be strictly increasing.", call. = FALSE)
  }

  n_classes <- length(breaks) - 1

  if (is.null(labels)) {
    labels <- vapply(seq_len(n_classes), function(i) {
      if (breaks[i + 1] >= 9999) {
        paste0(breaks[i], "+")
      } else {
        paste0(breaks[i], "-", breaks[i + 1])
      }
    }, character(1))
  }

  if (length(labels) != n_classes) {
    stop("Number of labels must equal number of classes (", n_classes, ").",
         call. = FALSE)
  }

  data.frame(
    min_slope = breaks[-length(breaks)],
    max_slope = breaks[-1],
    label = labels,
    class_id = seq_len(n_classes),
    stringsAsFactors = FALSE
  )
}
