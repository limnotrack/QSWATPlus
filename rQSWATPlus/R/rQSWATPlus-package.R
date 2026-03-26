#' rQSWATPlus: R Interface to QSWATPlus for SWAT+ Model Setup
#'
#' @description
#' The rQSWATPlus package provides R functions to replicate the QSWATPlus
#' QGIS plugin workflow for setting up SWAT+ (Soil and Water Assessment Tool)
#' hydrological models.
#'
#' @details
#' The package implements a complete workflow for SWAT+ model setup:
#'
#' \enumerate{
#'   \item **Project setup** ([qswat_setup()]): Initialize project structure
#'     and validate inputs
#'   \item **Watershed delineation** ([qswat_delineate()]): DEM processing
#'     and watershed delineation using TauDEM via the traudem package
#'   \item **Stream network** ([qswat_create_streams()]): Extract stream
#'     topology from delineation results
#'   \item **HRU creation** ([qswat_create_hrus()]): Create Hydrologic
#'     Response Units from land use, soil, and slope overlays
#'   \item **Database output** ([qswat_write_database()]): Write SWAT+
#'     project database for use with SWAT+ Editor
#' }
#'
#' A convenience function [qswat_run()] executes all steps in sequence.
#'
#' The package includes example data from the Ravn watershed in Denmark
#' for testing and demonstration.
#'
#' @section TauDEM Dependency:
#' Watershed delineation requires TauDEM to be installed. Use
#' `traudem::taudem_sitrep()` to check your installation. See
#' `vignette("taudem-installation", package = "traudem")` for
#' installation instructions.
#'
#' @seealso
#' \itemize{
#'   \item `vignette("introduction", package = "rQSWATPlus")` for a
#'     getting started guide
#'   \item `vignette("data-requirements", package = "rQSWATPlus")` for
#'     input data format specifications
#'   \item `vignette("workflow", package = "rQSWATPlus")` for a complete
#'     workflow example
#' }
#'
#' @import terra
#' @importFrom sf st_read st_write st_crs st_transform st_geometry_type
#'   st_coordinates st_as_sf st_bbox
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbExecute dbGetQuery
#'   dbListTables
#' @importFrom RSQLite SQLite
#' @importFrom utils read.csv
#' @importFrom rlang .data
#' @name rQSWATPlus-package
#' @aliases rQSWATPlus
#' @keywords internal
"_PACKAGE"
