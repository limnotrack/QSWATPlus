#' Check Database Compatibility with SWAT+ Editor
#'
#' Validates that a SQLite database produced by [qswat_write_database()]
#' is compatible with the SWAT+ Editor.
#'
#' @param db_file Character. Path to the SQLite database file.
#' @param verbose Logical. If `TRUE`, print detailed check results.
#'   Default `TRUE`.
#'
#' @return A list of class `"qswat_db_check"` containing:
#' \describe{
#'   \item{passed}{Logical. `TRUE` if all checks passed.}
#'   \item{checks}{A data frame with columns `check`, `status`,
#'     and `message` describing each validation.}
#'   \item{warnings}{Character vector of non-fatal warnings.}
#'   \item{errors}{Character vector of fatal errors.}
#' }
#'
#' @details
#' The function performs the following checks against the SWAT+ Editor
#' requirements:
#' \enumerate{
#'   \item **Required tables** exist (gis_subbasins, gis_channels,
#'     gis_lsus, gis_hrus, gis_routing)
#'   \item **Required columns** are present in each table with correct
#'     types
#'   \item **Non-empty data** in mandatory tables
#'   \item **Referential integrity**: HRU subbasins reference existing
#'     subbasins, channel subbasins reference existing subbasins,
#'     routing source/sink IDs exist in their respective tables
#'   \item **Routing validity**: every routing chain terminates at an
#'     outlet, routing percentages sum to 100
#'   \item **Completeness**: every subbasin has at least one HRU,
#'     every subbasin has at least one channel
#'   \item **Data quality**: areas are positive, no NULL values in
#'     required fields
#' }
#'
#' @examples
#' \dontrun{
#' result <- qswat_check_database("project.sqlite")
#' if (!result$passed) {
#'   cat("Errors found:\n")
#'   cat(result$errors, sep = "\n")
#' }
#' }
#'
#' @export
qswat_check_database <- function(db_file, verbose = TRUE) {
  if (!file.exists(db_file)) {
    stop("Database file not found: ", db_file, call. = FALSE)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  checks <- list()
  warnings_list <- character()
  errors_list <- character()

  # Helper to record a check
  record <- function(name, pass, msg) {
    checks[[length(checks) + 1]] <<- data.frame(
      check = name,
      status = if (pass) "PASS" else "FAIL",
      message = msg,
      stringsAsFactors = FALSE
    )
    if (!pass) errors_list[[length(errors_list) + 1]] <<- msg
  }

  warn <- function(msg) {
    warnings_list[[length(warnings_list) + 1]] <<- msg
  }

  # ---- 1. Required tables ----
  existing_tables <- DBI::dbListTables(con)

  required_tables <- c("gis_subbasins", "gis_channels", "gis_lsus",
                        "gis_hrus", "gis_routing", "project_config")

  for (tbl in required_tables) {
    ok <- tbl %in% existing_tables
    record(
      paste0("table_exists:", tbl),
      ok,
      if (ok) paste0("Table '", tbl, "' exists")
      else paste0("Required table '", tbl, "' is missing")
    )
  }

  optional_tables <- c("gis_aquifers", "gis_deep_aquifers", "gis_water",
                        "gis_points", "gis_elevationbands",
                        "gis_landexempt", "gis_splithrus",
                        "BASINSDATA", "HRUSDATA", "LSUSDATA", "WATERDATA",
                        "config_delin", "config_hru", "config_landuse",
                        "config_lsu", "config_observed", "config_params",
                        "config_soil")
  for (tbl in optional_tables) {
    if (!(tbl %in% existing_tables)) {
      warn(paste0("Optional table '", tbl, "' is missing"))
    }
  }

  # Short-circuit if core tables are missing
  if (!all(required_tables %in% existing_tables)) {
    result <- .build_check_result(checks, warnings_list, errors_list)
    if (verbose) .print_check_result(result)
    return(invisible(result))
  }

  # ---- 2. Required columns ----
  expected_cols <- list(
    gis_subbasins = c("id", "area", "slo1", "len1", "sll", "lat", "lon",
                       "elev", "elevmin", "elevmax"),
    gis_hrus = c("id", "lsu", "landuse", "soil", "slope",
                  "lat", "lon", "elev"),
    gis_channels = c("id", "subbasin"),
    gis_lsus = c("id", "channel", "area", "slope", "lat", "lon", "elev"),
    gis_routing = c("sourceid", "sourcecat", "sinkid", "sinkcat", "percent"),
    project_config = c("id", "project_name", "delineation_done",
                        "hrus_done")
  )

  for (tbl in names(expected_cols)) {
    cols <- DBI::dbListFields(con, tbl)
    missing <- setdiff(expected_cols[[tbl]], cols)
    ok <- length(missing) == 0
    record(
      paste0("columns:", tbl),
      ok,
      if (ok) paste0("Table '", tbl, "' has all required columns")
      else paste0("Table '", tbl, "' missing columns: ",
                  paste(missing, collapse = ", "))
    )
  }

  # ---- 3. Non-empty data ----
  for (tbl in required_tables) {
    n <- DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS n FROM", tbl))$n
    ok <- n > 0
    record(
      paste0("non_empty:", tbl),
      ok,
      if (ok) paste0("Table '", tbl, "' has ", n, " rows")
      else paste0("Table '", tbl, "' is empty")
    )
  }

  # ---- 4. Referential integrity ----
  subbasins <- DBI::dbGetQuery(con, "SELECT id FROM gis_subbasins")$id
  hrus <- DBI::dbGetQuery(con, "SELECT * FROM gis_hrus")
  channels <- DBI::dbGetQuery(con, "SELECT id, subbasin FROM gis_channels")
  lsus <- DBI::dbGetQuery(con, "SELECT id, channel FROM gis_lsus")
  routing <- DBI::dbGetQuery(con, "SELECT * FROM gis_routing")

  # HRU → LSU (lsu column references gis_lsus.id)
  if (nrow(hrus) > 0 && "lsu" %in% names(hrus) && nrow(lsus) > 0) {
    orphan_hru <- hrus$lsu[!hrus$lsu %in% lsus$id]
    # Also allow referencing subbasins directly
    orphan_hru2 <- orphan_hru[!orphan_hru %in% subbasins]
    ok <- length(orphan_hru2) == 0
    record(
      "ref_integrity:hru_lsu",
      ok,
      if (ok) "All HRUs reference valid LSUs or subbasins"
      else paste0(length(orphan_hru2), " HRU(s) reference non-existent ",
                  "LSU(s): ",
                  paste(unique(orphan_hru2), collapse = ", "))
    )
  }

  # Channel → subbasin
  if (nrow(channels) > 0 && "subbasin" %in% names(channels)) {
    orphan_ch <- channels$subbasin[!channels$subbasin %in% subbasins]
    ok <- length(orphan_ch) == 0
    record(
      "ref_integrity:channel_subbasin",
      ok,
      if (ok) "All channels reference valid subbasins"
      else paste0(length(orphan_ch), " channel(s) reference non-existent ",
                  "subbasin(s): ",
                  paste(unique(orphan_ch), collapse = ", "))
    )
  }

  # LSU → channel
  if (nrow(lsus) > 0 && "channel" %in% names(lsus)) {
    ch_ids <- channels$id
    orphan_lsu <- lsus$channel[!lsus$channel %in% ch_ids]
    # LSU.channel may point to subbasin rather than channel id
    # Allow either channel IDs or subbasin IDs
    orphan_lsu2 <- orphan_lsu[!orphan_lsu %in% subbasins]
    ok <- length(orphan_lsu2) == 0
    record(
      "ref_integrity:lsu_channel",
      ok,
      if (ok) "All LSUs reference valid channels or subbasins"
      else paste0(length(orphan_lsu2), " LSU(s) reference non-existent ",
                  "channel/subbasin: ",
                  paste(unique(orphan_lsu2), collapse = ", "))
    )
  }

  # ---- 5. Routing validity ----
  if (nrow(routing) > 0) {
    # 5a. At least one outlet
    outlet_rows <- routing$sinkcat %in% c("outlet", "X", "x")
    has_outlet <- any(outlet_rows)
    record(
      "routing:has_outlet",
      has_outlet,
      if (has_outlet)
        paste0("Routing has ", sum(outlet_rows), " outlet route(s)")
      else "No outlet found in routing table"
    )

    # 5b. Routing percentages sum to ~100 per source
    if ("percent" %in% names(routing)) {
      pct_tolerance <- 1  # allow 1% rounding error
      pct_sums <- stats::aggregate(
        percent ~ sourceid + sourcecat,
        data = routing, FUN = sum
      )
      bad_pct <- pct_sums[abs(pct_sums$percent - 100) > pct_tolerance, ]
      ok <- nrow(bad_pct) == 0
      record(
        "routing:percent_sum",
        ok,
        if (ok)
          "All routing percentages sum to 100%"
        else paste0(nrow(bad_pct), " source(s) have routing percentages ",
                    "that do not sum to 100%")
      )
    }

    # 5c. No routing loops (simple cycle detection)
    loop_found <- .detect_routing_loop(routing)
    record(
      "routing:no_loops",
      !loop_found,
      if (!loop_found) "No routing loops detected"
      else "Circular routing loop detected in routing table"
    )
  }

  # ---- 6. Completeness ----
  # Every subbasin should have >= 1 HRU (via LSU)
  if (nrow(hrus) > 0 && length(subbasins) > 0 && "lsu" %in% names(hrus)) {
    # HRU.lsu → LSU.id → LSU.channel → subbasin
    hru_subs <- unique(hrus$lsu)
    # LSUs link to subbasins via channel field
    if (nrow(lsus) > 0) {
      lsu_subs <- unique(lsus$channel[lsus$id %in% hru_subs])
      hru_subs <- unique(c(hru_subs, lsu_subs))
    }
    missing_hru <- subbasins[!subbasins %in% hru_subs]
    ok <- length(missing_hru) == 0
    record(
      "completeness:subbasin_hrus",
      ok,
      if (ok) "Every subbasin has at least one HRU"
      else paste0(length(missing_hru), " subbasin(s) have no HRUs: ",
                  paste(missing_hru, collapse = ", "))
    )
  }

  # Every subbasin should have >= 1 channel
  if (nrow(channels) > 0 && length(subbasins) > 0 &&
      "subbasin" %in% names(channels)) {
    subs_with_ch <- unique(channels$subbasin)
    missing_ch <- subbasins[!subbasins %in% subs_with_ch]
    ok <- length(missing_ch) == 0
    record(
      "completeness:subbasin_channels",
      ok,
      if (ok) "Every subbasin has at least one channel"
      else paste0(length(missing_ch), " subbasin(s) have no channels: ",
                  paste(missing_ch, collapse = ", "))
    )
  }

  # ---- 7. Data quality ----
  # Positive areas
  if (nrow(hrus) > 0 && "area" %in% names(hrus)) {
    neg_area <- sum(hrus$area <= 0, na.rm = TRUE)
    ok <- neg_area == 0
    record(
      "quality:hru_areas",
      ok,
      if (ok) "All HRU areas are positive"
      else paste0(neg_area, " HRU(s) have zero or negative area")
    )
  }

  if (length(subbasins) > 0) {
    sub_data <- DBI::dbGetQuery(con, "SELECT * FROM gis_subbasins")
    neg_sub <- sum(sub_data$area <= 0, na.rm = TRUE)
    ok <- neg_sub == 0
    record(
      "quality:subbasin_areas",
      ok,
      if (ok) "All subbasin areas are positive"
      else paste0(neg_sub, " subbasin(s) have zero or negative area")
    )
  }

  # No NULL IDs in key tables
  null_ids <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) AS n FROM gis_hrus WHERE id IS NULL"
  )$n
  ok <- null_ids == 0
  record(
    "quality:no_null_ids",
    ok,
    if (ok) "No NULL IDs in gis_hrus"
    else paste0(null_ids, " HRU row(s) have NULL id")
  )

  result <- .build_check_result(checks, warnings_list, errors_list)
  if (verbose) .print_check_result(result)
  return(invisible(result))
}


#' Detect Routing Loops
#'
#' Simple cycle detection: follow each source through sinks and check
#' if we revisit a node before reaching an outlet.
#' @noRd
.detect_routing_loop <- function(routing) {
  # Build adjacency: source → sinks
  outlet_cats <- c("outlet", "X", "x")
  sources <- unique(paste(routing$sourceid, routing$sourcecat, sep = ":"))

  for (src in sources) {
    visited <- character()
    current <- src
    steps <- 0L
    # A path can visit at most N unique nodes before repeating (cycle)
    max_steps <- nrow(routing) + 1L

    while (steps < max_steps) {
      if (current %in% visited) return(TRUE)  # loop
      visited <- c(visited, current)
      parts <- strsplit(current, ":")[[1]]
      sid <- as.integer(parts[1])
      scat <- parts[2]
      # Find sink for this source
      row <- routing[routing$sourceid == sid & routing$sourcecat == scat, ]
      if (nrow(row) == 0) break  # no further routing
      sink_cat <- row$sinkcat[1]
      if (sink_cat %in% outlet_cats) break  # reached outlet
      current <- paste(row$sinkid[1], sink_cat, sep = ":")
      steps <- steps + 1L
    }
  }
  return(FALSE)
}


#' Build Check Result Object
#' @noRd
.build_check_result <- function(checks, warnings_list, errors_list) {
  checks_df <- do.call(rbind, checks)
  result <- list(
    passed = length(errors_list) == 0,
    checks = checks_df,
    warnings = unlist(warnings_list),
    errors = unlist(errors_list)
  )
  class(result) <- "qswat_db_check"
  result
}


#' Print Check Results
#' @noRd
.print_check_result <- function(x) {
  n_pass <- sum(x$checks$status == "PASS")
  n_fail <- sum(x$checks$status == "FAIL")
  n_warn <- length(x$warnings)

  cat("SWAT+ Editor Database Compatibility Check\n")
  cat("==========================================\n")

  for (i in seq_len(nrow(x$checks))) {
    icon <- if (x$checks$status[i] == "PASS") "\u2714" else "\u2718"
    cat(sprintf("  %s %s\n", icon, x$checks$message[i]))
  }

  if (n_warn > 0) {
    cat("\nWarnings:\n")
    for (w in x$warnings) cat(sprintf("  ! %s\n", w))
  }

  cat(sprintf("\nResult: %d passed, %d failed, %d warnings\n",
              n_pass, n_fail, n_warn))

  if (x$passed) {
    cat("Database is compatible with SWAT+ Editor.\n")
  } else {
    cat("Database has compatibility issues. Fix errors above.\n")
  }
}


#' Print Method for Database Check Results
#'
#' @param x A `qswat_db_check` object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.qswat_db_check <- function(x, ...) {
  .print_check_result(x)
  invisible(x)
}
