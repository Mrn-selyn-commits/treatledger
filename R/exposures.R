#' List the available conversion factor sets
#'
#' @return A data frame with one row per factor set, its drug class and the
#'   source it comes from.
#' @export
#' @examples
#' tl_factor_sets()
tl_factor_sets <- function() {
  a <- unique(treatledger::anthracycline_factors[, c("set", "source")])
  a <- a[!duplicated(a$set), ]
  a$source[a$set == "feijen2019"] <-
    "Feijen et al. (2019); idarubicin from COG v6.0"
  c_ <- unique(treatledger::ced_coefficients[, c("set", "source")])
  out <- rbind(
    data.frame(set = a$set, class = "anthracycline", source = a$source),
    data.frame(set = c_$set, class = "alkylating agent", source = c_$source)
  )
  rownames(out) <- NULL
  out
}

# Shared engine for drug-class exposures.
drug_exposure <- function(data, factors, set_label, id, agent, dose, unit,
                          mgkg_to_m2, value_name, warn_unmatched) {
  check_columns(data, c(id, agent, dose, unit))
  if (!all(c("agent", "factor") %in% names(factors))) {
    stop("`factors` must have columns `agent` and `factor`.", call. = FALSE)
  }

  ids <- as.character(data[[id]])
  all_ids <- unique(ids)
  std <- tl_standardise_agent(data[[agent]])

  unmatched <- unique(as.character(data[[agent]])[is.na(std) & !is.na(data[[agent]])])
  if (warn_unmatched && length(unmatched) > 0) {
    warning("Agent name(s) not recognised and not counted: ",
            paste(unmatched, collapse = ", "), call. = FALSE)
  }

  in_class <- !is.na(std) & std %in% factors$agent
  d <- suppressWarnings(as.numeric(data[[dose]]))
  u <- standardise_unit(data[[unit]])
  flag <- rep("", nrow(data))
  converted <- rep(NA_real_, nrow(data))

  ok_m2 <- in_class & u %in% c("mg/m2", "mgm2")
  ok_kg <- in_class & u %in% c("mg/kg", "mgkg")
  bad_unit <- in_class & !(ok_m2 | ok_kg)

  mult <- factors$factor[match(std, factors$agent)]
  converted[ok_m2] <- d[ok_m2] * mult[ok_m2]

  if (mgkg_to_m2) {
    converted[ok_kg] <- d[ok_kg] * 30 * mult[ok_kg]
    flag[ok_kg] <- "mg/kg dose converted to mg/m2 (x30, COG v6.0)"
  } else {
    flag[ok_kg] <- "mg/kg dose not converted: no validated conversion for this class"
  }
  flag[bad_unit] <- "dose unit not recognised"
  missing_dose <- in_class & is.na(d)
  flag[missing_dose] <- "missing dose"

  keep <- in_class
  out <- data.frame(
    patient_id = all_ids,
    value = sum_by_patient(ids[keep], converted[keep], all_ids),
    factor_set = set_label,
    treatledger_version = pkg_version(),
    flags = collect_flags(ids[keep], flag[keep], all_ids),
    stringsAsFactors = FALSE
  )
  names(out)[names(out) == "value"] <- value_name
  names(out)[names(out) == "patient_id"] <- id
  attr(out, "unmatched_agents") <- unmatched
  out
}

resolve_factors <- function(set, factors, table, sets) {
  if (!is.null(factors)) {
    return(list(factors = factors, label = "custom"))
  }
  if (missing(set) || is.null(set)) {
    stop("Choose a factor set with `set` (one of: ",
         paste(sets, collapse = ", "),
         "), or supply your own with `factors`.", call. = FALSE)
  }
  if (!set %in% sets) {
    stop("Unknown factor set '", set, "'. Available: ",
         paste(sets, collapse = ", "), call. = FALSE)
  }
  list(factors = table[table$set == set, ], label = set)
}

#' Cumulative doxorubicin-equivalent anthracycline dose
#'
#' Converts each anthracycline and anthraquinone course to its
#' doxorubicin-equivalent dose and sums the result for each patient.
#'
#' There is no default factor set: the choice is a scientific decision and
#' should be reported. Use `"cog_v6"` for the Children's Oncology Group
#' Long-Term Follow-Up Guidelines version 6.0 factors, or `"feijen2019"` for
#' the estimates of Feijen et al. (2019). Feijen et al. did not estimate an
#' idarubicin ratio, so the COG factor of 5 is used for idarubicin in both sets.
#'
#' Doses in mg/kg are converted to mg/m2 by multiplying by 30, as instructed
#' in the COG guidelines for anthracyclines, and the conversion is flagged.
#' A patient with any missing anthracycline dose gets `NA`, so that a partial
#' total is never mistaken for a complete one.
#'
#' @param data A data frame with one row per treatment course.
#' @param set Name of the factor set, `"cog_v6"` or `"feijen2019"`.
#' @param id,agent,dose,unit Names of the columns holding the patient
#'   identifier, agent name, cumulative dose and dose unit.
#' @param factors Optional custom factor table with columns `agent` (generic
#'   name, lower case) and `factor`. Overrides `set`.
#' @param warn_unmatched Warn about agent names that are not recognised.
#' @return A data frame with one row per patient: the identifier,
#'   `dox_equivalent_mg_m2`, the factor set, the package version and any flags.
#' @references
#' Children's Oncology Group (2023). Long-Term Follow-Up Guidelines for
#' Survivors of Childhood, Adolescent, and Young Adult Cancers, Version 6.0.
#'
#' Feijen EAM et al. (2019). Derivation of anthracycline and anthraquinone
#' equivalence ratios to doxorubicin for late-onset cardiotoxicity. JAMA
#' Oncology 5(6):864-871. \doi{10.1001/jamaoncol.2018.6634}
#' @export
#' @examples
#' tl_dox_equivalent(treatledger_demo, set = "cog_v6")
#' tl_dox_equivalent(treatledger_demo, set = "feijen2019")
tl_dox_equivalent <- function(data, set, id = "patient_id", agent = "agent",
                              dose = "dose", unit = "unit", factors = NULL,
                              warn_unmatched = TRUE) {
  tab <- treatledger::anthracycline_factors
  r <- resolve_factors(if (missing(set)) NULL else set, factors, tab,
                       unique(tab$set))
  drug_exposure(data, r$factors, r$label, id, agent, dose, unit,
                mgkg_to_m2 = TRUE, value_name = "dox_equivalent_mg_m2",
                warn_unmatched = warn_unmatched)
}

#' Cumulative cyclophosphamide equivalent dose (CED)
#'
#' Converts each alkylating agent course to its cyclophosphamide equivalent
#' dose using the coefficients of Green et al. (2014) and sums the result for
#' each patient. Doses must be in mg/m2; doses in mg/kg are flagged and not
#' converted, because no validated conversion exists for this class.
#'
#' @inheritParams tl_dox_equivalent
#' @param set Name of the factor set. Currently `"green2014"`.
#' @return A data frame with one row per patient: the identifier,
#'   `ced_mg_m2`, the factor set, the package version and any flags.
#' @references
#' Green DM et al. (2014). The cyclophosphamide equivalent dose as an approach
#' for quantifying alkylating agent exposure: a report from the Childhood
#' Cancer Survivor Study. Pediatric Blood & Cancer 61(1):53-67.
#' \doi{10.1002/pbc.24679}
#' @export
#' @examples
#' tl_ced(treatledger_demo)
tl_ced <- function(data, set = "green2014", id = "patient_id", agent = "agent",
                   dose = "dose", unit = "unit", factors = NULL,
                   warn_unmatched = TRUE) {
  tab <- treatledger::ced_coefficients
  r <- resolve_factors(set, factors, tab, unique(tab$set))
  drug_exposure(data, r$factors, r$label, id, agent, dose, unit,
                mgkg_to_m2 = FALSE, value_name = "ced_mg_m2",
                warn_unmatched = warn_unmatched)
}

#' Cumulative radiotherapy dose by body region
#'
#' Sums radiotherapy dose for each patient and body region. Rows count as
#' radiotherapy when the agent is "radiotherapy" or a synonym ("radiation",
#' "RT", "XRT"). Doses may be in Gy or cGy; cGy are divided by 100.
#'
#' @inheritParams tl_dox_equivalent
#' @param region Name of the column holding the treated body region, for
#'   example "chest" or "cranial". Region names are lower-cased and trimmed.
#' @return A data frame with one row per patient and one column per region,
#'   named `rt_<region>_gy`, plus the package version and any flags. Rows
#'   with no region are summed under `rt_unspecified_gy` and flagged.
#' @export
#' @examples
#' tl_radiation(treatledger_demo)
tl_radiation <- function(data, id = "patient_id", agent = "agent",
                         dose = "dose", unit = "unit", region = "region") {
  check_columns(data, c(id, agent, dose, unit, region))
  ids <- as.character(data[[id]])
  all_ids <- unique(ids)
  is_rt <- tl_standardise_agent(data[[agent]]) %in% "radiotherapy"

  d <- suppressWarnings(as.numeric(data[[dose]]))
  u <- standardise_unit(data[[unit]])
  reg <- tolower(trimws(as.character(data[[region]])))
  reg <- gsub("[^a-z0-9]+", "_", reg)
  flag <- rep("", nrow(data))

  gy <- rep(NA_real_, nrow(data))
  gy[u == "gy"] <- d[u == "gy"]
  gy[u == "cgy"] <- d[u == "cgy"] / 100
  flag[is_rt & !(u %in% c("gy", "cgy"))] <- "radiotherapy unit not recognised"
  flag[is_rt & is.na(d)] <- "missing radiotherapy dose"
  no_reg <- is_rt & (is.na(reg) | reg == "" | reg == "na")
  reg[no_reg] <- "unspecified"
  flag[no_reg] <- "radiotherapy region missing"

  out <- data.frame(patient_id = all_ids, stringsAsFactors = FALSE)
  regions <- sort(unique(reg[is_rt]))
  for (r in regions) {
    sel <- is_rt & reg == r
    out[[paste0("rt_", r, "_gy")]] <- sum_by_patient(ids[sel], gy[sel], all_ids)
  }
  out$treatledger_version <- pkg_version()
  out$flags <- collect_flags(ids[is_rt], flag[is_rt], all_ids)
  names(out)[names(out) == "patient_id"] <- id
  out
}

#' Summarise all exposures for each patient
#'
#' Runs [tl_dox_equivalent()], [tl_ced()] and [tl_radiation()] and joins the
#' results into one row per patient.
#'
#' @inheritParams tl_dox_equivalent
#' @inheritParams tl_radiation
#' @param anthracycline_set Factor set for anthracyclines, `"cog_v6"` or
#'   `"feijen2019"`. Required.
#' @param ced_set Factor set for alkylating agents. Default `"green2014"`.
#' @return A data frame with one row per patient holding every exposure, the
#'   factor sets used, the package version and combined flags.
#' @export
#' @examples
#' tl_summarise(treatledger_demo, anthracycline_set = "cog_v6")
tl_summarise <- function(data, anthracycline_set, ced_set = "green2014",
                         id = "patient_id", agent = "agent", dose = "dose",
                         unit = "unit", region = "region") {
  if (missing(anthracycline_set)) {
    stop("Choose `anthracycline_set` (\"cog_v6\" or \"feijen2019\").",
         call. = FALSE)
  }
  a <- tl_dox_equivalent(data, set = anthracycline_set, id = id, agent = agent,
                         dose = dose, unit = unit)
  c_ <- tl_ced(data, set = ced_set, id = id, agent = agent, dose = dose,
               unit = unit, warn_unmatched = FALSE)
  r <- if (region %in% names(data)) {
    tl_radiation(data, id = id, agent = agent, dose = dose, unit = unit,
                 region = region)
  } else {
    NULL
  }

  out <- data.frame(a[[id]], a$dox_equivalent_mg_m2, c_$ced_mg_m2,
                    stringsAsFactors = FALSE)
  names(out) <- c(id, "dox_equivalent_mg_m2", "ced_mg_m2")
  flags <- list(a$flags, c_$flags)
  if (!is.null(r)) {
    rt_cols <- grep("^rt_.*_gy$", names(r), value = TRUE)
    out <- cbind(out, r[match(out[[id]], r[[id]]), rt_cols, drop = FALSE])
    flags[[3]] <- r$flags[match(out[[id]], r[[id]])]
  }
  out$anthracycline_set <- a$factor_set[1]
  out$ced_set <- c_$factor_set[1]
  out$treatledger_version <- pkg_version()
  out$flags <- apply(do.call(cbind, flags), 1, function(x) {
    paste(x[nzchar(x)], collapse = "; ")
  })
  rownames(out) <- NULL
  out
}
