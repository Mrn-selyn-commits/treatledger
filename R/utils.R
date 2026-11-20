# Internal helpers -----------------------------------------------------------

# Generic names and common synonyms, all lower case.
agent_synonyms <- c(
  "doxorubicin" = "doxorubicin", "adriamycin" = "doxorubicin",
  "daunorubicin" = "daunorubicin", "daunomycin" = "daunorubicin",
  "rubidomycin" = "daunorubicin",
  "epirubicin" = "epirubicin",
  "idarubicin" = "idarubicin",
  "mitoxantrone" = "mitoxantrone", "mitozantrone" = "mitoxantrone",
  "cyclophosphamide" = "cyclophosphamide",
  "ifosfamide" = "ifosfamide",
  "procarbazine" = "procarbazine",
  "chlorambucil" = "chlorambucil",
  "carmustine" = "carmustine", "bcnu" = "carmustine",
  "lomustine" = "lomustine", "ccnu" = "lomustine",
  "melphalan" = "melphalan",
  "thiotepa" = "thiotepa", "thio-tepa" = "thiotepa",
  "mechlorethamine" = "mechlorethamine", "nitrogen mustard" = "mechlorethamine",
  "mustine" = "mechlorethamine", "chlormethine" = "mechlorethamine",
  "busulfan" = "busulfan", "busulphan" = "busulfan",
  "radiotherapy" = "radiotherapy", "radiation" = "radiotherapy",
  "rt" = "radiotherapy", "xrt" = "radiotherapy"
)

#' Standardise drug names
#'
#' Converts agent names to lower-case generic names, recognising common
#' synonyms (for example "Adriamycin" to "doxorubicin" and "BCNU" to
#' "carmustine"). Unrecognised names are returned as `NA`.
#'
#' @param x A character vector of agent names.
#' @return A character vector of standardised generic names.
#' @export
#' @examples
#' tl_standardise_agent(c("Adriamycin", "BCNU", "Busulphan", "unknown drug"))
tl_standardise_agent <- function(x) {
  key <- tolower(trimws(as.character(x)))
  out <- unname(agent_synonyms[key])
  out[is.na(key)] <- NA_character_
  out
}

standardise_unit <- function(x) {
  u <- tolower(gsub("\\s+", "", as.character(x)))
  u <- gsub("\u00b2", "2", enc2utf8(u), fixed = TRUE)
  u <- gsub("\\^", "", u)
  u
}

check_columns <- function(data, cols) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  missing <- setdiff(unname(cols), names(data))
  if (length(missing) > 0) {
    stop("Column(s) not found in `data`: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
}

pkg_version <- function() {
  as.character(utils::packageVersion("treatledger"))
}

# Sum doses by patient; a patient with any missing converted dose gets NA.
sum_by_patient <- function(ids, values, all_ids) {
  out <- vapply(all_ids, function(i) {
    v <- values[ids == i]
    if (length(v) == 0) return(0)
    if (anyNA(v)) return(NA_real_)
    sum(v)
  }, numeric(1))
  unname(out)
}

collect_flags <- function(ids, flags, all_ids) {
  vapply(all_ids, function(i) {
    f <- unique(flags[ids == i & nzchar(flags)])
    paste(f, collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
}
