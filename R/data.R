#' Anthracycline conversion factors to doxorubicin
#'
#' Factors for converting anthracycline and anthraquinone doses to a
#' doxorubicin-equivalent dose, in two named sets.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{set}{Factor set name: `"cog_v6"` or `"feijen2019"`.}
#'   \item{agent}{Generic drug name, lower case.}
#'   \item{factor}{Multiplier applied to the cumulative dose in mg/m2.}
#'   \item{source}{Publication the factor comes from.}
#'   \item{location}{Where in the source the factor appears.}
#'   \item{note}{Confidence interval, history or caveat.}
#' }
#' @source Children's Oncology Group (2023) Long-Term Follow-Up Guidelines,
#'   Version 6.0, Section 34; Feijen et al. (2019)
#'   \doi{10.1001/jamaoncol.2018.6634}. Also available as a CSV file:
#'   `system.file("extdata", "anthracycline_factors.csv", package = "treatledger")`.
"anthracycline_factors"

#' Cyclophosphamide equivalent dose coefficients
#'
#' Coefficients of the cyclophosphamide equivalent dose (CED) of Green et al.
#' (2014).
#'
#' @format A data frame with the same columns as [anthracycline_factors].
#' @source Green et al. (2014) \doi{10.1002/pbc.24679}.
"ced_coefficients"

#' Synthetic demonstration cohort
#'
#' Invented treatment records for five patients, used in examples and
#' tests. No real patient data.
#'
#' @format A data frame with columns `patient_id`, `agent`, `dose`, `unit`
#'   and `region`.
"treatledger_demo"
