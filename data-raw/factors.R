# Builds the conversion factor tables shipped with treatledger.
# Every factor records its source and where in the source it appears.
# Run from the package root:  Rscript data-raw/factors.R

cog <- "Children's Oncology Group (2023) Long-Term Follow-Up Guidelines, Version 6.0"
cog_loc <- "Section 34, anthracycline dose conversion"
feijen <- "Feijen et al. (2019) JAMA Oncol 5(6):864-871, doi:10.1001/jamaoncol.2018.6634"
feijen_loc <- "Results, mean equivalence ratios"

anthracycline_factors <- data.frame(
  set = c(rep("cog_v6", 5), rep("feijen2019", 5)),
  agent = rep(c("doxorubicin", "daunorubicin", "epirubicin",
                "idarubicin", "mitoxantrone"), 2),
  factor = c(1, 0.5, 0.67, 5, 10,
             1, 0.6, 0.8, 5, 10.5),
  source = c(rep(cog, 5),
             feijen, feijen, feijen, cog, feijen),
  location = c(rep(cog_loc, 5),
               "Reference agent", feijen_loc, feijen_loc, cog_loc, feijen_loc),
  note = c("", "", "", "",
           "Raised from 4 to 10 in version 6.0",
           "",
           "95% CI 0.4-1.0",
           "95% CI 0.5-2.8",
           "Feijen et al. did not estimate an idarubicin ratio; COG v6.0 factor used",
           "95% CI 6.2-19.1"),
  stringsAsFactors = FALSE
)

green <- "Green et al. (2014) Pediatr Blood Cancer 61(1):53-67, doi:10.1002/pbc.24679"

ced_coefficients <- data.frame(
  set = "green2014",
  agent = c("cyclophosphamide", "ifosfamide", "procarbazine", "chlorambucil",
            "carmustine", "lomustine", "melphalan", "thiotepa",
            "mechlorethamine", "busulfan"),
  factor = c(1.0, 0.244, 0.857, 14.286, 15.0, 16.0, 40, 50, 100, 8.823),
  source = green,
  location = "CED equation",
  note = paste("Coefficient as reproduced identically in BC Children's Hospital (2023)",
               "and RCH Melbourne (2023) guidance; check against original full text",
               "before release"),
  stringsAsFactors = FALSE
)
ced_coefficients$note[ced_coefficients$agent == "carmustine"] <-
  paste("BCNU.", ced_coefficients$note[1])
ced_coefficients$note[ced_coefficients$agent == "lomustine"] <-
  paste("CCNU.", ced_coefficients$note[1])
ced_coefficients$note[ced_coefficients$agent == "mechlorethamine"] <-
  paste("Nitrogen mustard.", ced_coefficients$note[1])

dir.create("data", showWarnings = FALSE)
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
save(anthracycline_factors, file = "data/anthracycline_factors.rda", compress = "xz")
save(ced_coefficients, file = "data/ced_coefficients.rda", compress = "xz")
utils::write.csv(anthracycline_factors, "inst/extdata/anthracycline_factors.csv", row.names = FALSE)
utils::write.csv(ced_coefficients, "inst/extdata/ced_coefficients.csv", row.names = FALSE)

# Synthetic demonstration cohort: invented records, no real patients.
treatledger_demo <- data.frame(
  patient_id = c("P01", "P01", "P01", "P02", "P02", "P02", "P03", "P03",
                 "P04", "P04", "P05", "P05", "P05"),
  agent = c("Doxorubicin", "Daunorubicin", "Cyclophosphamide",
            "Daunorubicin", "Ifosfamide", "Radiotherapy",
            "Mitoxantrone", "Adriamycin",
            "Doxorubicin", "Radiotherapy",
            "Cyclophosphamide", "Procarbazine", "Radiotherapy"),
  dose = c(150, 100, 3000,
           300, 9000, 20,
           40, 2,
           225, 1800,
           4800, 4200, 25.5),
  unit = c("mg/m2", "mg/m2", "mg/m2",
           "mg/m2", "mg/m2", "Gy",
           "mg/m2", "mg/kg",
           "mg/m2", "cGy",
           "mg/m2", "mg/m2", "Gy"),
  region = c(NA, NA, NA,
             NA, NA, "chest",
             NA, NA,
             NA, "cranial",
             NA, NA, "chest"),
  stringsAsFactors = FALSE
)
save(treatledger_demo, file = "data/treatledger_demo.rda", compress = "xz")
