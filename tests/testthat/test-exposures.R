one <- function(agent, dose, unit = "mg/m2", id = "A") {
  data.frame(patient_id = id, agent = agent, dose = dose, unit = unit,
             stringsAsFactors = FALSE)
}

# Worked examples from the proposal, Section 1.2 ------------------------------

test_that("300 mg/m2 daunorubicin gives 150 (COG v6.0) and 180 (Feijen 2019)", {
  d <- one("daunorubicin", 300)
  expect_equal(tl_dox_equivalent(d, "cog_v6")$dox_equivalent_mg_m2, 150)
  expect_equal(tl_dox_equivalent(d, "feijen2019")$dox_equivalent_mg_m2, 180)
})

test_that("40 mg/m2 mitoxantrone gives 400 (COG v6.0) and 420 (Feijen 2019)", {
  d <- one("mitoxantrone", 40)
  expect_equal(tl_dox_equivalent(d, "cog_v6")$dox_equivalent_mg_m2, 400)
  expect_equal(tl_dox_equivalent(d, "feijen2019")$dox_equivalent_mg_m2, 420)
})

test_that("COG mg/kg example: 2 mg/kg doxorubicin = 60 mg/m2", {
  r <- tl_dox_equivalent(one("doxorubicin", 2, "mg/kg"), "cog_v6")
  expect_equal(r$dox_equivalent_mg_m2, 60)
  expect_match(r$flags, "x30")
})

# Behaviour -------------------------------------------------------------------

test_that("a factor set must be chosen", {
  expect_error(tl_dox_equivalent(one("doxorubicin", 100)), "Choose a factor set")
  expect_error(tl_dox_equivalent(one("doxorubicin", 100), "nope"), "Unknown")
})

test_that("synonyms and unit spellings are recognised", {
  d <- rbind(one("Adriamycin", 100, "mg/m\u00b2"), one("DAUNOMYCIN", 100, "mg/m^2"))
  expect_equal(tl_dox_equivalent(d, "cog_v6")$dox_equivalent_mg_m2, 150)
})

test_that("a missing dose makes the patient total NA and is flagged", {
  d <- rbind(one("doxorubicin", 100), one("epirubicin", NA))
  r <- tl_dox_equivalent(d, "cog_v6")
  expect_true(is.na(r$dox_equivalent_mg_m2))
  expect_match(r$flags, "missing dose")
})

test_that("unrecognised agents warn and are not counted", {
  d <- rbind(one("doxorubicin", 100), one("mystery drug", 50))
  expect_warning(r <- tl_dox_equivalent(d, "cog_v6"), "mystery drug")
  expect_equal(r$dox_equivalent_mg_m2, 100)
})

test_that("unknown units are flagged, not silently converted", {
  r <- tl_dox_equivalent(one("doxorubicin", 100, "mg"), "cog_v6")
  expect_true(is.na(r$dox_equivalent_mg_m2))
  expect_match(r$flags, "unit not recognised")
})

test_that("patients with no anthracycline get zero", {
  r <- tl_dox_equivalent(one("cyclophosphamide", 1000), "cog_v6")
  expect_equal(r$dox_equivalent_mg_m2, 0)
})

test_that("custom factor tables are accepted and labelled", {
  f <- data.frame(agent = "daunorubicin", factor = 1)
  r <- tl_dox_equivalent(one("daunorubicin", 300), factors = f)
  expect_equal(r$dox_equivalent_mg_m2, 300)
  expect_equal(r$factor_set, "custom")
})

test_that("CED sums alkylating agents with Green et al. coefficients", {
  d <- rbind(one("cyclophosphamide", 1000), one("ifosfamide", 9000),
             one("BCNU", 100))
  expect_equal(tl_ced(d)$ced_mg_m2, 1000 + 9000 * 0.244 + 100 * 15)
})

test_that("CED does not convert mg/kg doses", {
  r <- tl_ced(one("busulfan", 16, "mg/kg"))
  expect_true(is.na(r$ced_mg_m2))
  expect_match(r$flags, "not converted")
})

test_that("radiotherapy sums by region and converts cGy", {
  d <- data.frame(patient_id = c("A", "A", "A"),
                  agent = c("Radiotherapy", "RT", "radiation"),
                  dose = c(10, 1500, 20), unit = c("Gy", "cGy", "Gy"),
                  region = c("Chest", "chest", "Cranial"))
  r <- tl_radiation(d)
  expect_equal(r$rt_chest_gy, 25)
  expect_equal(r$rt_cranial_gy, 20)
})

test_that("radiotherapy with no region is flagged", {
  d <- data.frame(patient_id = "A", agent = "RT", dose = 10, unit = "Gy",
                  region = NA)
  r <- tl_radiation(d)
  expect_equal(r$rt_unspecified_gy, 10)
  expect_match(r$flags, "region missing")
})

# Demonstration cohort, checked by hand ----------------------------------------

test_that("summary of the demo cohort matches manual calculation", {
  s <- tl_summarise(treatledger_demo, anthracycline_set = "cog_v6")
  expect_equal(s$dox_equivalent_mg_m2, c(200, 150, 460, 225, 0))
  expect_equal(s$ced_mg_m2, c(3000, 2196, 0, 0, 4800 + 4200 * 0.857))
  expect_equal(s$rt_chest_gy, c(0, 20, 0, 0, 25.5))
  expect_equal(s$rt_cranial_gy, c(0, 0, 0, 18, 0))
  expect_true(all(s$anthracycline_set == "cog_v6"))
  expect_true(all(s$treatledger_version == as.character(packageVersion("treatledger"))))

  f <- tl_summarise(treatledger_demo, anthracycline_set = "feijen2019")
  expect_equal(f$dox_equivalent_mg_m2, c(210, 180, 480, 225, 0))
})

test_that("tl_summarise requires an anthracycline set", {
  expect_error(tl_summarise(treatledger_demo), "anthracycline_set")
})
