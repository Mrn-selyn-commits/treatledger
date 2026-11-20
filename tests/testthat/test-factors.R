# Factors must match their sources exactly (see the verification log).

test_that("COG v6.0 anthracycline factors match Section 34", {
  f <- anthracycline_factors[anthracycline_factors$set == "cog_v6", ]
  expect_equal(f$factor[match(c("doxorubicin", "daunorubicin", "epirubicin",
                                "idarubicin", "mitoxantrone"), f$agent)],
               c(1, 0.5, 0.67, 5, 10))
})

test_that("Feijen 2019 ratios match the published estimates", {
  f <- anthracycline_factors[anthracycline_factors$set == "feijen2019", ]
  expect_equal(f$factor[match(c("doxorubicin", "daunorubicin", "epirubicin",
                                "mitoxantrone"), f$agent)],
               c(1, 0.6, 0.8, 10.5))
  # No idarubicin ratio was estimated; the COG factor is used and flagged.
  ida <- f[f$agent == "idarubicin", ]
  expect_equal(ida$factor, 5)
  expect_match(ida$note, "did not estimate")
})

test_that("CED coefficients match Green et al. (2014)", {
  expect_equal(
    ced_coefficients$factor[match(
      c("cyclophosphamide", "ifosfamide", "procarbazine", "chlorambucil",
        "carmustine", "lomustine", "melphalan", "thiotepa",
        "mechlorethamine", "busulfan"), ced_coefficients$agent)],
    c(1.0, 0.244, 0.857, 14.286, 15.0, 16.0, 40, 50, 100, 8.823)
  )
})

test_that("every factor records its source", {
  expect_false(any(anthracycline_factors$source == ""))
  expect_false(any(ced_coefficients$source == ""))
})

test_that("factor sets are listed", {
  s <- tl_factor_sets()
  expect_setequal(s$set, c("cog_v6", "feijen2019", "green2014"))
})
