# -------------------------------------------------------------------------
# test-compute_nar.R
# -------------------------------------------------------------------------

test_that("compute_nar computes correct adequacy ratios for general nutrients", {
  df <- tibble::tibble(
    age = c(10, 25, 35),
    Magnesium_g = c(220, 300, 320)
  )
  nutrients <- c("Magnesium" = "Magnesium_g")

  res <- compute_nar(
    data = df,
    age_col = "age",
    life_group = "Female",
    nutrients = nutrients,
    include_rni_values = TRUE
  )

  # Expected RNI (mock from internal reference table)
  expected_rni <- vapply(df$age, function(a) .get_rni(a, "Female", "Magnesium"), numeric(1))
  expected_nar <- pmin(df$Magnesium_g / expected_rni, 1)

  expect_equal(res$Female_Magnesium_g_rni, expected_rni)
  expect_equal(res$Female_Magnesium_g_adequacy_ratio, expected_nar)
})

# -------------------------------------------------------------------------
# Zinc case — all bioavailability levels tested
# -------------------------------------------------------------------------
test_that("compute_nar applies zinc RNI adjustments correctly for bioavailability", {
  df <- tibble::tibble(age = c(25, 25, 25), Zinc_mg = c(4, 6, 9))
  nutrients <- c("Zinc" = "Zinc_mg")

  levels <- c("low", "moderate", "high")
  expected_rnis <- list("low" = 9.8, "moderate" = 4.9, "high" = 3.0)

  for (bio in levels) {
    res <- compute_nar(
      df,
      age_col = "age",
      life_group = "Female",
      nutrients = nutrients,
      bioavailability = bio,
      include_rni_values = TRUE
    )

    expect_equal(unique(res$Female_Zinc_mg_rni), expected_rnis[[bio]])
    expected_nar <- pmin(df$Zinc_mg / expected_rnis[[bio]], 1)
    expect_equal(res$Female_Zinc_mg_adequacy_ratio, expected_nar)
  }
})

# -------------------------------------------------------------------------
# Iron case — bioavailability and prefix naming
# -------------------------------------------------------------------------
test_that("compute_nar handles iron RNI with bioavailability and naming consistency", {
  df <- tibble::tibble(age = c(30, 30), Iron_mg = c(8, 20))
  nutrients <- c("Iron" = "Iron_mg")

  res <- compute_nar(df, "age", "Female", nutrients, bioavailability = "low")

  expect_true(all(c("Female_Iron_mg_rni", "Female_Iron_mg_adequacy_ratio") %in% names(res)))
  expect_true(all(res$Female_Iron_mg_adequacy_ratio <= 1))
  expect_true(all(res$Female_Iron_mg_adequacy_ratio >= 0))
})

# -------------------------------------------------------------------------
# MAR computation — mean of all adequacy ratios per row
# -------------------------------------------------------------------------
test_that("compute_nar computes mean adequacy ratio (MAR) correctly", {
  df <- tibble::tibble(
    age = c(20, 30),
    Protein_g = c(1.2, 0.9),
    Iron_mg = c(18, 10)
  )
  nutrients <- c("Protein" = "Protein_g", "Iron" = "Iron_mg")

  res <- compute_nar(
    data = df,
    age_col = "age",
    life_group = "Female",
    nutrients = nutrients,
    bioavailability = "low",
    include_rni_values = TRUE
  )

  nar_cols <- grep("_adequacy_ratio$", names(res), value = TRUE)
  manual_mar <- rowMeans(res[nar_cols], na.rm = TRUE)
  expect_equal(res$Female_mean_adequacy_ratio, manual_mar)
})

# -------------------------------------------------------------------------
# Life group coverage — ensure function works for all supported groups
# -------------------------------------------------------------------------
test_that("compute_nar works for all life groups", {
  nutrients <- c("Protein" = "Protein_g")
  groups <- c("Female", "Male", "Child", "Pregnant", "Lactating")

  for (grp in groups) {
    df <- tibble::tibble(age = c(10, 25), Protein_g = c(0.8, 1.0))
    res <- compute_nar(df, "age", grp, nutrients)

    rni_col <- paste0(grp, "_Protein_g_rni")
    nar_col <- paste0(grp, "_Protein_g_adequacy_ratio")
    mar_col <- paste0(grp, "_mean_adequacy_ratio")

    expect_true(all(c(rni_col, nar_col, mar_col) %in% names(res)))
    expect_true(all(res[[nar_col]] >= 0 & res[[nar_col]] <= 1, na.rm = TRUE))
  }
})

# -------------------------------------------------------------------------
# Handling missing intake or unmatched age
# -------------------------------------------------------------------------
test_that("compute_nar returns NA when intake or age is missing", {
  df <- tibble::tibble(age = c(NA, 250), Protein_g = c(1.0, 0.8))
  nutrients <- c("Protein" = "Protein_g")

  res <- compute_nar(df, "age", "Female", nutrients)
  expect_true(all(is.na(res$Female_Protein_g_adequacy_ratio)))
  expect_true(all(is.na(res$Female_mean_adequacy_ratio)))
})

# -------------------------------------------------------------------------
# Excluding RNI columns
# -------------------------------------------------------------------------
test_that("compute_nar excludes RNI columns when include_rni_values = FALSE", {
  df <- tibble::tibble(age = c(25), Protein_g = c(0.8))
  nutrients <- c("Protein" = "Protein_g")

  res <- compute_nar(df, "age", "Female", nutrients, include_rni_values = FALSE)
  expect_false(any(grepl("_rni$", names(res))))
  expect_true(any(grepl("_adequacy_ratio$", names(res))))
})

# -------------------------------------------------------------------------
# Input validation — all error and edge conditions
# -------------------------------------------------------------------------
test_that("compute_nar validates inputs properly", {
  df <- tibble::tibble(age = c(20), Protein_g = c(0.8))
  nutrients <- c("Protein" = "Protein_g")

  # Non-dataframe input
  expect_error(compute_nar(list(df), "age", "Female", nutrients), "must be a data frame")

  # Invalid life_group
  expect_error(compute_nar(df, "age", "Unknown", nutrients), "Invalid `life_group`")

  # Invalid bioavailability
  expect_error(compute_nar(df, "age", "Female", nutrients, bioavailability = "extreme"), "Invalid `bioavailability`")

  # Missing nutrients
  expect_error(compute_nar(df, "age", "Female", c()), "must be a named character vector")

  # Unnamed nutrients
  expect_error(compute_nar(df, "age", "Female", c("Protein_g")), "must be a named character vector")

  # Missing age column
  expect_error(compute_nar(df, "AgeX", "Female", nutrients), "not found in `data`")

  # Age column present but empty
  df_empty <- tibble::tibble()
  expect_error(compute_nar(df_empty, "age", "Female", nutrients), "not found in `data`")
})

# -------------------------------------------------------------------------
# Column structure and consistency
# -------------------------------------------------------------------------
test_that("compute_nar creates correctly named and structured output", {
  df <- tibble::tibble(age = c(25), Protein_g = c(0.8), Zinc_mg = c(4.0))
  nutrients <- c("Protein" = "Protein_g", "Zinc" = "Zinc_mg")

  res <- compute_nar(df, "age", "Female", nutrients, include_rni_values = TRUE)
  expect_true(all(c(
    "Female_Protein_g_rni",
    "Female_Protein_g_adequacy_ratio",
    "Female_Zinc_mg_rni",
    "Female_Zinc_mg_adequacy_ratio",
    "Female_mean_adequacy_ratio"
  ) %in% names(res)))
})

# -------------------------------------------------------------------------
# Edge handling for zinc and iron internal RNI helpers
# -------------------------------------------------------------------------
test_that(".get_zinc_rni and .get_iron_rni handle invalid inputs gracefully", {
  expect_true(is.na(dietrecallkit:::.get_zinc_rni(age = 999, life_group = "Alien", bioavailability = "low")))
  expect_true(is.na(dietrecallkit:::.get_iron_rni(age = 0.1, life_group = "Child", bioavailability = "low")))
})

test_that(".get_rni handles edge branches completely", {
  # 1. Invalid life group (triggers is.null(grp))
  res_invalid_grp <- suppressWarnings(
    dietrecallkit:::.get_rni(age = 25, life_group = "Alien", nutrient = "Folate")
  )
  expect_true(is.na(res_invalid_grp))

  # 2. NA value branch — mock minimal reference table
  fake_ref <- list(
    Female = list(
      age_ranges = list(c(19, 30)),
      values = list("Folate" = c(NA_real_))
    )
  )

  testthat::with_mocked_bindings(
    .rni_reference_table = function() fake_ref,
    {
      res_na_val <- dietrecallkit:::.get_rni(age = 25, life_group = "Female", nutrient = "Folate")
      expect_true(is.na(res_na_val))
    },
    .package = "dietrecallkit"
  )

  # 3. Open-ended range (is.infinite(r[2]))
  res_inf_range <- dietrecallkit:::.get_rni(age = 120, life_group = "Male", nutrient = "Folate")
  expect_true(!is.na(res_inf_range))
  expect_true(is.numeric(res_inf_range))
})

test_that(".get_zinc_rni triggers and handles all branches", {
  # 1. Non-numeric age triggers stop() in get_zinc_rni
  expect_error(
    dietrecallkit:::.get_zinc_rni(age = "ten", life_group = "Child", bioavailability = "moderate"),
    regexp = "`age` must be numeric"
  )

  # 2. Non-numeric age triggers stop() in get_iron_rni
  expect_error(
    dietrecallkit:::.get_iron_rni(age = "ten", life_group = "Child", bioavailability = "moderate"),
    regexp = "`age` must be numeric"
  )

  # 3. Unmatched age (too high, not in any range) triggers final return(NA_real_) in get_zinc_rni
  val <- dietrecallkit:::.get_zinc_rni(age = 999, life_group = "Child", bioavailability = "moderate")
  expect_true(is.na(val))

  # 4. Unmatched age (too high, not in any range) triggers final return(NA_real_) in get_iron_rni
  val <- dietrecallkit:::.get_iron_rni(age = 999, life_group = "Child", bioavailability = "moderate")
  expect_true(is.na(val))
})

# -------------------------------------------------------------------------
# NA propagation check — ensure missing intake propagates as NA
# -------------------------------------------------------------------------
test_that("compute_nar propagates NA when intake is missing", {
  df <- tibble::tibble(age = c(25, 30), Calcium_g = c(NA, 900))
  nutrients <- c("Calcium" = "Calcium_g")

  res <- compute_nar(df, "age", "Female", nutrients)
  expect_true(is.na(res$Female_Calcium_g_adequacy_ratio[1]))
  expect_false(is.na(res$Female_Calcium_g_adequacy_ratio[2]))
})
