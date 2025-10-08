# -------------------------------------------------------------------------
# test-compute_pni.R
# -------------------------------------------------------------------------
test_that("compute_pni computes correct probabilities for general nutrients", {
  df <- tibble::tibble(
    age = c(25, 10, 30),
    Protein_g = c(0.8, 0.5, NA)
  )
  nutrients <- c("Protein" = "Protein_g")

  res <- compute_pni(
    data = df,
    age_col = "age",
    life_group = "Female",
    nutrients = nutrients,
    include_ear_values = TRUE
  )

  # Expected EARs (from reference): 25y = 0.66, 10y = 0.76
  # Compute expected manually
  expected_ear <- c(0.66, 0.76, 0.66)
  expected_pni <- 1 - pnorm(df$Protein_g, mean = expected_ear, sd = 0.1 * expected_ear)

  expect_equal(res$Female_Protein_g_ear, expected_ear)
  expect_equal(res$Female_Protein_g_pni, expected_pni)
})

# -------------------------------------------------------------------------
# Zinc case — three bioavailability levels
# -------------------------------------------------------------------------
test_that("compute_pni computes zinc PNI correctly for all bioavailability levels", {
  df <- tibble::tibble(age = c(24, 24, 24), Zinc_mg = c(3, 5, 9))
  nutrients <- c("Zinc" = "Zinc_mg")

  levels <- c("low", "moderate", "high")
  expected_ears <- list("low" = 8.2, "moderate" = 4.1, "high" = 2.5)

  for (bio in levels) {
    res <- compute_pni(
      data = df,
      age_col = "age",
      life_group = "Female",
      nutrients = nutrients,
      bioavailability = bio,
      include_ear_values = TRUE
    )

    expect_equal(unique(res$Female_Zinc_mg_ear), expected_ears[[bio]])
    calc_expected <- 1 - pnorm(df$Zinc_mg, mean = expected_ears[[bio]], sd = 0.1 * expected_ears[[bio]])
    expect_equal(res$Female_Zinc_mg_pni, calc_expected)
  }
})

# -------------------------------------------------------------------------
# Life group coverage tests
# -------------------------------------------------------------------------
test_that("compute_pni works for all supported life groups", {
  nutrients <- c("Protein" = "Protein_g")

  for (grp in c("Female", "Male", "Child", "Pregnant", "Lactating")) {
    df <- tibble::tibble(
      age = c(15, 25),
      Protein_g = c(0.8, 1.0)
    )
    res <- compute_pni(df,
      age_col = "age",
      life_group = grp,
      nutrients = nutrients,
      include_ear_values = TRUE
    )
    pni_col <- paste0(grp, "_Protein_g_pni")
    ear_col <- paste0(grp, "_Protein_g_ear")

    expect_true(all(c(pni_col, ear_col) %in% names(res)))
    expect_true(all(!is.na(res[[pni_col]])))
  }
})

# -------------------------------------------------------------------------
# Iron case — lookup-based PNI from Allen et al. (2006)
# -------------------------------------------------------------------------
test_that("compute_pni correctly applies iron lookup probabilities", {
  df <- tibble::tibble(
    age = c(30, 30, 30), # adult menstruating women (19–50)
    Iron_mg = c(14, 20, 70)
  )
  nutrients <- c("Iron" = "Iron_mg")

  res <- compute_pni(
    df,
    age_col = "age",
    life_group = "Female",
    nutrients = nutrients,
    bioavailability = "low",
    include_ear_values = TRUE
  )

  # The first (14mg) < 15.0 => probability = 1
  # The second (20mg) falls within 18.7–21.4 => probability = 0.85
  # The third (70mg) > max range => probability = 0
  expect_equal(round(res$Female_Iron_mg_pni, 2), c(1.00, 0.85, 0.00))
})

# -------------------------------------------------------------------------
# Unsupported nutrient triggers correct error
# -------------------------------------------------------------------------
test_that("compute_pni throws error for unsupported nutrient", {
  df <- tibble::tibble(age = c(20, 30), VitaminK = c(70, 80))
  nutrients <- c("Vitamin K" = "VitaminK")

  expect_error(
    compute_pni(df, "age", "Female", nutrients),
    regexp = "Unsupported nutrient"
  )
})

# -------------------------------------------------------------------------
# Iron case — child (1–3y) with months and different bioavailability
# -------------------------------------------------------------------------
test_that("compute_pni computes iron PNI correctly for children with months", {
  df <- tibble::tibble(
    age = c(24, 30, 90), # 24m (1–3y), 30m (1–3y), 90m (7.5y)
    Iron_mg = c(3, 7, 10)
  )
  nutrients <- c("Iron" = "Iron_mg")

  res_low <- compute_pni(df, "age", "Child", nutrients, bioavailability = "low")
  res_mod <- compute_pni(df, "age", "Child", nutrients, bioavailability = "moderate")

  # Spot check: ensure PNI values differ with bioavailability
  expect_true(any(res_low$Child_Iron_mg_pni != res_mod$Child_Iron_mg_pni))
  expect_true(all(res_low$Child_Iron_mg_pni >= 0 & res_low$Child_Iron_mg_pni <= 1))
})

# -------------------------------------------------------------------------
# Edge case — missing intake or age outside range
# -------------------------------------------------------------------------
test_that("compute_pni returns NA for missing intake or out-of-range age", {
  df <- tibble::tibble(age = c(200, NA), Protein_g = c(1.0, 0.8))
  nutrients <- c("Protein" = "Protein_g")

  res <- compute_pni(df, "age", "Child", nutrients)

  expect_true(all(is.na(res$Child_Protein_g_pni)))
  expect_true(all(is.na(res$Child_Protein_g_ear)))
})

# -------------------------------------------------------------------------
# Invalid inputs and error handling
# -------------------------------------------------------------------------
test_that("compute_pni throws errors and warnings for invalid inputs", {
  df <- tibble::tibble(age = c(20), Protein_g = c(0.7))
  nutrients <- c("Protein" = "Protein_g")

  # invalid data type
  expect_error(
    compute_pni(list(df), "age", "Female", nutrients),
    regexp = "must be a data frame or tibble"
  )

  # empty data frame triggers warning but returns tibble
  empty_df <- tibble::tibble()
  expect_warning(
    res <- compute_pni(empty_df, "age", "Female", nutrients),
    regexp = "has 0 rows"
  )
  expect_s3_class(res, "tbl_df")

  # invalid bioavailability
  expect_error(
    compute_pni(df, "age", "Female", nutrients, bioavailability = "wrong"),
    regexp = "must be one of"
  )

  # invalid nutrients (unnamed vector)
  expect_error(
    compute_pni(df, "age", "Female", c("Protein_g")),
    regexp = "must be a named character vector"
  )

  # invalid nutrient column name (missing in data)
  expect_error(
    compute_pni(df, "age", "Female", c("Protein" = "NotExisting")),
    regexp = "missing in `data`"
  )

  # invalid age_col type (vector)
  expect_error(
    compute_pni(df, c("age", "age2"), "Female", nutrients),
    regexp = "must be a single column name"
  )

  # missing age_col
  expect_error(
    compute_pni(df, "AgeX", "Female", nutrients),
    regexp = "not found in `data`"
  )

  # invalid life_group (not recognized)
  expect_error(
    compute_pni(df, "age", "Alien", nutrients),
    regexp = "must be one of"
  )

  # invalid multiple life_group values
  expect_error(
    compute_pni(df, "age", c("Male", "Female"), nutrients),
    regexp = "must be a single character value"
  )

  # invalid nutrient not supported by EAR (e.g., "Vitamin K")
  nutrients2 <- c("Vitamin K" = "Protein_g")
  expect_error(
    compute_pni(df, "age", "Female", nutrients2),
    regexp = "Unsupported nutrient"
  )
})

# -------------------------------------------------------------------------
# Column naming and structure
# -------------------------------------------------------------------------
test_that("compute_pni produces correctly named columns", {
  df <- tibble::tibble(age = c(25), Protein_g = c(0.8), Zinc_mg = c(4.0))
  nutrients <- c("Protein" = "Protein_g", "Zinc" = "Zinc_mg")

  res <- compute_pni(df, "age", "Female", nutrients, include_ear_values = TRUE)
  expect_true(all(c(
    "Female_Protein_g_ear", "Female_Protein_g_pni",
    "Female_Zinc_mg_ear", "Female_Zinc_mg_pni"
  ) %in% names(res)))

  # Exclude EARs if specified
  res_noear <- compute_pni(df, "age", "Female", nutrients, include_ear_values = FALSE)
  expect_false(any(grepl("_ear$", names(res_noear))))
})

# -------------------------------------------------------------------------
# EXTRA COVERAGE TESTS — Supporting internal helper functions
# -------------------------------------------------------------------------

# ---- .get_ear(): invalid life group + open-ended age range ----
test_that(".get_ear handles invalid life group and open-ended range", {
  # Invalid life group should return NA_real_
  expect_true(is.na(dietrecallkit:::.get_ear(age = 25, life_group = "UnknownGroup", nutrient = "Protein")))

  # To reach the open-ended range (age above upper bound but Inf range)
  # We'll use a life group whose last range ends with Inf (e.g., Male)
  val <- dietrecallkit:::.get_ear(age = 120, life_group = "Male", nutrient = "Protein")
  expect_true(!is.na(val))
  expect_true(is.numeric(val))
})

# ---- .get_zinc_ear(): invalid life group and out-of-range age ----
test_that(".get_zinc_ear handles missing group and unmatched age", {
  # Invalid life group
  expect_true(is.na(dietrecallkit:::.get_zinc_ear(age = 25, life_group = "Alien", bioavailability = "moderate")))

  # Valid life group but out-of-range age (e.g., Child = months only)
  expect_true(is.na(dietrecallkit:::.get_zinc_ear(age = 200, life_group = "Child", bioavailability = "moderate")))

  # Valid group and edge case for open-ended range (upper edge)
  expect_true(!is.na(dietrecallkit:::.get_zinc_ear(age = 35, life_group = "Child", bioavailability = "moderate")))
})

# ---- .get_iron_pni(): non-numeric inputs ----
test_that(".get_iron_pni validates numeric input types", {
  expect_error(dietrecallkit:::.get_iron_pni(intake = "five", age = 25, life_group = "Female", bioavailability = "low"))
  expect_error(dietrecallkit:::.get_iron_pni(intake = 5, age = "twenty", life_group = "Female", bioavailability = "low"))
})

# ---- .get_iron_pni(): NA subgroup and no match conditions ----
test_that(".get_iron_pni handles NA subgroup and unmatched intake", {
  # Age outside defined subgroup (e.g., 2-month-old infant)
  expect_true(is.na(
    suppressWarnings(
      dietrecallkit:::.get_iron_pni(intake = 5, age = 0.2, life_group = "Child", bioavailability = "low")
    )
  ))

  # Intake below minimum range → prob = 1
  res_low <- dietrecallkit:::.get_iron_pni(intake = 0.1, age = 30, life_group = "Female", bioavailability = "low")
  expect_equal(res_low, 1)

  # Intake above maximum range → prob = 0
  res_high <- dietrecallkit:::.get_iron_pni(intake = 200, age = 30, life_group = "Female", bioavailability = "low")
  expect_equal(res_high, 0)

  # Intake within no-matching but not extreme range → NA
  res_na <- suppressWarnings(
    dietrecallkit:::.get_iron_pni(intake = NA_real_, age = 30, life_group = "Female", bioavailability = "low")
  )
  expect_true(is.na(res_na))
})

# ---- .get_iron_pni(): ensure it handles vector input with mix of edge cases ----
test_that(".get_iron_pni handles vectorized edge cases correctly", {
  res <- suppressWarnings(
    dietrecallkit:::.get_iron_pni(
      intake = c(5, 70, NA),
      age = c(30, 30, 30),
      life_group = "Female",
      bioavailability = "low"
    )
  )
  expect_length(res, 3)
  expect_true(all(res %in% c(0, 1, 0.85, NA), na.rm = TRUE))
})

# ---- .get_iron_pni assigns 1 for below min and 0 for above max intake
test_that(".get_iron_pni assigns 1 for below min and 0 for above max intake", {
  # Below minimum intake (should assign 1)
  low_intake <- .get_iron_pni(
    intake = c(-1), # well below min -Inf is not matched, this ensures the branch
    age = c(36),
    life_group = "Child",
    bioavailability = "moderate"
  )
  expect_equal(low_intake, 1)

  # Above maximum intake (should assign 0)
  high_intake <- .get_iron_pni(
    intake = c(999), # well above highest range
    age = c(36),
    life_group = "Child",
    bioavailability = "moderate"
  )
  expect_equal(high_intake, 0)

  # Intake exactly equal to the highest max boundary (should yield NA)
  mid_intake <- .get_iron_pni(
    intake = c(max(.iron_pni_reference()[["Child_4_8"]][["moderate"]]$max)),
    age = c(60),
    life_group = "Child",
    bioavailability = "moderate"
  )
  expect_true(is.na(mid_intake))
})

# --- Error handling for zinc EAR: non-numeric age
test_that(".get_zinc_ear throws error for non-numeric age", {
  expect_error(
    .get_zinc_ear(age = "ten", life_group = "Child", bioavailability = "moderate"),
    regexp = "`age` must be numeric"
  )
})
