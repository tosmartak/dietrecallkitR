# -------------------------------------------------------------------------
# test-compute_ear_adequacy.R
# -------------------------------------------------------------------------

# Helper
expect_warning_free <- function(expr) testthat::expect_silent(expr)

# -------------------------------------------------------------------------
# 1. Correct adequacy computation (above, below, NA)
# -------------------------------------------------------------------------
test_that("compute_ear_adequacy computes correct adequacy flags", {
  df <- tibble::tibble(
    age = c(25, 10, 30),
    Protein_g = c(0.7, 0.6, NA)
  )
  nutrients <- c("Protein" = "Protein_g")

  res <- compute_ear_adequacy(df,
    age_col = "age",
    life_group = "Female",
    nutrients = nutrients,
    include_ear_values = TRUE
  )

  # 25y female EAR = 0.66; 10y = 0.76
  expect_equal(res$Female_Protein_g_adequacy, c(1, 0, NA_real_))
  expect_equal(res$Female_Protein_g_ear, c(0.66, 0.76, 0.66))
})

# -------------------------------------------------------------------------
# 2. Cross-check known EARs
# -------------------------------------------------------------------------
test_that("compute_ear_adequacy matches expected EAR reference lookup", {
  df <- tibble::tibble(
    age = c(25, 10),
    Protein_g = c(0.7, 0.6)
  )
  nutrients <- c("Protein" = "Protein_g")

  res <- compute_ear_adequacy(df, "age", "Female", nutrients, include_ear_values = TRUE)

  expect_equal(res$Female_Protein_g_ear, c(0.66, 0.76))
  expect_equal(res$Female_Protein_g_adequacy, c(1, 0))
})

# -------------------------------------------------------------------------
# 3. Life group coverage tests
# -------------------------------------------------------------------------
test_that("compute_ear_adequacy works for all supported life groups", {
  nutrients <- c("Protein" = "Protein_g")

  for (grp in c("Female", "Male", "Child", "Pregnant", "Lactating")) {
    df <- tibble::tibble(
      age = c(15, 25),
      Protein_g = c(0.8, 1.0)
    )
    res <- compute_ear_adequacy(df,
      age_col = "age",
      life_group = grp,
      nutrients = nutrients,
      include_ear_values = TRUE
    )
    adequacy_col <- paste0(grp, "_Protein_g_adequacy")
    ear_col <- paste0(grp, "_Protein_g_ear")

    expect_true(all(c(adequacy_col, ear_col) %in% names(res)))
    expect_true(all(!is.na(res[[adequacy_col]])))
  }
})

# -------------------------------------------------------------------------
# 4. Unsupported nutrient triggers correct error
# -------------------------------------------------------------------------
test_that("compute_ear_adequacy throws error for unsupported nutrient", {
  df <- tibble::tibble(age = c(20, 30), VitaminK = c(70, 80))
  nutrients <- c("Vitamin K" = "VitaminK")

  expect_error(
    compute_ear_adequacy(df, "age", "Female", nutrients),
    regexp = "Unsupported nutrient"
  )
})

# -------------------------------------------------------------------------
# 5. Age outside known range gives NA EAR and NA adequacy
# -------------------------------------------------------------------------
test_that("compute_ear_adequacy returns valid EAR for elderly age (71+)", {
  df <- tibble::tibble(age = c(120), Protein_g = c(1.0))
  nutrients <- c("Protein" = "Protein_g")

  res <- compute_ear_adequacy(
    df,
    "age",
    "Female",
    nutrients,
    include_ear_values = TRUE
  )

  # should be within the 71+ EAR category
  expect_equal(res$Female_Protein_g_ear, 0.66)
  expect_equal(res$Female_Protein_g_adequacy, 1)
})


# -------------------------------------------------------------------------
# 6. Age outside known range gives NA EAR and NA adequacy
# -------------------------------------------------------------------------
test_that("compute_ear_adequacy returns NA for out-of-range ages", {
  df <- tibble::tibble(age = c(120), Protein_g = c(1.0))
  nutrients <- c("Protein" = "Protein_g")

  res <- compute_ear_adequacy(
    df,
    "age",
    "Child",
    nutrients,
    include_ear_values = TRUE
  )

  expect_true(is.na(res$Child_Protein_g_ear))
  expect_true(is.na(res$Child_Protein_g_adequacy))
})

# -------------------------------------------------------------------------
# 7. Invalid input checks
# -------------------------------------------------------------------------
test_that("compute_ear_adequacy validates inputs correctly", {
  df <- tibble::tibble(age = c(20, 25), Protein_g = c(0.7, 0.8))
  nutrients <- c("Protein" = "Protein_g")

  # not a data.frame
  expect_error(compute_ear_adequacy(list(df), "age", "Female", nutrients),
    regexp = "`data` must be a data.frame"
  )

  # invalid age_col type
  expect_error(compute_ear_adequacy(df, c("age", "age2"), "Female", nutrients),
    regexp = "must be a single column name"
  )

  # missing age_col
  expect_error(compute_ear_adequacy(df, "AgeX", "Female", nutrients),
    regexp = "not found in `data`"
  )

  # invalid life_group
  expect_error(compute_ear_adequacy(df, "age", "Alien", nutrients),
    regexp = "must be one of"
  )

  # unnamed nutrients
  expect_error(compute_ear_adequacy(df, "age", "Female", "Protein_g"),
    regexp = "must be a named character vector"
  )

  # missing nutrient column
  nutrients2 <- c("Protein" = "MissingCol")
  expect_error(compute_ear_adequacy(df, "age", "Female", nutrients2),
    regexp = "missing in `data`"
  )
})

# -------------------------------------------------------------------------
# 8. Column naming validation
# -------------------------------------------------------------------------
test_that("compute_ear_adequacy creates correctly named columns", {
  df <- tibble::tibble(age = c(20, 25), Protein_g = c(0.7, 0.8))
  nutrients <- c("Protein" = "Protein_g")

  res <- compute_ear_adequacy(df,
    "age",
    "Female",
    nutrients,
    include_ear_values = TRUE
  )

  expect_true(all(c("Female_Protein_g_ear", "Female_Protein_g_adequacy") %in% names(res)))
})

# -------------------------------------------------------------------------
# 9. include_ear_values = FALSE omits EAR columns
# -------------------------------------------------------------------------
test_that("compute_ear_adequacy omits EAR column when include_ear_values = FALSE", {
  df <- tibble::tibble(age = c(25), Protein_g = c(0.8))
  nutrients <- c("Protein" = "Protein_g")

  res <- compute_ear_adequacy(df, "age", "Female", nutrients, include_ear_values = FALSE)

  expect_false(any(grepl("_ear$", names(res))))
  expect_true(any(grepl("_adequacy$", names(res))))
})

# -------------------------------------------------------------------------
# 10. Handles NA intake values correctly
# -------------------------------------------------------------------------
test_that("compute_ear_adequacy handles NA nutrient intake properly", {
  df <- tibble::tibble(age = c(25, 35), Protein_g = c(NA, 0.7))
  nutrients <- c("Protein" = "Protein_g")

  res <- compute_ear_adequacy(df, "age", "Female", nutrients, include_ear_values = TRUE)

  expect_true(is.na(res$Female_Protein_g_adequacy[1]))
  expect_equal(res$Female_Protein_g_adequacy[2], 1)
})

# -------------------------------------------------------------------------
# 10. Handles age flooring with accuracy
# -------------------------------------------------------------------------
test_that("decimal ages are floored for correct EAR lookup", {
  df <- tibble::tibble(
    age = c(13.4, 13.99, 14.0),
    Protein_g = c(0.7, 0.7, 0.7)
  )
  res <- compute_ear_adequacy(
    df,
    age_col = "age",
    life_group = "Female",
    nutrients = c("Protein" = "Protein_g"),
    include_ear_values = TRUE
  )
  # both 13.4 and 13.99 should map to 9–13 bracket (0.76)
  expect_equal(res$Female_Protein_g_ear, c(0.76, 0.76, 0.71))
})
