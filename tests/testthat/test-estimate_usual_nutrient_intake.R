# -------------------------------------------------------------------------
# test-estimate_usual_nutrient_intake.R
# -------------------------------------------------------------------------

# Helper: expect_warning_free (for happy paths)
expect_warning_free <- function(expr) testthat::expect_silent(expr)

# NOTE:
# Constant intake values are used intentionally to guarantee
# non-identifiable between-person variance (Vbetween = 0),
# so that the NRC skip-adjustment branch is tested deterministically.

# -------------------------------------------------------------------------
# 1. Minimal valid input (expected limited replicate warning)
# -------------------------------------------------------------------------
test_that("estimate_usual_nutrient_intake works with minimal valid input", {
  df <- tibble::tibble(
    id = c(1, 1, 2, 2, 3),
    Energy.kcal_intake = c(1800, 2200, 1500, 1600, 2000),
    Protein.g_intake = c(55, 65, 40, 42, 50)
  )
  suppressWarnings(
    expect_warning(
      res <- estimate_usual_nutrient_intake(
        df,
        id_col = "id",
        nutrient_cols = c("Energy.kcal_intake", "Protein.g_intake")
      ),
      regexp = "Very limited replicate information"
    )
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), length(unique(df$id)))
})

# -------------------------------------------------------------------------
# 2. Auto policy with strong replicate info (no warnings)
# -------------------------------------------------------------------------
test_that("auto policy runs cleanly when replicate info is strong", {
  set.seed(1)
  ids <- rep(1:25, each = 2)
  base <- 1800 + 10 * (1:25)
  y <- as.numeric(rep(base, each = 2)) + rep(c(3, -3), 25)
  df <- tibble::tibble(id = ids, Energy.kcal_intake = y)

  expect_warning_free(
    estimate_usual_nutrient_intake(df,
      id_col = "id", nutrient_cols = "Energy.kcal_intake",
      repeater_policy = "auto"
    )
  )
})

# -------------------------------------------------------------------------
# 3. Auto policy limited replicate info (expected warning)
# -------------------------------------------------------------------------
test_that("auto policy emits expected high-uncertainty and negative between variance warning", {
  set.seed(2)
  df <- tibble::tibble(
    id = rep(1:6, each = 2),
    Energy.kcal_intake = rnorm(12, mean = 2000, sd = 100)
  )

  # Match both potential messages for realism
  suppressWarnings(
    expect_warning(
      estimate_usual_nutrient_intake(
        df,
        id_col = "id",
        nutrient_cols = "Energy.kcal_intake",
        repeater_policy = "auto"
      ),
      regexp = "high-uncertainty|Negative between variance"
    )
  )
})

# -------------------------------------------------------------------------
# 4. Non-identifiable between-person variance handling (deterministic)
# -------------------------------------------------------------------------
test_that("non-identifiable between-person variance skips adjustment and preserves transformed means", {
  df <- tibble::tibble(
    id = rep(1:8, each = 2),
    Energy.kcal_intake = rep(2000, 16) # constant intake → Vbetween = 0
  )

  suppressWarnings(
    expect_warning(
      res <- estimate_usual_nutrient_intake(
        df,
        id_col = "id",
        nutrient_cols = "Energy.kcal_intake"
      ),
      regexp = "Non-identifiable between-person variance|Skipping adjustment"
    )
  )

  # NRC-consistent reference calculation (default cuberoot)
  apply_transform <- function(x) x^(1 / 3)
  inverse_transform <- function(x) x^3

  obs_means <- df %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      obs = inverse_transform(mean(apply_transform(Energy.kcal_intake))),
      .groups = "drop"
    )

  merged <- dplyr::left_join(res, obs_means, by = "id")

  # Usual intake equals NRC-consistent observed mean
  expect_true(
    all(abs(merged$Energy.kcal_intake_usual - merged$obs) < 1e-8)
  )

  # All values should be identical here by construction
  expect_equal(length(unique(merged$Energy.kcal_intake_usual)), 1)
})

# -------------------------------------------------------------------------
# 5. Messy nutrient names and extra columns (no warnings)
# -------------------------------------------------------------------------
test_that("handles messy nutrient names and extra columns without unexpected warnings", {
  set.seed(3)
  df <- tibble::tibble(
    id = rep(1:40, each = 2),
    `Protein (g)` = rnorm(80, 55, 3),
    `Vitamin A (mcg)` = rnorm(80, 450, 50),
    region = rep(c("Urban", "Rural"), 40)
  )

  # Expect either no warning or only benign "high-uncertainty" messages
  suppressWarnings(
    expect_warning(
      res <- estimate_usual_nutrient_intake(
        recall_data = df,
        id_col = "id",
        nutrient_cols = c("Protein (g)", "Vitamin A (mcg)")
      ),
      regexp = "Non-identifiable between-person variance|Limited replicate|high-uncertainty"
    )
  )

  expect_true(any(grepl("Protein \\(g\\)_usual", names(res))))
  expect_true(any(grepl("Vitamin A \\(mcg\\)_usual", names(res))))
  expect_equal(nrow(res), 40)
})

# -------------------------------------------------------------------------
# 6. Transform + jitter (should be warning-free)
# -------------------------------------------------------------------------
test_that("transform and jitter options are stable and warning-free", {
  set.seed(4)
  ids <- rep(1:30, each = 2)
  base <- 1500 + 8 * (1:30)
  y <- as.numeric(rep(base, each = 2)) + rep(c(2, -2), 30)
  df <- tibble::tibble(id = ids, Energy.kcal_intake = y)

  for (tr in c("cuberoot", "log", "sqrt", "none")) {
    expect_warning_free(
      res <- estimate_usual_nutrient_intake(
        df,
        id_col = "id",
        nutrient_cols = "Energy.kcal_intake",
        transform = tr,
        jitter = TRUE
      )
    )
    expect_true("Energy.kcal_intake_usual" %in% names(res))
  }
})

# -------------------------------------------------------------------------
# 7. detailed = TRUE diagnostics reflect NRC variance identifiability
# -------------------------------------------------------------------------
test_that("detailed = TRUE diagnostics correctly reflect variance identifiability", {
  set.seed(5)
  ids <- rep(1:25, each = 2)
  base <- 1700 + 5 * (1:25)
  y <- as.numeric(rep(base, each = 2)) + rep(c(4, -4), 25)
  df <- tibble::tibble(id = ids, Energy.kcal_intake = y)

  suppressWarnings(
    res <- estimate_usual_nutrient_intake(
      recall_data = df,
      id_col = "id",
      nutrient_cols = "Energy.kcal_intake",
      detailed = TRUE
    )
  )

  # Column existence checks
  expect_true("Energy.kcal_intake_observed_mean" %in% names(res))
  expect_true("Energy.kcal_intake_sd_between" %in% names(res))
  expect_true("Energy.kcal_intake_sd_observed" %in% names(res))
  expect_true("Energy.kcal_intake_shrink_ratio" %in% names(res))

  # sd_between should be either NA (non-identifiable) or non-negative
  expect_true(
    all(is.na(res$Energy.kcal_intake_sd_between) |
      res$Energy.kcal_intake_sd_between >= 0)
  )

  # shrink ratio should be 1 (skipped adjustment) or between 0 and 1
  expect_true(
    all(res$Energy.kcal_intake_shrink_ratio >= 0 &
      res$Energy.kcal_intake_shrink_ratio <= 1)
  )
})

# -------------------------------------------------------------------------
# 8. strict policy skips adjustment when replicate info is low
# -------------------------------------------------------------------------
test_that("strict policy skips adjustment when replicate info is low", {
  df <- tibble::tibble(
    id = rep(1:3, each = 2),
    Energy.kcal_intake = rep(2000, 6)
  )

  expect_warning(
    res <- estimate_usual_nutrient_intake(
      df,
      id_col = "id",
      nutrient_cols = "Energy.kcal_intake",
      repeater_policy = "strict"
    ),
    regexp = "Insufficient replicate information"
  )

  expect_true("Energy.kcal_intake_usual" %in% names(res))
})

# -------------------------------------------------------------------------
# 9. handles datasets with no repeaters gracefully
# -------------------------------------------------------------------------
test_that("handles datasets with no repeaters gracefully", {
  df <- tibble::tibble(
    id = 1:5,
    Energy.kcal_intake = c(1800, 2000, 2100, 1900, 2200)
  )

  expect_warning(
    res <- estimate_usual_nutrient_intake(
      df,
      id_col = "id",
      nutrient_cols = "Energy.kcal_intake"
    ),
    regexp = "No repeaters available"
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 5)
})

# -------------------------------------------------------------------------
# 10. Large-scale consistency (suppressing expected warnings)
# -------------------------------------------------------------------------
test_that("function scales consistently under all policies and transforms", {
  set.seed(6)
  ids <- rep(1:100, each = 2)
  df <- tibble::tibble(
    id = ids,
    Energy.kcal_intake = rnorm(200, 2100, 250),
    Protein.g_intake = rnorm(200, 60, 8)
  )

  policies <- c("auto", "strict", "lenient")
  transforms <- c("cuberoot", "log", "sqrt", "none")

  for (p in policies) {
    for (t in transforms) {
      suppressWarnings({
        res <- estimate_usual_nutrient_intake(
          df,
          id_col = "id",
          nutrient_cols = c("Energy.kcal_intake", "Protein.g_intake"),
          transform = t,
          repeater_policy = p,
          detailed = TRUE
        )
      })
      expect_s3_class(res, "tbl_df")
      expect_equal(nrow(res), length(unique(df$id)))
      expect_true(all(sapply(res[-1], is.numeric)))
    }
  }
})

# -------------------------------------------------------------------------
# 11. Input validation and error handling coverage
# -------------------------------------------------------------------------
test_that("estimate_usual_nutrient_intake validates inputs correctly", {
  df <- tibble::tibble(id = 1:3, Energy.kcal_intake = c(2000, 2100, 2200))

  # not a data.frame
  expect_error(estimate_usual_nutrient_intake(
    recall_data = list(df),
    id_col = "id",
    nutrient_cols = "Energy.kcal_intake"
  ), regexp = "must be a data.frame")

  # id_col not a single character
  expect_error(estimate_usual_nutrient_intake(
    recall_data = df,
    id_col = c("id", "id2"),
    nutrient_cols = "Energy.kcal_intake"
  ), regexp = "must be the \\*name\\* of a single column")

  # id_col not found
  expect_error(estimate_usual_nutrient_intake(
    recall_data = df,
    id_col = "wrong_id",
    nutrient_cols = "Energy.kcal_intake"
  ), regexp = "not found in `recall_data`")

  # nutrient_cols not character vector
  expect_error(estimate_usual_nutrient_intake(
    recall_data = df,
    id_col = "id",
    nutrient_cols = 123
  ), regexp = "must be a non-empty character vector")

  # nutrient_cols missing in data
  expect_error(estimate_usual_nutrient_intake(
    recall_data = df,
    id_col = "id",
    nutrient_cols = "MissingNutrient"
  ), regexp = "Nutrient columns not found")

  # nutrient column not numeric
  df$Energy.kcal_intake <- as.character(df$Energy.kcal_intake)
  expect_error(estimate_usual_nutrient_intake(
    recall_data = df,
    id_col = "id",
    nutrient_cols = "Energy.kcal_intake"
  ), regexp = "must be numeric")

  # nutrient column negative values
  df$Energy.kcal_intake <- c(-10, 20, 30)
  expect_error(estimate_usual_nutrient_intake(
    recall_data = df,
    id_col = "id",
    nutrient_cols = "Energy.kcal_intake"
  ), regexp = "contains negative values")
})

# -------------------------------------------------------------------------
# 12. Too few distinct repeaters triggers warning
# -------------------------------------------------------------------------
test_that("warns when there is only one distinct repeater", {
  df <- tibble::tibble(
    id = rep(1, 3), # one ID, multiple days
    Energy.kcal_intake = c(2000, 2100, 2200)
  )

  expect_warning(
    res <- estimate_usual_nutrient_intake(
      df,
      id_col = "id",
      nutrient_cols = "Energy.kcal_intake"
    ),
    regexp = "Too few distinct repeaters"
  )

  expect_true("Energy.kcal_intake_usual" %in% names(res))
})

# -------------------------------------------------------------------------
# 13. lenient policy triggers very low replicate information warning
# -------------------------------------------------------------------------
test_that("lenient policy triggers very low replicate information warning", {
  df <- tibble::tibble(
    id = rep(1:2, each = 2),
    Energy.kcal_intake = c(2000, 2010, 2020, 2030)
  )

  expect_warning(
    estimate_usual_nutrient_intake(
      df,
      id_col = "id",
      nutrient_cols = "Energy.kcal_intake",
      repeater_policy = "lenient"
    ),
    regexp = "Very low replicate information"
  )
})

# -------------------------------------------------------------------------
# 14. Non-identifiable between variance returns transformed observed means
# -------------------------------------------------------------------------
test_that("non-identifiable between variance returns transformed observed means (no shrinkage)", {
  df <- tibble::tibble(
    id = rep(1:5, each = 2),
    Energy.kcal_intake = rep(1800, 10) # constant intake → Vbetween = 0
  )

  suppressWarnings(
    res <- estimate_usual_nutrient_intake(
      df,
      id_col = "id",
      nutrient_cols = "Energy.kcal_intake"
    )
  )

  apply_transform <- function(x) x^(1 / 3)
  inverse_transform <- function(x) x^3

  obs_means <- df %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      obs = inverse_transform(mean(apply_transform(Energy.kcal_intake))),
      .groups = "drop"
    )

  merged <- dplyr::left_join(res, obs_means, by = "id")

  # Exact equality on NRC-consistent scale
  expect_true(
    all(abs(merged$Energy.kcal_intake_usual - merged$obs) < 1e-8)
  )

  # Again, identical by construction
  expect_equal(length(unique(merged$Energy.kcal_intake_usual)), 1)
})
