# =====================================================================
# Unit tests for compute_hfias()
# =====================================================================

test_that("compute_hfias validates inputs correctly", {
  # Non-dataframe input
  expect_error(
    compute_hfias(123, hfia_cols = letters[1:18]),
    "`data` must be a data frame or tibble"
  )

  # Insufficient columns
  df <- tibble::tibble(Q1 = "Yes", F1 = "Rarely (1-2 times)")
  expect_error(
    compute_hfias(df, hfia_cols = names(df)),
    "must include all 18 HFIAS columns"
  )

  # Missing specified columns
  df2 <- tibble::tibble(
    Q1 = "Yes", F1 = "Rarely (1-2 times)",
    Q2 = "No", F2 = "No"
  )
  expect_error(
    compute_hfias(df2, hfia_cols = paste0("Q", 1:9)),
    "The following columns are missing"
  )
})

# ---------------------------------------------------------------------
test_that("compute_hfias correctly converts text responses and computes score/category", {
  df <- tibble::tibble(
    id = 1:3,
    Q1 = c("Yes", "Yes", "No"),
    F1 = c("Rarely (1-2 times)", "Often (more than 10 times)", NA),
    Q2 = c("No", "Yes", "No"),
    F2 = c("No", "Sometimes (3 to 10 times)", "No"),
    Q3 = c("No", "Yes", "No"),
    F3 = c("No", "Rarely (1-2 times)", "No"),
    Q4 = c("No", "Yes", "No"),
    F4 = c("No", "Rarely (1-2 times)", "No"),
    Q5 = c("No", "Yes", "No"),
    F5 = c("No", "Rarely (1-2 times)", "No"),
    Q6 = c("No", "Yes", "No"),
    F6 = c("No", "Rarely (1-2 times)", "No"),
    Q7 = c("No", "Yes", "No"),
    F7 = c("No", "Rarely (1-2 times)", "No"),
    Q8 = c("No", "Yes", "No"),
    F8 = c("No", "Rarely (1-2 times)", "No"),
    Q9 = c("No", "Yes", "No"),
    F9 = c("No", "Rarely (1-2 times)", "No")
  )

  result <- compute_hfias(df, hfia_cols = paste0(rep(c("Q", "F"), each = 9), rep(1:9, 2)))

  # Should retain original columns + hfia_score + hfia_category
  expect_true(all(c("hfia_score", "hfia_category") %in% names(result)))
  expect_true(all(c("id", paste0("Q", 1:9), paste0("F", 1:9)) %in% names(result)))

  # NA frequencies (e.g., for skipped questions) should have been converted to 0
  expect_true(all(!is.na(result$hfia_score)))

  # hfia_score should be integer numeric
  expect_type(result$hfia_score, "double")

  # hfia_category should be integer values within 0:4
  expect_true(all(result$hfia_category %in% 0:4))

  # Score should increase with frequency intensity
  expect_true(result$hfia_score[2] > result$hfia_score[1])
})

# ---------------------------------------------------------------------
test_that("compute_hfias correctly handles numeric inputs directly", {
  df <- tibble::tibble(
    id = 1:2,
    Q1 = c(1, 0),
    F1 = c(2, 0),
    Q2 = c(1, 0),
    F2 = c(3, 0),
    Q3 = c(0, 1),
    F3 = c(0, 2),
    Q4 = c(0, 0),
    F4 = c(0, 0),
    Q5 = c(1, 0),
    F5 = c(1, 0),
    Q6 = c(0, 1),
    F6 = c(0, 3),
    Q7 = c(0, 0),
    F7 = c(0, 0),
    Q8 = c(1, 0),
    F8 = c(3, 0),
    Q9 = c(0, 1),
    F9 = c(0, 2)
  )

  # Correct column order (occurrence, frequency alternating)
  hfia_cols <- c(
    "Q1", "F1", "Q2", "F2", "Q3", "F3", "Q4", "F4", "Q5", "F5",
    "Q6", "F6", "Q7", "F7", "Q8", "F8", "Q9", "F9"
  )

  result <- compute_hfias(df, hfia_cols = hfia_cols)

  # No warnings or errors
  expect_s3_class(result, "tbl_df")

  # Compute expected score from frequency columns (even indices)
  expected_score <- rowSums(df[, hfia_cols[seq(2, 18, by = 2)]], na.rm = TRUE)

  expect_equal(result$hfia_score, expected_score)
})

# ---------------------------------------------------------------------
test_that("compute_hfias raises informative error for unexpected text values", {
  df_bad <- tibble::tibble(
    Q1 = c("Yes", "Maybe", "No"),
    F1 = c("Rarely (1-2 times)", "Often (more than 10 times)", "No"),
    Q2 = c("No", "Yes", "No"),
    F2 = c("No", "Sometimes (3 to 10 times)", "No"),
    Q3 = c("No", "Yes", "No"),
    F3 = c("No", "Rarely (1-2 times)", "No"),
    Q4 = c("No", "Yes", "No"),
    F4 = c("No", "Rarely (1-2 times)", "No"),
    Q5 = c("No", "Yes", "No"),
    F5 = c("No", "Rarely (1-2 times)", "No"),
    Q6 = c("No", "Yes", "No"),
    F6 = c("No", "Rarely (1-2 times)", "No"),
    Q7 = c("No", "Yes", "No"),
    F7 = c("No", "Rarely (1-2 times)", "No"),
    Q8 = c("No", "Yes", "No"),
    F8 = c("No", "Rarely (1-2 times)", "No"),
    Q9 = c("No", "Yes", "No"),
    F9 = c("No", "Rarely (1-2 times)", "No")
  )

  expect_error(
    compute_hfias(df_bad, hfia_cols = names(df_bad)),
    "Please clean these entries before passing"
  )
})

# ---------------------------------------------------------------------
test_that("compute_hfias preserves tibble structure and numeric consistency", {
  df <- tibble::tibble(
    id = 1:2,
    Q1 = c("Yes", "No"),
    F1 = c("Rarely (1-2 times)", "No"),
    Q2 = c("No", "No"),
    F2 = c("No", "No"),
    Q3 = c("No", "No"),
    F3 = c("No", "No"),
    Q4 = c("No", "No"),
    F4 = c("No", "No"),
    Q5 = c("No", "No"),
    F5 = c("No", "No"),
    Q6 = c("No", "No"),
    F6 = c("No", "No"),
    Q7 = c("No", "No"),
    F7 = c("No", "No"),
    Q8 = c("No", "No"),
    F8 = c("No", "No"),
    Q9 = c("No", "No"),
    F9 = c("No", "No")
  )

  result <- compute_hfias(df, hfia_cols = names(df)[-1])

  expect_s3_class(result, "tbl_df")
  expect_true(is.numeric(result$hfia_score))
  expect_true(is.integer(result$hfia_category))
  expect_equal(nrow(result), nrow(df))
  expect_equal(ncol(result), ncol(df) + 2)
})
