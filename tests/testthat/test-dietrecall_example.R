test_that("dietrecall_example structure is valid", {
  data("dietrecall_example", package = "dietrecallkit")

  # ---- 1) Check object type ----
  expect_type(dietrecall_example, "list")
  expect_named(dietrecall_example, c("maintable", "food_details", "food_ingredients_group"))

  # ---- 2) Maintable checks ----
  mt <- dietrecall_example$maintable
  expect_s3_class(mt, "tbl_df")
  expect_true(nrow(mt) <= 100)
  expect_true(all(c(
    "survey_id", "household_id", "survey_date", "recall_number",
    "county", "subcounty", "ward", "cu", "mothers_age_in_years"
  ) %in% names(mt)))

  # ---- 3) food_details checks ----
  fd <- dietrecall_example$food_details
  expect_s3_class(fd, "tbl_df")
  expect_true("survey_id" %in% names(fd))

  # ---- 4) food_ingredients_group checks ----
  fig <- dietrecall_example$food_ingredients_group
  expect_s3_class(fig, "tbl_df")
  expect_true("survey_id" %in% names(fig))

  # ---- 5) Referential integrity ----
  ids <- mt$survey_id
  expect_true(all(unique(fd$survey_id) %in% ids))
  expect_true(all(unique(fig$survey_id) %in% ids))
})


test_that("dietrecall_example works with get_non_gram_foods", {
  skip_if_not_installed("openxlsx")

  # Write example dataset into a temporary Excel file
  tmpfile <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      maintable = dietrecall_example$maintable,
      food_details = dietrecall_example$food_details,
      food_ingredients_group = dietrecall_example$food_ingredients_group
    ),
    tmpfile
  )

  # Run function on the example dataset
  result <- get_non_gram_foods(tmpfile)

  # Basic structure check
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("subcounty", "food_item", "unit", "amount", "gram") %in% names(result)))

  # Result should not contain banned units
  expect_false(any(result$unit %in% c("g from scale", "g from photobook")))

  # Subcounties should match those in maintable
  expect_true(all(result$subcounty %in% dietrecall_example$maintable$subcounty))
})
