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
