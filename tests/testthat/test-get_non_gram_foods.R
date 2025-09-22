test_that("get_non_gram_foods works with example dataset", {
  skip_if_not_installed("openxlsx")

  # Write packaged example data to a temp Excel file
  tmpfile <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      maintable = dietrecall_example$maintable,
      food_details = dietrecall_example$food_details,
      food_ingredients_group = dietrecall_example$food_ingredients_group
    ),
    tmpfile
  )

  result <- get_non_gram_foods(tmpfile)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("subcounty", "food_item", "unit", "amount", "gram") %in% names(result)))

  # subcounty should be subset of maintable values
  expect_true(all(result$subcounty %in% dietrecall_example$maintable$subcounty))

  # amount and gram should be NA
  expect_true(all(is.na(result$amount)))
  expect_true(all(is.na(result$gram)))

  # Should not contain banned units
  expect_false(any(result$unit %in% c("g from scale", "g from photobook")))
})

test_that("get_non_gram_foods errors on missing file or subcounty column", {
  expect_error(get_non_gram_foods("nonexistent.xlsx"))

  tmpfile <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      maintable = dietrecall_example$maintable %>% dplyr::select(-subcounty),
      food_details = dietrecall_example$food_details,
      food_ingredients_group = dietrecall_example$food_ingredients_group
    ),
    tmpfile
  )

  expect_error(get_non_gram_foods(tmpfile))
})

test_that("get_non_gram_foods works with export option", {
  skip_if_not_installed("openxlsx")

  tmpfile <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      maintable = dietrecall_example$maintable,
      food_details = dietrecall_example$food_details,
      food_ingredients_group = dietrecall_example$food_ingredients_group
    ),
    tmpfile
  )

  out_file <- tempfile(fileext = ".xlsx")
  result <- get_non_gram_foods(tmpfile, export_path = out_file)

  expect_s3_class(result, "tbl_df")
  expect_true(file.exists(out_file))

  exported <- readxl::read_excel(out_file, sheet = "unique_food_items")
  expect_true(all(c("subcounty", "food_item", "unit", "amount", "gram") %in% names(exported)))
  expect_equal(nrow(exported), nrow(result))
})
