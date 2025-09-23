# tests/testthat/test-get_non_gram_foods.R

test_that("get_non_gram_foods works with example dataset", {
  skip_if_not_installed("openxlsx")

  # --- subset for speed but maintain integrity ---
  subset_ids <- head(dietrecall_example$maintable$survey_id, 3)
  example_small <- list(
    maintable = dplyr::filter(dietrecall_example$maintable, survey_id %in% subset_ids),
    food_details = dplyr::filter(dietrecall_example$food_details, survey_id %in% subset_ids),
    food_ingredients_group = dplyr::filter(dietrecall_example$food_ingredients_group, survey_id %in% subset_ids)
  )

  tmpfile <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(example_small, tmpfile)

  result <- get_non_gram_foods(tmpfile, location_col = "subcounty")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("subcounty", "food_item", "unit", "amount", "gram") %in% names(result)))

  expect_true(all(result$subcounty %in% example_small$maintable$subcounty))
  expect_true(all(is.na(result$amount)))
  expect_true(all(is.na(result$gram)))
  expect_false(any(result$unit %in% c("g from scale", "g from photobook")))
})

test_that("get_non_gram_foods errors on missing file or location column", {
  expect_error(get_non_gram_foods("nonexistent.xlsx"))

  subset_ids <- head(dietrecall_example$maintable$survey_id, 3)
  mt <- dplyr::filter(dietrecall_example$maintable, survey_id %in% subset_ids) %>%
    dplyr::select(-subcounty)

  tmpfile <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      maintable = mt,
      food_details = dplyr::filter(dietrecall_example$food_details, survey_id %in% subset_ids),
      food_ingredients_group = dplyr::filter(dietrecall_example$food_ingredients_group, survey_id %in% subset_ids)
    ),
    tmpfile
  )

  expect_error(get_non_gram_foods(tmpfile, location_col = "subcounty"))
})

test_that("get_non_gram_foods works with export option", {
  skip_if_not_installed("openxlsx")

  subset_ids <- head(dietrecall_example$maintable$survey_id, 3)
  example_small <- list(
    maintable = dplyr::filter(dietrecall_example$maintable, survey_id %in% subset_ids),
    food_details = dplyr::filter(dietrecall_example$food_details, survey_id %in% subset_ids),
    food_ingredients_group = dplyr::filter(dietrecall_example$food_ingredients_group, survey_id %in% subset_ids)
  )

  tmpfile <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(example_small, tmpfile)

  out_file <- tempfile(fileext = ".xlsx")
  result <- get_non_gram_foods(tmpfile, export_path = out_file)

  expect_s3_class(result, "tbl_df")
  expect_true(file.exists(out_file))

  exported <- readxl::read_excel(out_file, sheet = "non_gram_foods")
  expect_true(all(c("subcounty", "food_item", "unit", "amount", "gram") %in% names(exported)))
  expect_equal(nrow(exported), nrow(result))
})

test_that("get_non_gram_foods returns message and empty tibble when all units are grams", {
  skip_if_not_installed("openxlsx")

  subset_ids <- head(dietrecall_example$maintable$survey_id, 3)
  fd <- dplyr::filter(dietrecall_example$food_details, survey_id %in% subset_ids) %>%
    dplyr::mutate(unit_qty_food_consumed = "g from scale")

  fig <- dplyr::filter(dietrecall_example$food_ingredients_group, survey_id %in% subset_ids) %>%
    dplyr::mutate(food_ingredient_unit = "g from photobook")

  tmpfile <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      maintable = dplyr::filter(dietrecall_example$maintable, survey_id %in% subset_ids),
      food_details = fd,
      food_ingredients_group = fig
    ),
    tmpfile
  )

  expect_message(
    result <- get_non_gram_foods(tmpfile),
    regexp = "All food items in the dataset are recorded in grams"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true(all(c("subcounty", "food_item", "unit", "amount", "gram") %in% names(result)))

  out_file <- tempfile(fileext = ".xlsx")
  get_non_gram_foods(tmpfile, export_path = out_file)
  expect_false(file.exists(out_file))
})

test_that("get_non_gram_foods respects desc_of_food filtering", {
  subset_ids <- head(dietrecall_example$maintable$survey_id, 3)

  fd <- dplyr::filter(dietrecall_example$food_details, survey_id %in% subset_ids) %>%
    dplyr::mutate(desc_of_food = ifelse(dplyr::row_number() == 1, NA, desc_of_food))

  tmpfile <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      maintable = dplyr::filter(dietrecall_example$maintable, survey_id %in% subset_ids),
      food_details = fd,
      food_ingredients_group = dplyr::filter(dietrecall_example$food_ingredients_group, survey_id %in% subset_ids)
    ),
    tmpfile
  )

  result <- get_non_gram_foods(tmpfile)
  expect_false(any(is.na(result$food_item)))
})

test_that("get_non_gram_foods works with custom sheet and location names", {
  subset_ids <- head(dietrecall_example$maintable$survey_id, 3)

  mt <- dplyr::filter(dietrecall_example$maintable, survey_id %in% subset_ids) %>%
    dplyr::rename(district = subcounty)

  tmpfile <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      mt = mt,
      fd = dplyr::filter(dietrecall_example$food_details, survey_id %in% subset_ids),
      fig = dplyr::filter(dietrecall_example$food_ingredients_group, survey_id %in% subset_ids)
    ),
    tmpfile
  )

  result <- get_non_gram_foods(
    tmpfile,
    maintable_sheet = "mt",
    food_details_sheet = "fd",
    food_ingredients_sheet = "fig",
    location_col = "district"
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("district", "food_item", "unit", "amount", "gram") %in% names(result)))
  expect_true(all(result$district %in% mt$district))
})
