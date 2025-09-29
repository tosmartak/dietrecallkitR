# tests/testthat/test-get_non_gram_foods.R

test_that("get_non_gram_foods works with example dataset", {
  result <- get_non_gram_foods(
    maintable = dietrecall_example$maintable,
    food_details = dietrecall_example$food_details,
    food_ingredients = dietrecall_example$food_ingredients_group,
    location_col = "subcounty",
    key = "survey_id"
  )

  expect_true(tibble::is_tibble(result))
  expect_true(all(c("subcounty", "food_item", "unit", "amount", "gram") %in% names(result)))

  # location_col integrity
  expect_true(all(result$subcounty %in% dietrecall_example$maintable$subcounty))

  # amount and gram should be NA
  expect_true(all(is.na(result$amount)))
  expect_true(all(is.na(result$gram)))

  # Should not contain banned units
  expect_false(any(result$unit %in% c("g from scale", "g from photobook")))
})


test_that("get_non_gram_foods errors if required columns are missing", {
  mt <- dietrecall_example$maintable %>% dplyr::select(-subcounty)

  expect_error(
    get_non_gram_foods(
      maintable = mt,
      food_details = dietrecall_example$food_details,
      food_ingredients = dietrecall_example$food_ingredients_group,
      location_col = "subcounty",
      key = "survey_id"
    )
  )

  fd <- dietrecall_example$food_details %>% dplyr::select(-survey_id)
  expect_error(
    get_non_gram_foods(
      maintable = dietrecall_example$maintable,
      food_details = fd,
      food_ingredients = dietrecall_example$food_ingredients_group,
      location_col = "subcounty",
      key = "survey_id"
    )
  )

  # Wrong key
  expect_error(
    get_non_gram_foods(
      maintable = dietrecall_example$maintable,
      food_details = dietrecall_example$food_details,
      food_ingredients = dietrecall_example$food_ingredients_group,
      location_col = "subcounty",
      key = "wrong_id"
    )
  )
})


test_that("get_non_gram_foods trims whitespace from food_item", {
  fd <- dietrecall_example$food_details %>%
    dplyr::mutate(desc_of_food = paste0("  ", desc_of_food, "  "))

  result <- get_non_gram_foods(
    maintable = dietrecall_example$maintable,
    food_details = fd,
    food_ingredients = dietrecall_example$food_ingredients_group,
    location_col = "subcounty",
    key = "survey_id"
  )

  expect_false(any(grepl("^\\s|\\s$", result$food_item)))
})


test_that("get_non_gram_foods deduplicates multiple units per food item", {
  fd <- dietrecall_example$food_details %>%
    dplyr::mutate(unit_qty_food_consumed = ifelse(dplyr::row_number() <= 2, "cup", unit_qty_food_consumed))

  result <- get_non_gram_foods(
    maintable = dietrecall_example$maintable,
    food_details = fd,
    food_ingredients = dietrecall_example$food_ingredients_group,
    location_col = "subcounty",
    key = "survey_id"
  )

  dupes <- result %>%
    dplyr::count(subcounty, food_item, unit) %>%
    dplyr::filter(n > 1)

  expect_equal(nrow(dupes), 0)
})


test_that("get_non_gram_foods works with export option", {
  out_file <- tempfile(fileext = ".xlsx")

  result <- get_non_gram_foods(
    maintable = dietrecall_example$maintable,
    food_details = dietrecall_example$food_details,
    food_ingredients = dietrecall_example$food_ingredients_group,
    location_col = "subcounty",
    key = "survey_id",
    export_path = out_file
  )

  expect_true(tibble::is_tibble(result))
  expect_true(file.exists(out_file))

  exported <- readxl::read_excel(out_file, sheet = "non_gram_foods")
  expect_true(all(c("subcounty", "food_item", "unit", "amount", "gram") %in% names(exported)))
  expect_equal(nrow(exported), nrow(result))
})

test_that("get_non_gram_foods returns message and empty tibble when all units are grams", {
  fd <- dietrecall_example$food_details %>%
    dplyr::mutate(unit_qty_food_consumed = "g from scale")
  fig <- dietrecall_example$food_ingredients_group %>%
    dplyr::mutate(food_ingredient_unit = "g from photobook")

  expect_message(
    result <- get_non_gram_foods(
      maintable = dietrecall_example$maintable,
      food_details = fd,
      food_ingredients = fig,
      location_col = "subcounty",
      key = "survey_id"
    ),
    regexp = "All food items in the dataset are recorded in grams"
  )

  expect_true(tibble::is_tibble(result))
  expect_equal(nrow(result), 0)
  expect_true(all(c("subcounty", "food_item", "unit", "amount", "gram") %in% names(result)))

  out_file <- tempfile(fileext = ".xlsx")
  get_non_gram_foods(
    maintable = dietrecall_example$maintable,
    food_details = fd,
    food_ingredients = fig,
    location_col = "subcounty",
    key = "survey_id",
    export_path = out_file
  )
  expect_false(file.exists(out_file))
})

test_that("get_non_gram_foods respects desc_of_food filtering", {
  fd <- dietrecall_example$food_details %>%
    dplyr::mutate(desc_of_food = ifelse(dplyr::row_number() == 1, NA, desc_of_food))

  result <- get_non_gram_foods(
    maintable = dietrecall_example$maintable,
    food_details = fd,
    food_ingredients = dietrecall_example$food_ingredients_group,
    location_col = "subcounty",
    key = "survey_id"
  )

  expect_false(any(is.na(result$food_item)))
})

test_that("get_non_gram_foods works with custom location and key names", {
  # Rename subcounty → district, survey_id → id
  mt <- dietrecall_example$maintable %>%
    dplyr::rename(district = subcounty, id = survey_id)
  fd <- dietrecall_example$food_details %>%
    dplyr::rename(id = survey_id)
  fig <- dietrecall_example$food_ingredients_group %>%
    dplyr::rename(id = survey_id)

  result <- get_non_gram_foods(
    maintable = mt,
    food_details = fd,
    food_ingredients = fig,
    location_col = "district",
    key = "id"
  )

  expect_true(tibble::is_tibble(result))
  expect_true(all(c("district", "food_item", "unit", "amount", "gram") %in% names(result)))
  expect_true(all(result$district %in% mt$district))
})
