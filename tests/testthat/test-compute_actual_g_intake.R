# tests/testthat/test-compute_actual_g_intake.R

test_that("compute_actual_g_intake groups results by default", {
  result <- compute_actual_g_intake(
    maintable = dietrecall_example$maintable,
    food_details = dietrecall_example$food_details,
    food_ingredients = dietrecall_example$food_ingredients_group,
    non_gram_foods = non_gram_foods_conversion,
    location_col = "subcounty",
    key = "survey_id"
  )

  # Only grouped columns should exist
  expect_equal(names(result), c("survey_id", "food_item", "actual_gram_intake"))

  # Grouping reduces row count compared to detailed
  result_detailed <- compute_actual_g_intake(
    maintable = dietrecall_example$maintable,
    food_details = dietrecall_example$food_details,
    food_ingredients = dietrecall_example$food_ingredients_group,
    non_gram_foods = non_gram_foods_conversion,
    location_col = "subcounty",
    key = "survey_id",
    group = FALSE
  )
  expect_gt(nrow(result_detailed), nrow(result))

  # Summed intake should match
  grouped_check <- result_detailed %>%
    dplyr::group_by(survey_id, food_item) %>%
    dplyr::summarise(total = sum(actual_gram_intake, na.rm = TRUE), .groups = "drop")

  expect_equal(
    dplyr::arrange(result, survey_id, food_item)$actual_gram_intake,
    dplyr::arrange(grouped_check, survey_id, food_item)$total
  )
})

test_that("compute_actual_g_intake returns detailed results when group = FALSE", {
  result <- compute_actual_g_intake(
    maintable = dietrecall_example$maintable,
    food_details = dietrecall_example$food_details,
    food_ingredients = dietrecall_example$food_ingredients_group,
    non_gram_foods = non_gram_foods_conversion,
    location_col = "subcounty",
    key = "survey_id",
    group = FALSE
  )

  expect_true(all(c(
    "survey_id", "food_item", "amt_consumed", "unit",
    "prop_consumed", "gram_per_unit", "actual_gram_intake"
  ) %in% names(result)))
})

test_that("compute_actual_g_intake errors when non-gram units exist but no conversion data is provided", {
  expect_error(
    compute_actual_g_intake(
      maintable = dietrecall_example$maintable,
      food_details = dietrecall_example$food_details,
      food_ingredients = dietrecall_example$food_ingredients_group,
      location_col = "subcounty",
      key = "survey_id"
    ),
    regexp = "Non-gram units found"
  )
})

test_that("compute_actual_g_intake errors when conversions contain NA/invalid gram values", {
  bad_conv <- non_gram_foods_conversion %>%
    dplyr::mutate(gram = ifelse(dplyr::row_number() == 1, NA, gram))

  expect_error(
    compute_actual_g_intake(
      maintable = dietrecall_example$maintable,
      food_details = dietrecall_example$food_details,
      food_ingredients = dietrecall_example$food_ingredients_group,
      non_gram_foods = bad_conv,
      location_col = "subcounty",
      key = "survey_id"
    ),
    regexp = "Invalid values in 'gram'"
  )
})

test_that("compute_actual_g_intake errors if amount has zero or negative values", {
  bad_conv <- non_gram_foods_conversion %>%
    dplyr::mutate(amount = ifelse(dplyr::row_number() == 1, 0, amount))

  expect_error(
    compute_actual_g_intake(
      maintable = dietrecall_example$maintable,
      food_details = dietrecall_example$food_details,
      food_ingredients = dietrecall_example$food_ingredients_group,
      non_gram_foods = bad_conv,
      location_col = "subcounty",
      key = "survey_id"
    ),
    regexp = "Invalid values in 'amount'"
  )

  bad_conv <- non_gram_foods_conversion %>%
    dplyr::mutate(amount = ifelse(dplyr::row_number() == 1, -1, amount))

  expect_error(
    compute_actual_g_intake(
      maintable = dietrecall_example$maintable,
      food_details = dietrecall_example$food_details,
      food_ingredients = dietrecall_example$food_ingredients_group,
      non_gram_foods = bad_conv,
      location_col = "subcounty",
      key = "survey_id"
    ),
    regexp = "Invalid values in 'amount'"
  )
})

test_that("compute_actual_g_intake errors if gram has zero, negative, or NA values", {
  bad_conv <- non_gram_foods_conversion %>%
    dplyr::mutate(gram = ifelse(dplyr::row_number() == 1, 0, gram))

  expect_error(
    compute_actual_g_intake(
      maintable = dietrecall_example$maintable,
      food_details = dietrecall_example$food_details,
      food_ingredients = dietrecall_example$food_ingredients_group,
      non_gram_foods = bad_conv,
      location_col = "subcounty",
      key = "survey_id"
    ),
    regexp = "Invalid values in 'gram'"
  )

  bad_conv <- non_gram_foods_conversion %>%
    dplyr::mutate(gram = ifelse(dplyr::row_number() == 1, -10, gram))

  expect_error(
    compute_actual_g_intake(
      maintable = dietrecall_example$maintable,
      food_details = dietrecall_example$food_details,
      food_ingredients = dietrecall_example$food_ingredients_group,
      non_gram_foods = bad_conv,
      location_col = "subcounty",
      key = "survey_id"
    ),
    regexp = "Invalid values in 'gram'"
  )

  bad_conv <- non_gram_foods_conversion %>%
    dplyr::mutate(gram = ifelse(dplyr::row_number() == 1, NA, gram))

  expect_error(
    compute_actual_g_intake(
      maintable = dietrecall_example$maintable,
      food_details = dietrecall_example$food_details,
      food_ingredients = dietrecall_example$food_ingredients_group,
      non_gram_foods = bad_conv,
      location_col = "subcounty",
      key = "survey_id"
    ),
    regexp = "Invalid values in 'gram'"
  )
})


test_that("compute_actual_g_intake errors on location mismatch between maintable and conversion data", {
  bad_conv <- non_gram_foods_conversion %>%
    dplyr::mutate(subcounty = ifelse(dplyr::row_number() == 1, "FAKE_SUBCOUNTY", subcounty))

  expect_error(
    compute_actual_g_intake(
      maintable = dietrecall_example$maintable,
      food_details = dietrecall_example$food_details,
      food_ingredients = dietrecall_example$food_ingredients_group,
      non_gram_foods = bad_conv,
      location_col = "subcounty",
      key = "survey_id"
    ),
    regexp = "Mismatch in location values"
  )
})

test_that("compute_actual_g_intake fills missing prop_consumed with 1", {
  fd_mod <- dietrecall_example$food_details %>%
    dplyr::mutate(
      food_item_price_prop_consumed =
        ifelse(dplyr::row_number() == 1, NA, food_item_price_prop_consumed)
    )

  result <- compute_actual_g_intake(
    maintable = dietrecall_example$maintable,
    food_details = fd_mod,
    food_ingredients = dietrecall_example$food_ingredients_group,
    non_gram_foods = non_gram_foods_conversion,
    location_col = "subcounty",
    key = "survey_id",
    group = FALSE
  )

  expect_true(all(!is.na(result$prop_consumed)))
  expect_true(all(result$prop_consumed >= 0))
})


test_that("compute_actual_g_intake correctly computes non-gram food_ingredients actual_gram_intake", {
  mt <- tibble::tibble(
    survey_id = 1,
    subcounty = "TEST"
  )

  fd <- tibble::tibble(
    survey_id                     = 1,
    food_details_rowid            = 1,
    desc_of_food                  = "Ugali",
    qty_food_consumed             = 50,
    amt_of_food_cooked            = 100,
    unit_qty_food_consumed        = "g from scale",
    food_item_price_prop_consumed = 1
  )

  fig <- tibble::tibble(
    survey_id = 1,
    food_details_rowid = 1,
    food_ingredients_used = "Ugali flour",
    food_ingredient_amt = 2,
    food_ingredient_unit = "cup",
    food_ingredient_price_prop_used = 1
  )

  conv <- tibble::tibble(
    subcounty = "TEST",
    food_item = "Ugali flour",
    unit      = "cup",
    amount    = 1,
    gram      = 120
  )

  result <- compute_actual_g_intake(
    maintable = mt,
    food_details = fd,
    food_ingredients = fig,
    non_gram_foods = conv,
    location_col = "subcounty",
    key = "survey_id",
    group = FALSE
  )

  expected <- (2 * 120 * 50) / 100

  actual_val <- result %>%
    dplyr::filter(food_item == "Ugali flour") %>%
    dplyr::pull(actual_gram_intake)

  expect_equal(actual_val, expected)
})

test_that("compute_actual_g_intake accepts custom location_col and key", {
  mt <- dietrecall_example$maintable %>%
    dplyr::rename(district = subcounty, id = survey_id)
  fd <- dietrecall_example$food_details %>%
    dplyr::rename(id = survey_id)
  fig <- dietrecall_example$food_ingredients_group %>%
    dplyr::rename(id = survey_id)
  conv <- non_gram_foods_conversion %>%
    dplyr::rename(district = subcounty)

  result <- compute_actual_g_intake(
    maintable = mt,
    food_details = fd,
    food_ingredients = fig,
    non_gram_foods = conv,
    location_col = "district",
    key = "id",
    group = FALSE
  )

  expect_true(tibble::is_tibble(result))
  expect_true(all(result$id %in% mt$id))
})

test_that("compute_actual_g_intake works when all units are gram-based", {
  fd <- dietrecall_example$food_details %>%
    dplyr::mutate(unit_qty_food_consumed = "g from scale")

  fig <- dietrecall_example$food_ingredients_group %>%
    dplyr::mutate(food_ingredient_unit = "g from photobook")

  result <- compute_actual_g_intake(
    maintable = dietrecall_example$maintable,
    food_details = fd,
    food_ingredients = fig,
    location_col = "subcounty",
    key = "survey_id",
    group = FALSE
  )

  expect_true(all(is.na(result$gram_per_unit)))
  expect_equal(
    result$actual_gram_intake[result$unit == "g from scale"],
    result$amt_consumed[result$unit == "g from scale"]
  )
})

test_that("compute_actual_g_intake trims whitespace in all food sources", {
  # Create minimal inputs with whitespace
  mt <- tibble::tibble(
    survey_id = 1,
    subcounty = "TEST"
  )

  fd <- tibble::tibble(
    survey_id                     = 1,
    food_details_rowid            = 1,
    desc_of_food                  = "  Ugali  ",
    qty_food_consumed             = 100,
    amt_of_food_cooked            = 200,
    unit_qty_food_consumed        = "cup",
    food_item_price_prop_consumed = 1
  )

  fig <- tibble::tibble(
    survey_id = 1,
    food_details_rowid = 1,
    food_ingredients_used = "  Flour  ",
    food_ingredient_amt = 2,
    food_ingredient_unit = "cup",
    food_ingredient_price_prop_used = 1
  )

  conv <- tibble::tibble(
    subcounty = "TEST",
    food_item = "  Flour  ",
    unit      = "cup",
    amount    = 1,
    gram      = 120
  )

  expect_warning(
    result <- compute_actual_g_intake(
      maintable = mt,
      food_details = fd,
      food_ingredients = fig,
      non_gram_foods = conv,
      location_col = "subcounty",
      key = "survey_id",
      group = FALSE
    ),
    regexp = "do not have gram_per_unit assigned"
  )

  # Check trimming applied
  expect_false(any(grepl("^\\s|\\s$", result$food_item)))
  expect_true("Ugali" %in% result$food_item)
  expect_true("Flour" %in% result$food_item)
})

test_that("compute_actual_g_intake warns if non-gram foods in ingredients lack conversion", {
  mt <- tibble::tibble(survey_id = 1, subcounty = "TEST")

  fd <- tibble::tibble(
    survey_id = 1, food_details_rowid = 1,
    desc_of_food = "Ugali", qty_food_consumed = 100, amt_of_food_cooked = 200,
    unit_qty_food_consumed = "g from scale", food_item_price_prop_consumed = 1
  )

  fig <- tibble::tibble(
    survey_id = 1, food_details_rowid = 1,
    food_ingredients_used = "Oil",
    food_ingredient_amt = 2,
    food_ingredient_unit = "tablespoon",
    food_ingredient_price_prop_used = 1
  )

  # Conversion file does not include "Oil"
  conv <- tibble::tibble(
    subcounty = "TEST",
    food_item = "Flour", unit = "cup", amount = 1, gram = 120
  )

  expect_warning(
    compute_actual_g_intake(
      maintable = mt, food_details = fd, food_ingredients = fig,
      non_gram_foods = conv, location_col = "subcounty", key = "survey_id",
      group = FALSE
    ),
    regexp = "do not have gram_per_unit assigned"
  )
})
