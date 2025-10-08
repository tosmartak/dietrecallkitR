# tests/testthat/test-compute_gdqs.R

test_that("compute_gdqs works with use_fct_db = TRUE", {
  recall <- tibble::tibble(
    survey_id = 1:4,
    food_item = c(
      "Meat Samosa",
      "Biscuits and Cookies, Savoury",
      "Tangerine (sandara), juice",
      "Rabbit meat, stewed"
    ),
    actual_gram_intake = c(50, 30, 200, 120)
  )

  result <- compute_gdqs(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    use_fct_db    = TRUE
  )

  expect_true(all(c("gdqs_plus", "gdqs_minus", "gdqs_overall", "gdqs_risk") %in% names(result)))
  expect_equal(nrow(result), 4)
})

test_that("compute_gdqs works with custom mapping", {
  recall <- tibble::tibble(
    id = c(1, 1, 2),
    food = c("Ugali", "Beans", "Soda"),
    actual_gram_intake = c(200, 50, 300)
  )

  gdqs_map <- tibble::tibble(
    item     = c("Ugali", "Beans", "Soda"),
    category = c("Whole Grains", "Legumes", "Sugar-Sweetened Beverages")
  )

  result <- compute_gdqs(
    recall_data = recall,
    id_col = "id",
    food_item_col = "food",
    actual_gram_intake_col = "actual_gram_intake",
    use_fct_db = FALSE,
    gdqs_map_data = gdqs_map,
    gdqs_map_food = "item",
    gdqs_map_col = "category"
  )

  expect_equal(nrow(result), 2)
  expect_true(all(c("gdqs_plus", "gdqs_minus", "gdqs_overall", "gdqs_risk") %in% names(result)))
})

test_that("compute_gdqs warns and returns zeros when no mapped foods", {
  recall <- tibble::tibble(
    survey_id = 1:2,
    food_item = c("Unknown1", "Unknown2"),
    actual_gram_intake = c(100, 200)
  )

  gdqs_map <- tibble::tibble(
    food_item = "Ugali",
    gdqs      = "Whole Grains"
  )

  expect_warning(
    expect_warning(
      result <- compute_gdqs(
        recall_data   = recall,
        id_col        = "survey_id",
        food_item_col = "food_item",
        use_fct_db    = FALSE,
        gdqs_map_data = gdqs_map,
        gdqs_map_food = "food_item",
        gdqs_map_col  = "gdqs"
      ),
      regexp = "Unmapped food items"
    ),
    regexp = "No mapped foods found"
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$gdqs_plus, c(0, 0))
  expect_equal(result$gdqs_overall, c(0, 0))
  expect_equal(result$gdqs_risk, c("High", "High"))
})

test_that("compute_gdqs errors on invalid GDQS categories", {
  recall <- tibble::tibble(
    survey_id = 1,
    food_item = "Ugali",
    actual_gram_intake = 200
  )

  gdqs_map <- tibble::tibble(
    food_item = "Ugali",
    gdqs      = "Invalid Category"
  )

  expect_error(
    compute_gdqs(
      recall_data   = recall,
      id_col        = "survey_id",
      food_item_col = "food_item",
      use_fct_db    = FALSE,
      gdqs_map_data = gdqs_map,
      gdqs_map_food = "food_item",
      gdqs_map_col  = "gdqs"
    ),
    regexp = "Invalid GDQS categories"
  )
})

test_that("compute_gdqs errors on invalid input combinations", {
  recall <- tibble::tibble(id = 1, food = "Ugali", actual_gram_intake = 100)
  gdqs_map <- tibble::tibble(item = "Ugali", gdqs = "Whole Grains")

  # both use_fct_db and gdqs_map_data
  expect_error(
    compute_gdqs(
      recall_data   = recall,
      id_col        = "id",
      food_item_col = "food",
      use_fct_db    = TRUE,
      gdqs_map_data = gdqs_map,
      gdqs_map_food = "item",
      gdqs_map_col  = "gdqs"
    )
  )

  # missing gdqs_map_data when required
  expect_error(
    compute_gdqs(
      recall_data   = recall,
      id_col        = "id",
      food_item_col = "food",
      use_fct_db    = FALSE
    )
  )

  # wrong id_col
  expect_error(
    compute_gdqs(
      recall_data   = recall,
      id_col        = "wrong_id",
      food_item_col = "food",
      use_fct_db    = TRUE
    )
  )

  # wrong food_item_col
  expect_error(
    compute_gdqs(
      recall_data   = recall,
      id_col        = "id",
      food_item_col = "wrong_col",
      use_fct_db    = TRUE
    )
  )
})

test_that("compute_gdqs splits multi-category foods and divides intake correctly", {
  recall <- tibble::tibble(
    survey_id = 1,
    food_item = "Mixed Dish",
    actual_gram_intake = 100
  )

  gdqs_map <- tibble::tibble(
    food_item = "Mixed Dish",
    gdqs      = "Whole Grains;Legumes"
  )

  result <- compute_gdqs(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    use_fct_db    = FALSE,
    gdqs_map_data = gdqs_map,
    gdqs_map_food = "food_item",
    gdqs_map_col  = "gdqs"
  )

  expect_equal(result$gdqs_whole_grains_g_intake, 50)
  expect_equal(result$gdqs_legumes_g_intake, 50)
})

test_that("compute_gdqs allocates points correctly", {
  recall <- tibble::tibble(
    survey_id = c(1, 2),
    food_item = c("Beans", "Soda"),
    actual_gram_intake = c(50, 300)
  )

  gdqs_map <- tibble::tibble(
    food_item = c("Beans", "Soda"),
    gdqs      = c("Legumes", "Sugar-Sweetened Beverages")
  )

  result <- compute_gdqs(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    use_fct_db    = FALSE,
    gdqs_map_data = gdqs_map,
    gdqs_map_food = "food_item",
    gdqs_map_col  = "gdqs"
  )

  # Legumes 50g → should get 4 points, SSB 300g → should get 0 points
  expect_equal(result$gdqs_legumes_points[1], 4)
  expect_equal(result$gdqs_sugar_sweetened_beverages_points[2], 0)
})

test_that("compute_gdqs computes GDQS summary and risk correctly", {
  recall <- tibble::tibble(
    survey_id = 1,
    food_item = c("Beans", "Ugali", "Soda"),
    actual_gram_intake = c(50, 200, 300)
  )

  gdqs_map <- tibble::tibble(
    food_item = c("Beans", "Ugali", "Soda"),
    gdqs      = c("Legumes", "Whole Grains", "Sugar-Sweetened Beverages")
  )

  result <- compute_gdqs(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    use_fct_db    = FALSE,
    gdqs_map_data = gdqs_map,
    gdqs_map_food = "food_item",
    gdqs_map_col  = "gdqs"
  )

  expect_equal(result$gdqs_overall, result$gdqs_plus + result$gdqs_minus)

  # Risk classification based on score
  risk <- ifelse(result$gdqs_overall < 15, "High",
    ifelse(result$gdqs_overall < 23, "Moderate", "Low")
  )
  expect_equal(result$gdqs_risk, risk)
})

test_that("compute_gdqs ensures consistent column order, numeric type, and valid values", {
  recall <- tibble::tibble(
    survey_id = c(1, 2),
    food_item = c("Beans", "Soda"),
    actual_gram_intake = c(50, 300)
  )

  gdqs_map <- tibble::tibble(
    food_item = c("Beans", "Soda"),
    gdqs      = c("Legumes", "Sugar-Sweetened Beverages")
  )

  result <- compute_gdqs(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    use_fct_db    = FALSE,
    gdqs_map_data = gdqs_map,
    gdqs_map_food = "food_item",
    gdqs_map_col  = "gdqs"
  )

  # Ensure all expected gdqs columns appear in correct order
  expected_cols <- c(
    "survey_id",
    # GDQS intake columns (with gdqs_ prefix and snake_case)
    "gdqs_citrus_fruits_g_intake", "gdqs_deep_orange_fruits_g_intake", "gdqs_other_fruits_g_intake",
    "gdqs_dark_green_leafy_vegetables_g_intake", "gdqs_cruciferous_vegetables_g_intake",
    "gdqs_deep_orange_vegetables_g_intake", "gdqs_other_vegetables_g_intake", "gdqs_legumes_g_intake",
    "gdqs_deep_orange_tubers_g_intake", "gdqs_nuts_and_seeds_g_intake", "gdqs_whole_grains_g_intake",
    "gdqs_liquid_oils_g_intake", "gdqs_fish_and_shellfish_g_intake", "gdqs_poultry_and_game_meat_g_intake",
    "gdqs_low_fat_dairy_g_intake", "gdqs_eggs_g_intake",
    "gdqs_high_fat_dairy_g_intake", "gdqs_red_meat_g_intake", "gdqs_processed_meat_g_intake",
    "gdqs_refined_grains_and_baked_goods_g_intake", "gdqs_sweets_and_ice_cream_g_intake",
    "gdqs_sugar_sweetened_beverages_g_intake", "gdqs_juice_g_intake",
    "gdqs_white_roots_and_tubers_g_intake", "gdqs_purchased_deep_fried_foods_g_intake",

    # Consumed flags
    "gdqs_citrus_fruits_consumed", "gdqs_deep_orange_fruits_consumed",
    "gdqs_other_fruits_consumed", "gdqs_dark_green_leafy_vegetables_consumed",
    "gdqs_cruciferous_vegetables_consumed", "gdqs_deep_orange_vegetables_consumed",
    "gdqs_other_vegetables_consumed", "gdqs_legumes_consumed",
    "gdqs_deep_orange_tubers_consumed", "gdqs_nuts_and_seeds_consumed",
    "gdqs_whole_grains_consumed", "gdqs_liquid_oils_consumed",
    "gdqs_fish_and_shellfish_consumed", "gdqs_poultry_and_game_meat_consumed",
    "gdqs_low_fat_dairy_consumed", "gdqs_eggs_consumed",
    "gdqs_high_fat_dairy_consumed", "gdqs_red_meat_consumed",
    "gdqs_processed_meat_consumed", "gdqs_refined_grains_and_baked_goods_consumed",
    "gdqs_sweets_and_ice_cream_consumed", "gdqs_sugar_sweetened_beverages_consumed",
    "gdqs_juice_consumed", "gdqs_white_roots_and_tubers_consumed",
    "gdqs_purchased_deep_fried_foods_consumed",

    # Points columns
    "gdqs_citrus_fruits_points", "gdqs_deep_orange_fruits_points",
    "gdqs_other_fruits_points", "gdqs_dark_green_leafy_vegetables_points",
    "gdqs_cruciferous_vegetables_points", "gdqs_deep_orange_vegetables_points",
    "gdqs_other_vegetables_points", "gdqs_legumes_points",
    "gdqs_deep_orange_tubers_points", "gdqs_nuts_and_seeds_points",
    "gdqs_whole_grains_points", "gdqs_liquid_oils_points",
    "gdqs_fish_and_shellfish_points", "gdqs_poultry_and_game_meat_points",
    "gdqs_low_fat_dairy_points", "gdqs_eggs_points",
    "gdqs_high_fat_dairy_points", "gdqs_red_meat_points",
    "gdqs_processed_meat_points", "gdqs_refined_grains_and_baked_goods_points",
    "gdqs_sweets_and_ice_cream_points", "gdqs_sugar_sweetened_beverages_points",
    "gdqs_juice_points", "gdqs_white_roots_and_tubers_points",
    "gdqs_purchased_deep_fried_foods_points",

    # Summary scores
    "gdqs_plus", "gdqs_minus", "gdqs_overall", "gdqs_risk"
  )

  expect_identical(names(result), expected_cols)

  # Numeric type checks
  num_cols <- setdiff(names(result), c("survey_id", "gdqs_risk"))
  expect_true(all(sapply(result[num_cols], is.numeric)))
})
