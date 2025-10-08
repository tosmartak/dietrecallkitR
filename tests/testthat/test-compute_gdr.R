# tests/testthat/test-compute_gdr.R

test_that("compute_gdr works with use_fct_db = TRUE", {
  recall <- tibble::tibble(
    survey_id = 1:5,
    food_item = c(
      "Biscuits and Cookies (BRITANNIA)", "Orange (chungwa), pulp, raw", "Pumpkin Leaves",
      "Crisps, Banana", "Beans, broad, dry, raw"
    )
  )

  result <- compute_gdr(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    use_fct_db    = TRUE
  )

  expect_true("gdrplus" %in% names(result))
  expect_true("gdrminus" %in% names(result))
  expect_true("gdr_score" %in% names(result))
  expect_equal(nrow(result), 5)
})

test_that("compute_gdr works with custom mapping", {
  recall <- tibble::tibble(
    id   = c(1, 1, 2),
    food = c("Ugali", "Beans", "Rice")
  )

  gdr_map <- tibble::tibble(
    item     = c("Ugali", "Beans", "Rice"),
    category = c("Whole Grains", "Legumes", "Whole Grains")
  )

  result <- compute_gdr(
    recall_data   = recall,
    id_col        = "id",
    food_item_col = "food",
    use_fct_db    = FALSE,
    gdr_map_data  = gdr_map,
    gdr_map_food  = "item",
    gdr_map_col   = "category"
  )

  expect_true("gdrplus" %in% names(result))
  expect_true("gdrminus" %in% names(result))
  expect_true("gdr_score" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("compute_gdr warns and returns minimal output when no mapped foods found", {
  recall <- tibble::tibble(
    survey_id = 1:2,
    food_item = c("Unknown1", "Unknown2")
  )

  gdr_map <- tibble::tibble(
    food_item = "Ugali",
    gdr       = "Whole Grains"
  )

  expect_warning(
    expect_warning(
      result <- compute_gdr(
        recall_data   = recall,
        id_col        = "survey_id",
        food_item_col = "food_item",
        use_fct_db    = FALSE,
        gdr_map_data  = gdr_map,
        gdr_map_food  = "food_item",
        gdr_map_col   = "gdr"
      ),
      regexp = "Unmapped food items"
    ),
    regexp = "No mapped foods found"
  )

  expect_equal(names(result), c("survey_id", "gdrplus", "gdrminus", "gdr_score"))
  expect_equal(nrow(result), 2)
  expect_true(all(result$gdrplus == 0 & result$gdrminus == 0 & result$gdr_score == 0))
})

test_that("compute_gdr errors on invalid categories", {
  recall <- tibble::tibble(
    survey_id = 1,
    food_item = "Ugali"
  )

  gdr_map <- tibble::tibble(
    food_item = "Ugali",
    gdr       = "Invalid Category"
  )

  expect_error(
    compute_gdr(
      recall_data   = recall,
      id_col        = "survey_id",
      food_item_col = "food_item",
      use_fct_db    = FALSE,
      gdr_map_data  = gdr_map,
      gdr_map_food  = "food_item",
      gdr_map_col   = "gdr"
    ),
    regexp = "Invalid GDR categories"
  )
})

test_that("compute_gdr errors on invalid input combinations", {
  recall <- tibble::tibble(id = 1, food = "Ugali")
  gdr_map <- tibble::tibble(item = "Ugali", gdr = "Whole Grains")

  # both use_fct_db and gdr_map_data
  expect_error(
    compute_gdr(
      recall_data   = recall,
      id_col        = "id",
      food_item_col = "food",
      use_fct_db    = TRUE,
      gdr_map_data  = gdr_map,
      gdr_map_food  = "item",
      gdr_map_col   = "gdr"
    )
  )

  # missing gdr_map_data when required
  expect_error(
    compute_gdr(
      recall_data   = recall,
      id_col        = "id",
      food_item_col = "food",
      use_fct_db    = FALSE
    )
  )

  # wrong id_col
  expect_error(
    compute_gdr(
      recall_data   = recall,
      id_col        = "wrong_id",
      food_item_col = "food",
      use_fct_db    = TRUE
    )
  )

  # wrong food_item_col
  expect_error(
    compute_gdr(
      recall_data   = recall,
      id_col        = "id",
      food_item_col = "wrong_col",
      use_fct_db    = TRUE
    )
  )
})

test_that("compute_gdr handles multi-category food items", {
  recall <- tibble::tibble(
    survey_id = 1,
    food_item = "Mixed Dish"
  )

  gdr_map <- tibble::tibble(
    food_item = "Mixed Dish",
    gdr       = "Whole Grains;Legumes;Other Vegetables"
  )

  result <- compute_gdr(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    use_fct_db    = FALSE,
    gdr_map_data  = gdr_map,
    gdr_map_food  = "food_item",
    gdr_map_col   = "gdr"
  )

  multi_cols <- grep("^gdr_", names(result), value = TRUE)
  expect_true(any(grepl("whole_grains", multi_cols)))
  expect_true(any(grepl("legumes", multi_cols)))
  expect_true(any(grepl("other_vegetables", multi_cols)))
  expect_equal(result$gdrplus, 3)
})

test_that("compute_gdr applies processed meat special rule", {
  recall <- tibble::tibble(
    survey_id = 1,
    food_item = "Sausage"
  )

  gdr_map <- tibble::tibble(
    food_item = "Sausage",
    gdr       = "Processed Meat"
  )

  result <- compute_gdr(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    use_fct_db    = FALSE,
    gdr_map_data  = gdr_map,
    gdr_map_food  = "food_item",
    gdr_map_col   = "gdr"
  )

  expect_equal(result$gdr_processed_meat, 2)
})

test_that("compute_gdr ensures consistent column order, numeric type, and valid values", {
  recall <- tibble::tibble(
    survey_id = c(1, 2, 3),
    food_item = c("Ugali", "Beans", "Sausage, beef, grilled")
  )

  gdr_map <- tibble::tibble(
    food_item = c("Ugali", "Beans", "Sausage, beef, grilled"),
    gdr       = c("Whole Grains", "Legumes", "Processed Meat")
  )

  result <- compute_gdr(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    use_fct_db    = FALSE,
    gdr_map_data  = gdr_map,
    gdr_map_food  = "food_item",
    gdr_map_col   = "gdr"
  )

  expected_cols <- c(
    "survey_id",
    "gdr_whole_grains", "gdr_legumes", "gdr_nuts_and_seeds",
    "gdr_vitamin_a_rich_orange_vegetables_roots_and_tubers",
    "gdr_dark_green_leafy_vegetables", "gdr_other_vegetables",
    "gdr_vitamin_a_rich_fruits", "gdr_citrus_fruits", "gdr_other_fruits",
    "gdr_sodas_sugar_sweetened_beverages", "gdr_baked_grain_based_sweets",
    "gdr_other_sweets", "gdr_processed_meat", "gdr_unprocessed_red_meat",
    "gdr_deep_fried_foods", "gdr_fast_food_and_instant_noodles",
    "gdr_packaged_ultra_processed_salty_snacks",
    "gdrplus", "gdrminus", "gdr_score"
  )

  # Column order is exactly as expected
  expect_identical(names(result), expected_cols)

  # All gdr_* dummy columns should be numeric
  gdr_dummy_cols <- grep("^gdr_", names(result), value = TRUE)
  expect_true(all(sapply(result[gdr_dummy_cols], is.numeric)))

  # All values in dummy columns are valid: 0/1 normally, 0/2 for processed meat
  for (col in gdr_dummy_cols) {
    if (col == "gdr_processed_meat") {
      expect_true(all(result[[col]] %in% c(0, 2)))
    } else if (!col %in% c("gdrplus", "gdrminus", "gdr_score")) {
      expect_true(all(result[[col]] %in% c(0, 1)))
    }
  }
})

test_that("compute_gdr enforces correct gdr_score calculation", {
  recall <- tibble::tibble(
    survey_id = c(1, 2),
    food_item = c("Ugali", "Processed Meat")
  )

  gdr_map <- tibble::tibble(
    food_item = c("Ugali", "Processed Meat"),
    gdr       = c("Whole Grains", "Processed Meat")
  )

  result <- compute_gdr(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    use_fct_db    = FALSE,
    gdr_map_data  = gdr_map,
    gdr_map_food  = "food_item",
    gdr_map_col   = "gdr"
  )

  # Check formula: gdr_score = gdrplus - gdrminus + 9
  expect_equal(
    result$gdr_score,
    result$gdrplus - result$gdrminus + 9
  )
})

test_that("compute_mdd errors on wrong id_col or food_item_col", {
  recall <- tibble::tibble(id = 1, food = "Ugali")
  fg_map <- tibble::tibble(
    item = "Ugali",
    group = "Grains, White Roots And Tubers And Plantains"
  )

  # wrong id_col
  expect_error(
    compute_mdd(
      recall_data   = recall,
      id_col        = "wrong_id",
      food_item_col = "food",
      life_stage    = "woman",
      use_fct_db    = FALSE,
      fg_map_data   = fg_map,
      fg_map_food   = "item",
      fg_map_col    = "group"
    )
  )

  # wrong food_item_col
  expect_error(
    compute_mdd(
      recall_data   = recall,
      id_col        = "id",
      food_item_col = "wrong_col",
      life_stage    = "woman",
      use_fct_db    = FALSE,
      fg_map_data   = fg_map,
      fg_map_food   = "item",
      fg_map_col    = "group"
    )
  )
})

test_that("compute_mdd ensures consistent column order, numeric type, and valid values", {
  recall <- tibble::tibble(
    survey_id = c(1, 2, 3),
    food_item = c("Ugali", "Beans", "Spinach")
  )

  fg_map <- tibble::tibble(
    food_item = c("Ugali", "Beans", "Spinach"),
    group = c(
      "Grains, White Roots And Tubers And Plantains",
      "Pulses (Beans, Peas And Lentils)",
      "Dark Green Leafy Vegetables"
    )
  )

  result <- compute_mdd(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    life_stage    = "woman",
    use_fct_db    = FALSE,
    fg_map_data   = fg_map,
    fg_map_food   = "food_item",
    fg_map_col    = "group"
  )

  expected_cols <- c(
    "survey_id",
    paste0("mdd_woman_", c(
      "Grains, White Roots And Tubers And Plantains",
      "Pulses (Beans, Peas And Lentils)",
      "Nuts And Seeds",
      "Milk And Dairy Products",
      "Meat, Poultry And Fish",
      "Eggs",
      "Dark Green Leafy Vegetables",
      "Vitamin A-Rich Fruits And Vegetables",
      "Other Vegetables",
      "Other Fruits",
      "Others"
    )),
    "dds_woman", "mdd_woman"
  ) |> janitor::make_clean_names()

  # Column order is exactly as expected
  expect_identical(names(result), expected_cols)

  # All dummy columns should be numeric
  dummy_cols <- grep("^mdd_woman_", names(result), value = TRUE)
  expect_true(all(sapply(result[dummy_cols], is.numeric)))

  # All values in dummy columns should be 0/1
  for (col in dummy_cols) {
    expect_true(all(result[[col]] %in% c(0, 1)))
  }
})

test_that("compute_mdd enforces correct MDD calculation", {
  recall <- tibble::tibble(
    survey_id = c(1, 2),
    food_item = c("Ugali", "Beans")
  )

  fg_map <- tibble::tibble(
    food_item = c("Ugali", "Beans"),
    group = c(
      "Grains, White Roots And Tubers And Plantains",
      "Pulses (Beans, Peas And Lentils)"
    )
  )

  result <- compute_mdd(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    life_stage    = "woman",
    use_fct_db    = FALSE,
    fg_map_data   = fg_map,
    fg_map_food   = "food_item",
    fg_map_col    = "group"
  )

  # Check formula: mdd_woman = 1 if dds_woman >= 5 else 0
  expect_equal(
    result$mdd_woman,
    ifelse(result$dds_woman >= 5, 1, 0)
  )
})
