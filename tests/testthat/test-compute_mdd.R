# tests/testthat/test-compute_mdd.R

test_that("compute_mdd works for adults with custom mapping (valid categories)", {
  recall <- tibble::tibble(
    survey_id = c(1, 1, 2),
    food = c("Ugali", "Beans", "Rice")
  )

  fg_map <- tibble::tibble(
    item = c("Ugali", "Beans", "Rice"),
    group = c(
      "Grains, White Roots And Tubers And Plantains",
      "Pulses (Beans, Peas And Lentils)",
      "Grains, White Roots And Tubers And Plantains"
    )
  )

  result <- compute_mdd(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food",
    life_stage    = "woman",
    use_fct_db    = FALSE,
    fg_map_data   = fg_map,
    fg_map_food   = "item",
    fg_map_col    = "group"
  )

  expect_true("dds_woman" %in% names(result))
  expect_true("mdd_woman" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_type(result$mdd_woman, "double")
})

test_that("compute_mdd works for child with breastfeeding data", {
  recall <- tibble::tibble(
    survey_id = c(1, 2),
    food_item = c("Ugali", "Beans")
  )

  fg_map <- tibble::tibble(
    food_item = c("Ugali", "Beans"),
    group = c(
      "Grains, White Roots And Tubers And Plantains",
      "Legumes And Nuts"
    )
  )

  bf <- tibble::tibble(
    survey_id = c(1, 2),
    bf        = c(1, 0)
  )

  result <- compute_mdd(
    recall_data = recall,
    id_col = "survey_id",
    food_item_col = "food_item",
    life_stage = "child",
    use_fct_db = FALSE,
    fg_map_data = fg_map,
    fg_map_food = "food_item",
    fg_map_col = "group",
    breastfeeding_data = bf,
    breastfeeding_col = "bf"
  )

  expect_true("dds_child" %in% names(result))
  expect_true("mdd_child" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_type(result$mdd_child, "double")
})

test_that("compute_mdd works for child without breastfeeding data", {
  recall <- tibble::tibble(
    survey_id = c(1, 2),
    food_item = c("Ugali", "Beans")
  )
  fg_map <- tibble::tibble(
    food_item = c("Ugali", "Beans"),
    group = c(
      "Grains, White Roots And Tubers And Plantains",
      "Legumes And Nuts"
    )
  )

  result <- compute_mdd(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    life_stage    = "child",
    use_fct_db    = FALSE,
    fg_map_data   = fg_map,
    fg_map_food   = "food_item",
    fg_map_col    = "group"
  )

  expect_true("dds_child" %in% names(result))
  expect_true("mdd_child" %in% names(result))
})

test_that("compute_mdd warns and returns zeros when no mapped items", {
  recall <- tibble::tibble(
    survey_id = 1:2,
    food_item = c("Unknown Food", "Another Unknown")
  )
  fg_map <- tibble::tibble(
    food_item = "Ugali",
    group     = "Grains, White Roots And Tubers And Plantains"
  )

  expect_warning(
    expect_warning(
      result <- compute_mdd(
        recall_data   = recall,
        id_col        = "survey_id",
        food_item_col = "food_item",
        life_stage    = "woman",
        use_fct_db    = FALSE,
        fg_map_data   = fg_map,
        fg_map_food   = "food_item",
        fg_map_col    = "group"
      ),
      regexp = "Unmapped food items"
    ),
    regexp = "No mapped foods found"
  )

  # Check structure and zeroed scores
  expect_true("dds_woman" %in% names(result))
  expect_true("mdd_woman" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_true(all(result$dds_woman == 0))
  expect_true(all(result$mdd_woman == 0))
})

test_that("compute_mdd errors on invalid inputs", {
  recall <- tibble::tibble(id = 1, food = "Ugali")
  fg_map <- tibble::tibble(item = "Ugali", group = "Grains, White Roots And Tubers And Plantains")

  # Wrong life_stage
  expect_error(
    compute_mdd(recall, "id", "food", "alien",
      use_fct_db = FALSE,
      fg_map_data = fg_map,
      fg_map_food = "item",
      fg_map_col = "group"
    )
  )

  # Missing fg_map_data when required
  expect_error(
    compute_mdd(recall, "id", "food", "woman", use_fct_db = FALSE)
  )

  # Both use_fct_db and fg_map_data
  expect_error(
    compute_mdd(recall, "id", "food", "woman",
      use_fct_db = TRUE,
      fg_map_data = fg_map,
      fg_map_food = "item",
      fg_map_col = "group"
    )
  )

  # Breastfeeding with non-child
  bf <- tibble::tibble(id = 1, bf = 1)
  expect_error(
    compute_mdd(recall, "id", "food", "woman",
      use_fct_db = FALSE,
      fg_map_data = fg_map,
      fg_map_food = "item",
      fg_map_col = "group",
      breastfeeding_data = bf,
      breastfeeding_col = "bf"
    )
  )
})

test_that("compute_mdd works with use_fct_db = TRUE", {
  recall <- tibble::tibble(
    survey_id = 1:4,
    food_item = c("Popcorn", "Stewed Guinea Fowl", "Coffee with Milk", "Egg Toast")
  )

  result <- compute_mdd(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    life_stage    = "woman",
    use_fct_db    = TRUE
  )

  expect_true("dds_woman" %in% names(result))
  expect_true("mdd_woman" %in% names(result))
  expect_equal(nrow(result), 4)
})

test_that("compute_mdd handles skeleton when no mapped categories exist", {
  recall <- tibble::tibble(
    survey_id = 1:2,
    food_item = c("Unknown1", "Unknown2")
  )

  fg_map <- tibble::tibble(
    food_item = "Ugali",
    group     = "Grains, White Roots And Tubers And Plantains"
  )

  expect_warning(
    expect_warning(
      result <- compute_mdd(
        recall_data   = recall,
        id_col        = "survey_id",
        food_item_col = "food_item",
        life_stage    = "child",
        use_fct_db    = FALSE,
        fg_map_data   = fg_map,
        fg_map_food   = "food_item",
        fg_map_col    = "group"
      ),
      regexp = "Unmapped food items"
    ),
    regexp = "No mapped foods found"
  )

  expected_cols <- c(
    "survey_id",
    paste0("mdd_child_", c(
      "Grains, White Roots And Tubers And Plantains",
      "Legumes And Nuts",
      "Milk And Dairy Products",
      "Meat, Poultry And Fish",
      "Eggs",
      "Vitamin A-Rich Fruits And Vegetables",
      "Other Fruits And Vegetables",
      "Others"
    )),
    "dds_child",
    "mdd_child"
  ) |> janitor::make_clean_names()

  expect_identical(names(result), expected_cols)
  expect_true(all(result$dds_child == 0))
  expect_true(all(result$mdd_child == 0))
})

test_that("compute_mdd handles skeleton with breastfeeding data for child", {
  recall <- tibble::tibble(
    survey_id = 1:2,
    food_item = c("Unknown1", "Unknown2")
  )

  fg_map <- tibble::tibble(
    food_item = "Ugali",
    group     = "Grains, White Roots And Tubers And Plantains"
  )

  bf <- tibble::tibble(
    survey_id = 1:2,
    bf        = c(1, 0)
  )


  expect_warning(
    expect_warning(
      result <- compute_mdd(
        recall_data = recall,
        id_col = "survey_id",
        food_item_col = "food_item",
        life_stage = "child",
        use_fct_db = FALSE,
        fg_map_data = fg_map,
        fg_map_food = "food_item",
        fg_map_col = "group",
        breastfeeding_data = bf,
        breastfeeding_col = "bf"
      ),
      regexp = "Unmapped food items"
    ),
    regexp = "No mapped foods found"
  )

  expected_cols <- c(
    "survey_id",
    paste0("mdd_child_", c(
      "Grains, White Roots And Tubers And Plantains",
      "Legumes And Nuts",
      "Milk And Dairy Products",
      "Meat, Poultry And Fish",
      "Eggs",
      "Vitamin A-Rich Fruits And Vegetables",
      "Other Fruits And Vegetables",
      "Others"
    )),
    "bf",
    "dds_child",
    "mdd_child"
  ) |> janitor::make_clean_names()

  expect_identical(names(result), expected_cols)
  # With only breastfeeding, DDS should equal breastfeeding values
  expect_equal(result$dds_child, bf$bf)
  # Since threshold = 5, all MDD must be 0
  expect_true(all(result$mdd_child == 0))
})

test_that("compute_mdd handles multi-group items with semicolon split", {
  recall <- tibble::tibble(
    survey_id = 1,
    food_item = "Mixed Dish"
  )

  fg_map <- tibble::tibble(
    food_item = "Mixed Dish",
    group     = "Grains, White Roots And Tubers And Plantains;Legumes And Nuts;Other Fruits And Vegetables"
  )

  result <- compute_mdd(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    life_stage    = "child",
    use_fct_db    = FALSE,
    fg_map_data   = fg_map,
    fg_map_food   = "food_item",
    fg_map_col    = "group"
  )

  expect_equal(result$dds_child, 3)
  expect_equal(result$mdd_child, 0) # threshold not met
})

test_that("compute_mdd errors on invalid MDD categories", {
  recall <- tibble::tibble(
    survey_id = 1,
    food_item = "FakeFood"
  )
  fg_map <- tibble::tibble(
    food_item = "FakeFood",
    group     = "Invalid Category"
  )

  expect_error(
    compute_mdd(
      recall_data   = recall,
      id_col        = "survey_id",
      food_item_col = "food_item",
      life_stage    = "woman",
      use_fct_db    = FALSE,
      fg_map_data   = fg_map,
      fg_map_food   = "food_item",
      fg_map_col    = "group"
    ),
    regexp = "Invalid MDD categories"
  )
})

test_that("compute_mdd ensures column order, consistency, and valid values", {
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

  expect_identical(names(result), expected_cols)
  expect_true(all(result$mdd_woman %in% c(0, 1)))
})
