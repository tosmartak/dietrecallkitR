test_that("compute_mdd works for adults with custom mapping", {
  recall <- tibble::tibble(
    survey_id = c(1, 1, 2),
    food = c("Ugali", "Beans", "Rice")
  )

  fg_map <- tibble::tibble(
    item  = c("Ugali", "Beans", "Rice"),
    group = c("Starch", "Legume", "Starch")
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
    group     = c("Starch", "Legume")
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
})

test_that("compute_mdd works for child without breastfeeding data", {
  recall <- tibble::tibble(
    survey_id = c(1, 2),
    food_item = c("Ugali", "Beans")
  )
  fg_map <- tibble::tibble(
    food_item = c("Ugali", "Beans"),
    group     = c("Starch", "Legume")
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

test_that("compute_mdd warns for unmapped items", {
  recall <- tibble::tibble(
    survey_id = 1,
    food_item = "Unknown Food"
  )
  fg_map <- tibble::tibble(
    food_item = "Ugali",
    group     = "Starch"
  )

  expect_warning(
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
    regexp = "Unmapped food items"
  )
})

test_that("compute_mdd errors on invalid inputs", {
  recall <- tibble::tibble(id = 1, food = "Ugali")
  fg_map <- tibble::tibble(item = "Ugali", group = "Starch")

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

test_that("compute_mdd handles empty recall gracefully", {
  recall <- tibble::tibble(survey_id = integer(), food_item = character())
  fg_map <- tibble::tibble(food_item = "Ugali", group = "Starch")

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

  expect_equal(nrow(result), 0)
  expect_true(all(c("dds_woman", "mdd_woman") %in% names(result)))
})

test_that("compute_mdd handles multi-group items with semicolon split", {
  recall <- tibble::tibble(
    survey_id = 1,
    food_item = "Mixed Dish"
  )

  fg_map <- tibble::tibble(
    food_item = "Mixed Dish",
    group     = "Starch;Legume;Vegetable"
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

  # Check that multiple one-hot columns exist
  multi_cols <- grep("^mdd_woman_", names(result), value = TRUE)
  expect_true(any(grepl("Starch", multi_cols)))
  expect_true(any(grepl("Legume", multi_cols)))
  expect_true(any(grepl("Vegetable", multi_cols)))

  # DDS should equal 3 (since three groups consumed)
  expect_equal(result$dds_woman, 3)
  # MDD should be 0 (not enough groups to meet threshold of 5)
  expect_equal(result$mdd_woman, 0)
})
