test_that("compute_actual_nutrient_intake works with use_fct_db = TRUE", {
  recall <- tibble::tibble(
    survey_id = 1:4,
    recall_id = c(1, 1, 1, 1),
    food_item = c(
      "Beans, broad, dry, raw",
      "Orange (chungwa), pulp, raw",
      "Rabbit meat, stewed",
      "Meat Samosa"
    ),
    actual_gram_intake = c(150, 200, 120, 80)
  )

  result <- compute_actual_nutrient_intake(
    recall_data   = recall,
    id_col        = "survey_id",
    recall_col    = "recall_id",
    food_item_col = "food_item",
    use_fct_db    = TRUE,
    output_level  = "recall_level"
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("survey_id", "recall_id", "actual_gram_intake") %in% names(result)))
  expect_true(any(endsWith(names(result), "_intake")))
  expect_equal(nrow(result), 4)
})

test_that("compute_actual_nutrient_intake works with custom nutrient map (recall_level)", {
  recall <- tibble::tibble(
    id = c(1, 1, 2),
    recall_day = c(1, 1, 1),
    food = c("Ugali", "Beans", "Soda"),
    grams = c(200, 50, 300)
  )

  nutrient_map <- tibble::tibble(
    item = c("Ugali", "Beans", "Soda"),
    ecf = c(1, 1, 1),
    Energy.kcal = c(110, 330, 40),
    Protein.g = c(2, 21, 0),
    Fat.g = c(0.5, 1.5, 0)
  )

  result <- compute_actual_nutrient_intake(
    recall_data = recall,
    id_col = "id",
    recall_col = "recall_day",
    food_item_col = "food",
    actual_gram_intake_col = "grams",
    use_fct_db = FALSE,
    nutrient_map_data = nutrient_map,
    nutrient_map_food = "item",
    ecf_col = "ecf",
    nutrient_cols = c("Energy.kcal", "Protein.g", "Fat.g"),
    output_level = "recall_level"
  )

  expect_true(all(c("Energy.kcal_intake", "Protein.g_intake", "Fat.g_intake") %in% names(result)))
  expect_equal(nrow(result), 2)
})

test_that("compute_actual_nutrient_intake works with custom nutrient map (food_level)", {
  recall <- tibble::tibble(
    id = c(1, 1),
    recall_day = c(1, 1),
    food = c("Beans", "Ugali"),
    grams = c(100, 150)
  )

  nutrient_map <- tibble::tibble(
    item = c("Beans", "Ugali"),
    ecf = c(1, 0.9),
    Energy.kcal = c(330, 110),
    Protein.g = c(21, 2)
  )

  result <- compute_actual_nutrient_intake(
    recall_data = recall,
    id_col = "id",
    recall_col = "recall_day",
    food_item_col = "food",
    actual_gram_intake_col = "grams",
    use_fct_db = FALSE,
    nutrient_map_data = nutrient_map,
    nutrient_map_food = "item",
    ecf_col = "ecf",
    nutrient_cols = c("Energy.kcal", "Protein.g"),
    output_level = "food_level"
  )

  expect_true("food" %in% names(recall))
  expect_true(all(endsWith(names(result)[-(1:4)], "_intake")))
  expect_equal(nrow(result), 2)
})

test_that("compute_actual_nutrient_intake correctly computes numeric nutrient intakes", {
  recall <- tibble::tibble(
    id = 1,
    recall_day = 1,
    food = "Beans",
    grams = 100
  )

  nutrient_map <- tibble::tibble(
    item = "Beans",
    ecf = 1,
    Energy.kcal = 330,
    Protein.g = 21
  )

  result <- compute_actual_nutrient_intake(
    recall_data = recall,
    id_col = "id",
    recall_col = "recall_day",
    food_item_col = "food",
    actual_gram_intake_col = "grams",
    use_fct_db = FALSE,
    nutrient_map_data = nutrient_map,
    nutrient_map_food = "item",
    ecf_col = "ecf",
    nutrient_cols = c("Energy.kcal", "Protein.g"),
    output_level = "food_level"
  )

  # Energy = 330 * 1 * 100 / 100 = 330
  expect_equal(result$`Energy.kcal_intake`, 330)
  # Protein = 21 * 1 * 100 / 100 = 21
  expect_equal(result$`Protein.g_intake`, 21)
})

test_that("compute_actual_nutrient_intake warns for unmapped food items", {
  recall <- tibble::tibble(
    id = 1,
    recall_day = 1,
    food = "Unknown Food",
    grams = 100
  )

  nutrient_map <- tibble::tibble(
    item = "Beans",
    ecf = 1,
    Energy.kcal = 330
  )

  expect_warning(
    compute_actual_nutrient_intake(
      recall_data = recall,
      id_col = "id",
      recall_col = "recall_day",
      food_item_col = "food",
      actual_gram_intake_col = "grams",
      use_fct_db = FALSE,
      nutrient_map_data = nutrient_map,
      nutrient_map_food = "item",
      ecf_col = "ecf",
      nutrient_cols = "Energy.kcal"
    ),
    regexp = "Unmapped food items"
  )
})

test_that("compute_actual_nutrient_intake handles NA nutrient or ECF values correctly", {
  recall <- tibble::tibble(
    id = 1,
    recall_day = 1,
    food = "Beans",
    grams = 100
  )

  nutrient_map <- tibble::tibble(
    item = "Beans",
    ecf = NA_real_,
    Energy.kcal = 330
  )

  result <- suppressWarnings(
    compute_actual_nutrient_intake(
      recall_data = recall,
      id_col = "id",
      recall_col = "recall_day",
      food_item_col = "food",
      actual_gram_intake_col = "grams",
      use_fct_db = FALSE,
      nutrient_map_data = nutrient_map,
      nutrient_map_food = "item",
      ecf_col = "ecf",
      nutrient_cols = "Energy.kcal",
      output_level = "food_level"
    )
  )

  expect_true(is.na(result$Energy.kcal_intake))
})

test_that("compute_actual_nutrient_intake errors on invalid input combinations", {
  recall <- tibble::tibble(
    id = 1, recall_day = 1, food = "Beans", grams = 100
  )

  nutrient_map <- tibble::tibble(
    item = "Beans", ecf = 1, Energy.kcal = 330
  )

  # both use_fct_db and nutrient_map_data
  expect_error(
    compute_actual_nutrient_intake(
      recall_data = recall,
      id_col = "id",
      recall_col = "recall_day",
      food_item_col = "food",
      actual_gram_intake_col = "grams",
      use_fct_db = TRUE,
      nutrient_map_data = nutrient_map,
      nutrient_map_food = "item",
      ecf_col = "ecf",
      nutrient_cols = "Energy.kcal",
      output_level = "recall_level"
    ),
    regexp = "You cannot set both"
  )

  # missing nutrient_map_data when required
  expect_error(
    compute_actual_nutrient_intake(
      recall_data = recall,
      id_col = "id",
      recall_col = "recall_day",
      food_item_col = "food",
      actual_gram_intake_col = "grams",
      use_fct_db = FALSE
    ),
    regexp = "you must provide"
  )

  # wrong id_col
  expect_error(
    compute_actual_nutrient_intake(
      recall_data = recall,
      id_col = "wrong_id",
      recall_col = "recall_day",
      food_item_col = "food",
      actual_gram_intake_col = "grams",
      use_fct_db = TRUE
    )
  )

  # wrong food_item_col
  expect_error(
    compute_actual_nutrient_intake(
      recall_data = recall,
      id_col = "id",
      recall_col = "recall_day",
      food_item_col = "wrong_col",
      actual_gram_intake_col = "grams",
      use_fct_db = TRUE
    )
  )
})

test_that("compute_actual_nutrient_intake returns correct aggregation by recall", {
  recall <- tibble::tibble(
    id = c(1, 1),
    recall_day = c(1, 1),
    food = c("Beans", "Ugali"),
    grams = c(100, 200)
  )

  nutrient_map <- tibble::tibble(
    item = c("Beans", "Ugali"),
    ecf = c(1, 1),
    Energy.kcal = c(330, 110)
  )

  result <- compute_actual_nutrient_intake(
    recall_data = recall,
    id_col = "id",
    recall_col = "recall_day",
    food_item_col = "food",
    actual_gram_intake_col = "grams",
    use_fct_db = FALSE,
    nutrient_map_data = nutrient_map,
    nutrient_map_food = "item",
    ecf_col = "ecf",
    nutrient_cols = "Energy.kcal",
    output_level = "recall_level"
  )

  # (Beans 330*1*100/100) + (Ugali 110*1*200/100) = 330 + 220 = 550
  expect_equal(result$Energy.kcal_intake, 550)
})

test_that("compute_actual_nutrient_intake output column types are consistent and numeric", {
  recall <- tibble::tibble(
    id = 1, recall_day = 1, food = "Beans", grams = 100
  )

  nutrient_map <- tibble::tibble(
    item = "Beans", ecf = 1, Energy.kcal = 330, Protein.g = 21
  )

  result <- compute_actual_nutrient_intake(
    recall_data = recall,
    id_col = "id",
    recall_col = "recall_day",
    food_item_col = "food",
    actual_gram_intake_col = "grams",
    use_fct_db = FALSE,
    nutrient_map_data = nutrient_map,
    nutrient_map_food = "item",
    ecf_col = "ecf",
    nutrient_cols = c("Energy.kcal", "Protein.g"),
    output_level = "food_level"
  )

  num_cols <- grep("_intake$", names(result), value = TRUE)
  expect_true(all(sapply(result[num_cols], is.numeric)))
})
