test_that("compute_dsr works correctly with use_fct_db = TRUE", {
  recall_sample <- tibble::tibble(
    survey_id = c(1, 1, 2, 2, 3),
    desc_of_food = c(
      "Rabbit meat, raw",
      "Ghee (cow milk)",
      "Cheese, cheddar, regular fat",
      "Tuna, grilled",
      "Rabbit meat, raw"
    )
  )

  result <- compute_dsr(
    recall_data   = recall_sample,
    id_col        = "survey_id",
    food_item_col = "desc_of_food",
    use_fct_db    = TRUE
  )

  expect_s3_class(result, "tbl_df")
  expect_true("dsr" %in% names(result))
  expect_equal(nrow(result), 3)
  expect_true(all(result$dsr >= 0))
})

test_that("compute_dsr works correctly with custom mapping", {
  recall <- tibble::tibble(
    id = c(1, 1, 2, 3),
    food = c(
      "Rabbit meat, raw", "Ghee (cow milk)",
      "Cheese, cheddar, regular fat", "Tuna, grilled"
    )
  )

  species_map <- tibble::tibble(
    item = c(
      "Rabbit meat, raw", "Ghee (cow milk)",
      "Cheese, cheddar, regular fat", "Tuna, grilled"
    ),
    species = c("Rabbit", "Cow", "Cow", "Tuna")
  )

  result <- compute_dsr(
    recall_data   = recall,
    id_col        = "id",
    food_item_col = "food",
    use_fct_db    = FALSE,
    dsr_map_data  = species_map,
    dsr_map_food  = "item",
    dsr_map_col   = "species",
    add_prefix    = "child"
  )

  expect_s3_class(result, "tbl_df")
  expect_true("child_dsr" %in% names(result))
  expect_equal(nrow(result), 3)
  expect_true(all(result$child_dsr >= 0))
})

test_that("compute_dsr warns on unmapped foods and returns zero DSR when no species found", {
  recall <- tibble::tibble(
    survey_id = 1:2,
    food_item = c("Unknown food 1", "Unknown food 2")
  )

  species_map <- tibble::tibble(
    item = "Known food",
    species = "Test species"
  )

  expect_warning(
    expect_warning(
      result <- compute_dsr(
        recall_data   = recall,
        id_col        = "survey_id",
        food_item_col = "food_item",
        use_fct_db    = FALSE,
        dsr_map_data  = species_map,
        dsr_map_food  = "item",
        dsr_map_col   = "species"
      ),
      regexp = "Unmapped food items"
    ),
    regexp = "No mapped species found"
  )

  expect_identical(names(result), c("survey_id", "dsr"))
  expect_equal(result$dsr, c(0, 0))
})

test_that("compute_dsr handles semicolon-separated species and counts distinct species correctly", {
  recall <- tibble::tibble(
    survey_id = 1,
    food_item = "Mixed Dish"
  )

  species_map <- tibble::tibble(
    item = "Mixed Dish",
    species = "Rabbit;Cow;Cow;Tuna"
  )

  result <- compute_dsr(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    use_fct_db    = FALSE,
    dsr_map_data  = species_map,
    dsr_map_food  = "item",
    dsr_map_col   = "species"
  )

  expect_equal(result$dsr, 3) # unique species = Rabbit, Cow, Tuna
})

test_that("compute_dsr deduplicates repeated species within same ID", {
  recall <- tibble::tibble(
    survey_id = c(1, 1, 1),
    food_item = c("Rabbit meat, raw", "Rabbit meat, raw", "Rabbit meat, raw")
  )

  species_map <- tibble::tibble(
    item = "Rabbit meat, raw",
    species = "Rabbit"
  )

  result <- compute_dsr(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    use_fct_db    = FALSE,
    dsr_map_data  = species_map,
    dsr_map_food  = "item",
    dsr_map_col   = "species"
  )

  expect_equal(result$dsr, 1)
})

test_that("compute_dsr errors on invalid argument combinations", {
  recall <- tibble::tibble(
    id = 1,
    food = "Rabbit meat, raw"
  )

  species_map <- tibble::tibble(
    item = "Rabbit meat, raw",
    species = "Rabbit"
  )

  # both use_fct_db and dsr_map_data
  expect_error(
    compute_dsr(
      recall_data   = recall,
      id_col        = "id",
      food_item_col = "food",
      use_fct_db    = TRUE,
      dsr_map_data  = species_map,
      dsr_map_food  = "item",
      dsr_map_col   = "species"
    )
  )

  # missing dsr_map_data when required
  expect_error(
    compute_dsr(
      recall_data   = recall,
      id_col        = "id",
      food_item_col = "food",
      use_fct_db    = FALSE
    )
  )

  # wrong id_col
  expect_error(
    compute_dsr(
      recall_data   = recall,
      id_col        = "wrong_id",
      food_item_col = "food",
      use_fct_db    = TRUE
    )
  )

  # wrong food_item_col
  expect_error(
    compute_dsr(
      recall_data   = recall,
      id_col        = "id",
      food_item_col = "wrong_col",
      use_fct_db    = TRUE
    )
  )
})

test_that("compute_dsr handles blank or NA species properly", {
  recall <- tibble::tibble(
    survey_id = c(1, 2, 3),
    food_item = c("Food1", "Food2", "Food3")
  )

  species_map <- tibble::tibble(
    item = c("Food1", "Food2", "Food3"),
    species = c("Rabbit; ; ", NA, "")
  )
  expect_warning(
    result <- compute_dsr(
      recall_data   = recall,
      id_col        = "survey_id",
      food_item_col = "food_item",
      use_fct_db    = FALSE,
      dsr_map_data  = species_map,
      dsr_map_food  = "item",
      dsr_map_col   = "species"
    ),
    regexp = "Unmapped food items"
  )

  expect_equal(result$dsr[result$survey_id == 1], 1)
  expect_equal(result$dsr[result$survey_id == 2], 0)
  expect_equal(result$dsr[result$survey_id == 3], 0)
})

test_that("compute_dsr returns a tibble and numeric output", {
  recall <- tibble::tibble(
    survey_id = c(1, 2),
    food_item = c("Rabbit meat, raw", "Tuna, grilled")
  )

  species_map <- tibble::tibble(
    item = c("Rabbit meat, raw", "Tuna, grilled"),
    species = c("Rabbit", "Tuna")
  )

  result <- compute_dsr(
    recall_data   = recall,
    id_col        = "survey_id",
    food_item_col = "food_item",
    use_fct_db    = FALSE,
    dsr_map_data  = species_map,
    dsr_map_food  = "item",
    dsr_map_col   = "species"
  )

  expect_s3_class(result, "tbl_df")
  expect_true(is.numeric(result$dsr))
  expect_equal(colnames(result), c("survey_id", "dsr"))
})
