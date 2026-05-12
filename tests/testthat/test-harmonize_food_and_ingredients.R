# tests/testthat/test-harmonize_food_and_ingredients.R

test_that("harmonize_food_and_ingredients returns expected tibble by default", {
  result <- harmonize_food_and_ingredients(
    food_details = dietrecall_example$food_details,
    food_ingredients = dietrecall_example$food_ingredients_group,
    key = "survey_id"
  )

  expect_true(tibble::is_tibble(result))
  expect_equal(names(result), c("survey_id", "food_item"))
  expect_false("food_details_rowid" %in% names(result))
})

test_that("harmonize_food_and_ingredients includes food_details_rowid when requested", {
  result <- harmonize_food_and_ingredients(
    food_details = dietrecall_example$food_details,
    food_ingredients = dietrecall_example$food_ingredients_group,
    key = "survey_id",
    include_rowid = TRUE
  )

  expect_true(tibble::is_tibble(result))
  expect_true(all(c("survey_id", "food_details_rowid", "food_item") %in% names(result)))
  expect_equal(names(result), c("survey_id", "food_details_rowid", "food_item"))
})

test_that("harmonize_food_and_ingredients excludes NA desc_of_food", {
  fd_mod <- dietrecall_example$food_details %>%
    dplyr::mutate(desc_of_food = ifelse(dplyr::row_number() == 1, NA, desc_of_food))

  result <- harmonize_food_and_ingredients(
    food_details = fd_mod,
    food_ingredients = dietrecall_example$food_ingredients_group,
    key = "survey_id"
  )

  expect_false(any(is.na(result$food_item)))
})

test_that("harmonize_food_and_ingredients errors on missing key", {
  fd_mod <- dietrecall_example$food_details %>% dplyr::select(-survey_id)
  fig_mod <- dietrecall_example$food_ingredients_group

  expect_error(
    harmonize_food_and_ingredients(
      food_details = fd_mod,
      food_ingredients = fig_mod,
      key = "survey_id"
    )
  )
})

test_that("harmonize_food_and_ingredients works with custom key", {
  fd <- dietrecall_example$food_details %>%
    dplyr::rename(id = survey_id)
  fig <- dietrecall_example$food_ingredients_group %>%
    dplyr::rename(id = survey_id)

  result <- harmonize_food_and_ingredients(
    food_details = fd,
    food_ingredients = fig,
    key = "id"
  )

  expect_true(all(c("id", "food_item") %in% names(result)))
})

test_that("harmonize_food_and_ingredients returns empty tibble when inputs are empty", {
  fd <- tibble::tibble(survey_id = integer(), desc_of_food = character())
  fig <- tibble::tibble(survey_id = integer(), food_ingredients_used = character())

  result <- harmonize_food_and_ingredients(
    food_details = fd,
    food_ingredients = fig,
    key = "survey_id"
  )

  expect_true(tibble::is_tibble(result))
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("survey_id", "food_item"))
})

test_that("harmonize_food_and_ingredients works when only one source is available", {
  fd <- dietrecall_example$food_details
  fig_empty <- tibble::tibble(survey_id = integer(), food_ingredients_used = character())

  result_fd <- harmonize_food_and_ingredients(
    food_details = fd,
    food_ingredients = fig_empty,
    key = "survey_id"
  )
  expect_gt(nrow(result_fd), 0)

  fd_empty <- tibble::tibble(survey_id = integer(), desc_of_food = character())
  result_fig <- harmonize_food_and_ingredients(
    food_details = fd_empty,
    food_ingredients = dietrecall_example$food_ingredients_group,
    key = "survey_id"
  )
  expect_gt(nrow(result_fig), 0)
})

test_that("harmonize_food_and_ingredients preserves duplicates", {
  fd <- tibble::tibble(
    survey_id = c(1, 1),
    desc_of_food = c("Ugali", "Ugali")
  )
  fig <- tibble::tibble(
    survey_id = 1,
    food_ingredients_used = "Ugali"
  )

  result <- harmonize_food_and_ingredients(
    food_details = fd,
    food_ingredients = fig,
    key = "survey_id"
  )

  expect_equal(sum(result$food_item == "Ugali"), 3)
})

test_that("harmonize_food_and_ingredients trims whitespace in food_item", {
  fd <- tibble::tibble(
    survey_id = 1,
    desc_of_food = "  Ugali  "
  )
  fig <- tibble::tibble(
    survey_id = 1,
    food_ingredients_used = " Sukuma Wiki "
  )

  result <- harmonize_food_and_ingredients(
    food_details = fd,
    food_ingredients = fig,
    key = "survey_id"
  )

  expect_false(any(grepl("^\\s|\\s$", result$food_item)))
  expect_true(all(result$food_item %in% c("Ugali", "Sukuma Wiki")))
})

test_that("harmonize_food_and_ingredients preserves food_details_rowid from foods and ingredients", {
  fd <- tibble::tibble(
    survey_id = c(1, 1),
    food_details_rowid = c(10, 20),
    desc_of_food = c("Ugali", "Rice")
  )

  fig <- tibble::tibble(
    survey_id = c(1, 1),
    food_details_rowid = c(10, 20),
    food_ingredients_used = c("Maize flour", "Cooking oil")
  )

  result <- harmonize_food_and_ingredients(
    food_details = fd,
    food_ingredients = fig,
    key = "survey_id",
    include_rowid = TRUE
  )

  expect_equal(nrow(result), 4)
  expect_true(all(result$food_details_rowid %in% c(10, 20)))

  expect_equal(
    result$food_details_rowid[result$food_item == "Ugali"],
    10
  )

  expect_equal(
    result$food_details_rowid[result$food_item == "Maize flour"],
    10
  )

  expect_equal(
    result$food_details_rowid[result$food_item == "Rice"],
    20
  )

  expect_equal(
    result$food_details_rowid[result$food_item == "Cooking oil"],
    20
  )
})

test_that("harmonize_food_and_ingredients works with custom key and include_rowid = TRUE", {
  fd <- dietrecall_example$food_details %>%
    dplyr::rename(id = survey_id)

  fig <- dietrecall_example$food_ingredients_group %>%
    dplyr::rename(id = survey_id)

  result <- harmonize_food_and_ingredients(
    food_details = fd,
    food_ingredients = fig,
    key = "id",
    include_rowid = TRUE
  )

  expect_equal(names(result), c("id", "food_details_rowid", "food_item"))
  expect_true(all(result$id %in% fd$id | result$id %in% fig$id))
})

test_that("harmonize_food_and_ingredients errors when include_rowid = TRUE but rowid is missing", {
  fd <- dietrecall_example$food_details %>%
    dplyr::select(-food_details_rowid)

  fig <- dietrecall_example$food_ingredients_group

  expect_error(
    harmonize_food_and_ingredients(
      food_details = fd,
      food_ingredients = fig,
      key = "survey_id",
      include_rowid = TRUE
    )
  )

  fd <- dietrecall_example$food_details

  fig <- dietrecall_example$food_ingredients_group %>%
    dplyr::select(-food_details_rowid)

  expect_error(
    harmonize_food_and_ingredients(
      food_details = fd,
      food_ingredients = fig,
      key = "survey_id",
      include_rowid = TRUE
    )
  )
})

test_that("harmonize_food_and_ingredients returns empty tibble with rowid when include_rowid = TRUE", {
  fd <- tibble::tibble(
    survey_id = integer(),
    food_details_rowid = integer(),
    desc_of_food = character()
  )

  fig <- tibble::tibble(
    survey_id = integer(),
    food_details_rowid = integer(),
    food_ingredients_used = character()
  )

  result <- harmonize_food_and_ingredients(
    food_details = fd,
    food_ingredients = fig,
    key = "survey_id",
    include_rowid = TRUE
  )

  expect_true(tibble::is_tibble(result))
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("survey_id", "food_details_rowid", "food_item"))
})
