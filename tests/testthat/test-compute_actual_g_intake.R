test_that("compute_actual_g_intake returns expected tibble with conversion", {
  skip_if_not_installed("openxlsx")

  subset_ids <- head(dietrecall_example$maintable$survey_id, 3)
  example_small <- list(
    maintable = dplyr::filter(dietrecall_example$maintable, survey_id %in% subset_ids),
    food_details = dplyr::filter(dietrecall_example$food_details, survey_id %in% subset_ids),
    food_ingredients_group = dplyr::filter(dietrecall_example$food_ingredients_group, survey_id %in% subset_ids)
  )

  tmpfile <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(example_small, tmpfile)

  conv_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(non_gram_foods_conversion, conv_file)

  result <- compute_actual_g_intake(tmpfile, non_gram_file = conv_file)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c(
    "survey_id", "food_item", "amt_consumed", "unit",
    "prop_consumed", "gram_per_unit", "actual_gram_intake"
  ) %in% names(result)))
})

test_that("compute_actual_g_intake errors when non-gram units exist but no conversion file is provided", {
  skip_if_not_installed("openxlsx")

  subset_ids <- head(dietrecall_example$maintable$survey_id, 3)
  example_small <- list(
    maintable = dplyr::filter(dietrecall_example$maintable, survey_id %in% subset_ids),
    food_details = dplyr::filter(dietrecall_example$food_details, survey_id %in% subset_ids),
    food_ingredients_group = dplyr::filter(dietrecall_example$food_ingredients_group, survey_id %in% subset_ids)
  )

  tmpfile <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(example_small, tmpfile)

  expect_error(
    compute_actual_g_intake(tmpfile),
    regexp = "Non-gram units found"
  )
})

test_that("compute_actual_g_intake warns when some conversions are missing", {
  skip_if_not_installed("openxlsx")

  subset_ids <- head(dietrecall_example$maintable$survey_id, 3)
  example_small <- list(
    maintable = dplyr::filter(dietrecall_example$maintable, survey_id %in% subset_ids),
    food_details = dplyr::filter(dietrecall_example$food_details, survey_id %in% subset_ids),
    food_ingredients_group = dplyr::filter(dietrecall_example$food_ingredients_group, survey_id %in% subset_ids)
  )

  tmpfile <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(example_small, tmpfile)

  # Break one gram value
  bad_conv <- non_gram_foods_conversion %>%
    dplyr::mutate(gram = ifelse(row_number() == 1, NA, gram))

  conv_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(bad_conv, conv_file)

  expect_warning(
    result <- compute_actual_g_intake(tmpfile, non_gram_file = conv_file),
    regexp = "do not have gram_per_unit assigned"
  )
  expect_s3_class(result, "tbl_df")
})

test_that("compute_actual_g_intake errors on location mismatch between maintable and conversion file", {
  skip_if_not_installed("openxlsx")

  bad_conv <- non_gram_foods_conversion %>%
    dplyr::mutate(subcounty = ifelse(row_number() == 1, "FAKE_SUBCOUNTY", subcounty))

  subset_ids <- head(dietrecall_example$maintable$survey_id, 10)
  example_small <- list(
    maintable = dplyr::filter(dietrecall_example$maintable, survey_id %in% subset_ids),
    food_details = dplyr::filter(dietrecall_example$food_details, survey_id %in% subset_ids),
    food_ingredients_group = dplyr::filter(dietrecall_example$food_ingredients_group, survey_id %in% subset_ids)
  )

  tmpfile <- tempfile(fileext = ".xlsx")
  conv_file <- tempfile(fileext = ".xlsx")

  openxlsx::write.xlsx(example_small, tmpfile)
  openxlsx::write.xlsx(bad_conv, conv_file)

  expect_error(
    compute_actual_g_intake(tmpfile, non_gram_file = conv_file),
    regexp = "Mismatch in .* values"
  )
})

test_that("compute_actual_g_intake fills missing prop_consumed with 1", {
  skip_if_not_installed("openxlsx")

  subset_ids <- head(dietrecall_example$maintable$survey_id, 3)
  fd_mod <- dplyr::filter(dietrecall_example$food_details, survey_id %in% subset_ids) %>%
    dplyr::mutate(food_item_price_prop_consumed = ifelse(row_number() == 1, NA, food_item_price_prop_consumed))

  example_small <- list(
    maintable = dplyr::filter(dietrecall_example$maintable, survey_id %in% subset_ids),
    food_details = fd_mod,
    food_ingredients_group = dplyr::filter(dietrecall_example$food_ingredients_group, survey_id %in% subset_ids)
  )

  tmpfile <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(example_small, tmpfile)

  conv_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(non_gram_foods_conversion, conv_file)

  result <- compute_actual_g_intake(tmpfile, non_gram_file = conv_file)

  expect_true(all(!is.na(result$prop_consumed)))
  expect_true(all(result$prop_consumed >= 0))
})

test_that("compute_actual_g_intake correctly computes non-gram food_ingredients actual_gram_intake", {
  skip_if_not_installed("openxlsx")

  # ---- Minimal maintable ----
  mt <- tibble::tibble(
    survey_id = 1,
    subcounty = "TEST"
  )

  # ---- Parent food_details (in grams, no conversion needed) ----
  fd <- tibble::tibble(
    survey_id                     = 1,
    food_details_rowid            = 1,
    desc_of_food                  = "Ugali",
    qty_food_consumed             = 50, # grams eaten
    amt_of_food_cooked            = 100, # grams cooked
    unit_qty_food_consumed        = "g from scale", # gram-based
    food_item_price_prop_consumed = 1
  )

  # ---- Ingredient (non-gram, needs conversion) ----
  fig <- tibble::tibble(
    survey_id = 1,
    food_details_rowid = 1,
    food_ingredients_used = "Ugali flour",
    food_ingredient_amt = 2, # 2 cups
    food_ingredient_unit = "cup", # non-gram unit
    food_ingredient_price_prop_used = 1
  )

  # ---- Conversion file: 1 cup = 120g ----
  conv <- tibble::tibble(
    subcounty = "TEST",
    food_item = "Ugali flour",
    unit      = "cup",
    amount    = 1,
    gram      = 120
  )

  tmpfile <- tempfile(fileext = ".xlsx")
  conv_file <- tempfile(fileext = ".xlsx")

  openxlsx::write.xlsx(
    list(maintable = mt, food_details = fd, food_ingredients_group = fig),
    tmpfile
  )
  openxlsx::write.xlsx(conv, conv_file)

  # ---- Run function ----
  result <- compute_actual_g_intake(tmpfile, non_gram_file = conv_file)

  # ---- Expected value ----
  # Formula: (amt * gram_per_unit * qty_consumed) / amt_cooked
  expected <- (2 * 120 * 50) / 100

  actual_val <- result %>%
    dplyr::filter(food_item == "Ugali flour") %>%
    dplyr::pull(actual_gram_intake)

  expect_equal(actual_val, expected)
})

test_that("compute_actual_g_intake accepts custom location_col", {
  skip_if_not_installed("openxlsx")

  mt <- dietrecall_example$maintable %>%
    dplyr::rename(district = subcounty)

  conv <- non_gram_foods_conversion %>%
    dplyr::rename(district = subcounty)

  tmpfile <- tempfile(fileext = ".xlsx")
  conv_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      maintable = mt,
      food_details = dietrecall_example$food_details,
      food_ingredients_group = dietrecall_example$food_ingredients_group
    ),
    tmpfile
  )
  openxlsx::write.xlsx(conv, conv_file)

  result <- compute_actual_g_intake(tmpfile, non_gram_file = conv_file, location_col = "district")

  expect_s3_class(result, "tbl_df")
  expect_true(all(result$survey_id %in% mt$survey_id))
})

test_that("compute_actual_g_intake works when all units are gram-based (controlled data)", {
  skip_if_not_installed("openxlsx")

  # Minimal, controlled data (no conversion file needed)
  mt <- tibble::tibble(
    survey_id = c(1, 2),
    subcounty = c("TEST", "TEST")
  )

  fd <- tibble::tibble(
    survey_id                       = c(1, 2),
    food_details_rowid              = c(10, 20),
    desc_of_food                    = c("Rice", "Beans"),
    qty_food_consumed               = c(300, 500), # grams consumed
    amt_of_food_cooked              = c(300, 500), # cooked = consumed (ratio = 1)
    unit_qty_food_consumed          = "g from scale", # gram-based
    food_item_price_prop_consumed   = 1 # neutral scaling
  )

  fig <- tibble::tibble(
    survey_id                       = c(1, 2),
    food_details_rowid              = c(10, 20),
    food_ingredients_used           = c("Rice", "Bean oil"),
    food_ingredient_amt             = c(50, 10), # grams
    food_ingredient_unit            = "g from photobook", # gram-based
    food_ingredient_price_prop_used = 1 # neutral scaling
  )

  tmpfile <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(maintable = mt, food_details = fd, food_ingredients_group = fig),
    tmpfile
  )

  result <- compute_actual_g_intake(tmpfile)

  expect_s3_class(result, "tbl_df")
  expect_true(all(is.na(result$gram_per_unit)))

  # ---- food_details rows: actual == amt_consumed
  fd_rows <- result %>% dplyr::filter(unit == "g from scale")
  expect_equal(fd_rows$actual_gram_intake, fd_rows$amt_consumed)

  # ---- food_ingredients rows: (amt * qty_consumed) / amt_cooked
  fig_rows <- result %>% dplyr::filter(unit == "g from photobook")

  fig_expected <- fig %>%
    dplyr::left_join(
      fd %>% dplyr::select(survey_id, food_details_rowid, qty_food_consumed, amt_of_food_cooked),
      by = c("survey_id", "food_details_rowid")
    ) %>%
    dplyr::mutate(expected_val = (food_ingredient_amt * qty_food_consumed) / amt_of_food_cooked)

  # Order of result fig_rows follows input fig; compare directly
  expect_equal(fig_rows$actual_gram_intake, fig_expected$expected_val)
})
