#' Compute Actual Gram Intake
#'
#' Computes the actual gram intake for foods and ingredients from dietary recall
#' data. Handles both gram-based and non-gram units, with support for user-
#' provided non-gram conversion data (from [get_non_gram_foods()]).
#'
#' @param maintable A data frame with survey-level information (must include
#'   the column specified in `key` and the specified `location_col`).
#' @param food_details A data frame with food-level information (must include
#'   the column specified in `key`, plus `desc_of_food`, `qty_food_consumed`,
#'   `unit_qty_food_consumed`, and `food_item_price_prop_consumed`).
#' @param food_ingredients A data frame with ingredient-level information (must include
#'   the column specified in `key`, `food_details_rowid`, `food_ingredients_used`,
#'   `food_ingredient_amt`, and `food_ingredient_unit`).
#' @param non_gram_foods Optional. A data frame produced from
#'   [get_non_gram_foods()] and filled by the user with conversions
#'   (must include `food_item`, `unit`, `amount`, and `gram`).
#'   If `NULL`, the function will check for non-gram units and stop if found.
#' @param location_col Character string. Name of the location column in `maintable`
#'   (e.g., `"subcounty"`, `"district"`).
#' @param key Character string. The column name that uniquely links
#'   `maintable`, `food_details`, and `food_ingredients`.
#'   Defaults to `"survey_id"`.
#' @param group Logical, default = `TRUE`. If `TRUE`, results are aggregated
#'   by `key` and `food_item`, returning summed `actual_gram_intake` only.
#'   If `FALSE`, detailed row-level data (with intermediate columns) is returned.
#'
#' @return A tibble:
#' - If `group = TRUE`: columns `key` - Survey identifier, `food_item` - Name of food or ingredient, `actual_gram_intake` - Final computed intake in grams
#' - If `group = FALSE`: detailed columns including `amt_consumed` - Amount reported, `unit` - Unit of measurement,
#'   `prop_consumed` - Proportion consumed `(default = 1 if missing)`, `gram_per_unit` - Conversion factor `(grams per unit, from non-gram foods)`,
#'   and `actual_gram_intake` - Final computed intake in grams
#'
#' @details
#' - If `non_gram_foods` is `NULL` and non-gram units are found, the function
#'   will stop and request a conversion sheet.
#' - If provided, `non_gram_foods` must include the same location column
#'   (specified in `location_col`), along with columns
#'   `food_item`, `unit`, `amount`, and `gram`.
#' - The function warns the user if any non-gram foods lack a valid
#'   `gram_per_unit`.
#'
#' @examples
#' data("dietrecall_example")
#' data("non_gram_foods_conversion")
#'
#' # Default: grouped output, ready for FCT merge
#' result_grouped <- compute_actual_g_intake(
#'   maintable = dietrecall_example$maintable,
#'   food_details = dietrecall_example$food_details,
#'   food_ingredients = dietrecall_example$food_ingredients_group,
#'   non_gram_foods = non_gram_foods_conversion,
#'   location_col = "subcounty",
#'   key = "survey_id",
#'   group = FALSE
#' )
#'
#' head(result_grouped)
#'
#' # Detailed output (row-level), useful for debugging
#' result_detailed <- compute_actual_g_intake(
#'   maintable = dietrecall_example$maintable,
#'   food_details = dietrecall_example$food_details,
#'   food_ingredients = dietrecall_example$food_ingredients_group,
#'   non_gram_foods = non_gram_foods_conversion,
#'   location_col = "subcounty",
#'   key = "survey_id"
#' )
#'
#' head(result_detailed)
#'
#' @export
compute_actual_g_intake <- function(maintable,
                                    food_details,
                                    food_ingredients,
                                    non_gram_foods = NULL,
                                    location_col,
                                    key = "survey_id",
                                    group = TRUE) {
  stopifnot(is.data.frame(maintable), is.data.frame(food_details), is.data.frame(food_ingredients))
  stopifnot(location_col %in% names(maintable))
  stopifnot(key %in% names(maintable), key %in% names(food_details), key %in% names(food_ingredients))

  banned_units <- c("g from scale", "g from photobook")

  # --- Trim whitespace early ---
  food_details <- food_details |>
    dplyr::mutate(desc_of_food = stringr::str_trim(desc_of_food))

  food_ingredients <- food_ingredients |>
    dplyr::mutate(food_ingredients_used = stringr::str_trim(food_ingredients_used))

  if (!is.null(non_gram_foods)) {
    non_gram_foods <- non_gram_foods |>
      dplyr::mutate(food_item = stringr::str_trim(food_item))
  }

  # Helper to validate positive columns
  validate_positive <- function(df, col) {
    bad <- df[is.na(df[[col]]) | df[[col]] <= 0, c("food_item", "unit", col)]
    if (nrow(bad) > 0) {
      stop(
        "Invalid values in '", col, "' column of conversion file:\n",
        paste0("- ", bad$food_item, " [", bad$unit, "] = ", bad[[col]], collapse = "\n"),
        "\nAll values must be > 0 and not NA."
      )
    }
  }

  # --- Load and validate non_gram_foods ---
  if (!is.null(non_gram_foods)) {
    stopifnot(is.data.frame(non_gram_foods))
    stopifnot(location_col %in% names(non_gram_foods))
    stopifnot(all(c("food_item", "unit", "amount", "gram") %in% names(non_gram_foods)))

    validate_positive(non_gram_foods, "amount")
    validate_positive(non_gram_foods, "gram")

    non_gram_foods <- non_gram_foods |>
      dplyr::mutate(gram_per_unit = gram / amount)

    # Validate consistency of location values
    loc_main <- sort(unique(maintable[[location_col]]))
    loc_non <- sort(unique(non_gram_foods[[location_col]]))
    if (!identical(loc_main, loc_non)) {
      stop(
        "Mismatch in location values between maintable and non_gram_foods.\n",
        "Maintable: ", paste(loc_main, collapse = ", "), "\n",
        "Non-gram foods: ", paste(loc_non, collapse = ", ")
      )
    }
  }

  # --- Check for non-gram units if no file provided ---
  unique_units <- unique(c(
    food_details$unit_qty_food_consumed,
    food_ingredients$food_ingredient_unit
  ))
  other_units <- setdiff(unique_units, banned_units)

  if (is.null(non_gram_foods) && length(other_units) > 0) {
    stop(
      "Non-gram units found: ", paste(other_units, collapse = ", "),
      ". Please provide the non-gram foods conversion sheet."
    )
  }

  # --- Process food_details ---
  fd_clean <- food_details |>
    dplyr::filter(!is.na(desc_of_food)) |>
    dplyr::left_join(
      maintable |> dplyr::select(!!rlang::sym(key), !!rlang::sym(location_col)),
      by = key
    ) |>
    dplyr::select(
      !!rlang::sym(key),
      !!rlang::sym(location_col),
      food_item = desc_of_food,
      amt_consumed = qty_food_consumed,
      unit = unit_qty_food_consumed,
      prop_consumed = food_item_price_prop_consumed
    )

  if (!is.null(non_gram_foods)) {
    fd_clean <- fd_clean |>
      dplyr::left_join(
        non_gram_foods |>
          dplyr::select(!!rlang::sym(location_col), food_item, unit, gram_per_unit),
        by = c(location_col, "food_item", "unit")
      )
  } else {
    fd_clean$gram_per_unit <- NA_real_
  }

  # Warn if missing conversions
  missing_conv_fd <- fd_clean |>
    dplyr::filter(!unit %in% banned_units, is.na(gram_per_unit)) |>
    dplyr::distinct(food_item, unit)

  if (nrow(missing_conv_fd) > 0) {
    warning(
      "Some non-gram foods in food_details do not have gram_per_unit assigned:\n",
      paste0("- ", missing_conv_fd$food_item, " [", missing_conv_fd$unit, "]", collapse = "\n")
    )
  }

  fd_clean <- fd_clean |>
    dplyr::mutate(
      prop_consumed = ifelse(is.na(prop_consumed), 1, prop_consumed),
      actual_gram_intake = dplyr::case_when(
        unit %in% banned_units ~ amt_consumed,
        TRUE ~ amt_consumed * gram_per_unit * prop_consumed
      )
    )

  # --- Process food_ingredients_group ---
  fig_clean <- food_ingredients |>
    dplyr::left_join(
      maintable |> dplyr::select(!!rlang::sym(key), !!rlang::sym(location_col)),
      by = key
    ) |>
    dplyr::select(
      !!rlang::sym(key),
      food_details_rowid,
      !!rlang::sym(location_col),
      food_item = food_ingredients_used,
      amt_consumed = food_ingredient_amt,
      unit = food_ingredient_unit,
      prop_consumed = food_ingredient_price_prop_used
    ) |>
    dplyr::left_join(
      food_details |>
        dplyr::select(
          !!rlang::sym(key),
          food_details_rowid,
          amt_of_food_cooked,
          qty_food_consumed
        ),
      by = c(key, "food_details_rowid")
    )

  if (!is.null(non_gram_foods)) {
    fig_clean <- fig_clean |>
      dplyr::left_join(
        non_gram_foods |>
          dplyr::select(!!rlang::sym(location_col), food_item, unit, gram_per_unit),
        by = c(location_col, "food_item", "unit")
      )
  } else {
    fig_clean$gram_per_unit <- NA_real_
  }

  # Warn if missing conversions
  missing_conv_fig <- fig_clean |>
    dplyr::filter(!unit %in% banned_units, is.na(gram_per_unit)) |>
    dplyr::distinct(food_item, unit)

  if (nrow(missing_conv_fig) > 0) {
    warning(
      "Some non-gram foods in food_ingredients do not have gram_per_unit assigned:\n",
      paste0("- ", missing_conv_fig$food_item, " [", missing_conv_fig$unit, "]", collapse = "\n")
    )
  }

  fig_clean <- fig_clean |>
    dplyr::mutate(
      prop_consumed = ifelse(is.na(prop_consumed), 1, prop_consumed),
      gram_intake = dplyr::case_when(
        unit %in% banned_units ~ amt_consumed,
        TRUE ~ amt_consumed * gram_per_unit * prop_consumed
      ),
      actual_gram_intake = (gram_intake * qty_food_consumed) / amt_of_food_cooked
    ) |>
    dplyr::select(-gram_intake)

  # --- Combine ---
  final <- dplyr::bind_rows(
    fd_clean |>
      dplyr::select(
        !!rlang::sym(key), food_item, amt_consumed, unit,
        prop_consumed, gram_per_unit, actual_gram_intake
      ),
    fig_clean |>
      dplyr::select(
        !!rlang::sym(key), food_item, amt_consumed, unit,
        prop_consumed, gram_per_unit, actual_gram_intake
      )
  )

  # --- Group if requested ---
  if (group) {
    final <- final |>
      dplyr::group_by(!!rlang::sym(key), food_item) |>
      dplyr::summarise(
        actual_gram_intake = sum(actual_gram_intake, na.rm = TRUE),
        .groups = "drop"
      )
  }

  return(tibble::as_tibble(final))
}
