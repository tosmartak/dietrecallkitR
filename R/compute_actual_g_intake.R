#' Compute Actual Gram Intake
#'
#' Computes the actual gram intake for foods and ingredients from dietary recall
#' data. Handles both gram-based and non-gram units, with support for user-
#' provided non-gram conversion data (from \code{get_non_gram_foods()}).
#'
#' @param filepath Path to the Excel file containing dietary recall data.
#' @param non_gram_file Optional. Path to a completed non-gram conversion Excel file.
#'   If NULL, the function will check for non-gram units and stop if found.
#' @param maintable_sheet Name of the maintable sheet (default = \code{"maintable"}).
#' @param food_details_sheet Name of the food details sheet (default = \code{"food_details"}).
#' @param food_ingredients_sheet Name of the food ingredients sheet
#'   (default = \code{"food_ingredients_group"}).
#' @param location_col Name of the location column in the maintable
#'   (e.g., subcounty, district). Defaults to \code{"subcounty"}.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{survey_id}{Survey identifier}
#'   \item{food_item}{Name of food or ingredient}
#'   \item{amt_consumed}{Amount reported}
#'   \item{unit}{Unit of measurement}
#'   \item{prop_consumed}{Proportion consumed (default = 1 if missing)}
#'   \item{gram_per_unit}{Conversion factor (grams per unit, from non-gram foods)}
#'   \item{actual_gram_intake}{Final computed intake in grams}
#' }
#'
#' @details
#' - If \code{non_gram_file} is NULL and non-gram units are found, the function
#'   will stop and request a conversion sheet.
#' - If provided, the non-gram conversion file must include the same location column
#'   (specified in \code{location_col}), along with columns
#'   \code{food_item}, \code{unit}, \code{amount}, and \code{gram}.
#' - The function warns the user if any non-gram foods lack a valid
#'   \code{gram_per_unit}.
#'
#' @examples
#' \dontrun{
#' compute_actual_g_intake("dietary_recall_full.xlsx",
#'   non_gram_file = "non_gram_foods_conversion_data.xlsx"
#' )
#' }
#'
#' @export
compute_actual_g_intake <- function(filepath,
                                    non_gram_file = NULL,
                                    maintable_sheet = "maintable",
                                    food_details_sheet = "food_details",
                                    food_ingredients_sheet = "food_ingredients_group",
                                    location_col = "subcounty") {
  stopifnot(file.exists(filepath))

  maintable <- readxl::read_excel(filepath, sheet = maintable_sheet)
  food_details <- readxl::read_excel(filepath, sheet = food_details_sheet)
  food_ingredients <- readxl::read_excel(filepath, sheet = food_ingredients_sheet)

  banned_units <- c("g from scale", "g from photobook")

  # --- Validate location column in maintable ---
  if (!location_col %in% names(maintable)) {
    stop("Column '", location_col, "' not found in maintable.")
  }

  # --- Load conversion file if provided ---
  if (!is.null(non_gram_file)) {
    stopifnot(file.exists(non_gram_file))
    non_gram_foods <- readxl::read_excel(non_gram_file, sheet = 1) %>%
      dplyr::mutate(gram_per_unit = gram / amount)

    # Validate location_col exists in conversion file
    if (!location_col %in% names(non_gram_foods)) {
      stop("Column '", location_col, "' not found in non_gram_file.")
    }

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
  } else {
    non_gram_foods <- NULL
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
  fd_clean <- food_details %>%
    dplyr::filter(!is.na(desc_of_food)) %>%
    dplyr::left_join(
      maintable %>% dplyr::select(survey_id, !!rlang::sym(location_col)),
      by = "survey_id"
    ) %>%
    dplyr::select(
      survey_id,
      !!rlang::sym(location_col),
      food_item = desc_of_food,
      amt_consumed = qty_food_consumed,
      unit = unit_qty_food_consumed,
      prop_consumed = food_item_price_prop_consumed
    )

  if (!is.null(non_gram_foods)) {
    fd_clean <- fd_clean %>%
      dplyr::left_join(
        non_gram_foods %>%
          dplyr::select(!!rlang::sym(location_col), food_item, unit, gram_per_unit),
        by = c(location_col, "food_item", "unit")
      )
  } else {
    fd_clean$gram_per_unit <- NA_real_
  }

  # Check for missing conversions
  missing_conv_fd <- fd_clean %>%
    dplyr::filter(!unit %in% banned_units, is.na(gram_per_unit)) %>%
    dplyr::distinct(food_item, unit)

  if (nrow(missing_conv_fd) > 0) {
    warning(
      "Some non-gram foods in food_details do not have gram_per_unit assigned:\n",
      paste0("- ", missing_conv_fd$food_item, " [", missing_conv_fd$unit, "]", collapse = "\n")
    )
  }

  fd_clean <- fd_clean %>%
    dplyr::mutate(
      prop_consumed = ifelse(is.na(prop_consumed), 1, prop_consumed),
      actual_gram_intake = dplyr::case_when(
        unit %in% banned_units ~ amt_consumed,
        TRUE ~ amt_consumed * gram_per_unit * prop_consumed
      )
    )

  # --- Process food_ingredients_group ---
  fig_clean <- food_ingredients %>%
    dplyr::left_join(
      maintable %>% dplyr::select(survey_id, !!rlang::sym(location_col)),
      by = "survey_id"
    ) %>%
    dplyr::select(
      survey_id,
      food_details_rowid,
      !!rlang::sym(location_col),
      food_item = food_ingredients_used,
      amt_consumed = food_ingredient_amt,
      unit = food_ingredient_unit,
      prop_consumed = food_ingredient_price_prop_used
    ) %>%
    dplyr::left_join(
      food_details %>% dplyr::select(
        survey_id,
        food_details_rowid,
        amt_of_food_cooked,
        qty_food_consumed
      ),
      by = c("survey_id", "food_details_rowid")
    )

  if (!is.null(non_gram_foods)) {
    fig_clean <- fig_clean %>%
      dplyr::left_join(
        non_gram_foods %>%
          dplyr::select(!!rlang::sym(location_col), food_item, unit, gram_per_unit),
        by = c(location_col, "food_item", "unit")
      )
  } else {
    fig_clean$gram_per_unit <- NA_real_
  }

  # Check for missing conversions
  missing_conv_fig <- fig_clean %>%
    dplyr::filter(!unit %in% banned_units, is.na(gram_per_unit)) %>%
    dplyr::distinct(food_item, unit)

  if (nrow(missing_conv_fig) > 0) {
    warning(
      "Some non-gram foods in food_ingredients do not have gram_per_unit assigned:\n",
      paste0("- ", missing_conv_fig$food_item, " [", missing_conv_fig$unit, "]", collapse = "\n")
    )
  }

  fig_clean <- fig_clean %>%
    dplyr::mutate(
      prop_consumed = ifelse(is.na(prop_consumed), 1, prop_consumed),
      gram_intake = dplyr::case_when(
        unit %in% banned_units ~ amt_consumed,
        TRUE ~ amt_consumed * gram_per_unit * prop_consumed
      ),
      actual_gram_intake = (gram_intake * qty_food_consumed) / amt_of_food_cooked
    )

  # --- Combine and return ---
  final <- dplyr::bind_rows(
    fd_clean %>%
      dplyr::select(
        survey_id, food_item, amt_consumed, unit,
        prop_consumed, gram_per_unit, actual_gram_intake
      ),
    fig_clean %>%
      dplyr::select(
        survey_id, food_item, amt_consumed, unit,
        prop_consumed, gram_per_unit, actual_gram_intake
      )
  )

  return(final)
}
