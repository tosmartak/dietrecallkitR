#' Compute Actual Nutrient Intake from 24h Recall Data
#'
#' This function computes actual nutrient intakes at either the food-level or recall-level
#' using reported gram intakes from 24-hour dietary recall data. It multiplies reported
#' intakes by nutrient composition values (per 100g edible portion) from either the built-in
#' food composition database (fct_db) from the `dietracallkitR` repository or a user-supplied nutrient mapping table.
#' Intakes are adjusted for edible conversion factors (ECF) and reported food gram weights.
#'
#' @param recall_data A data frame containing at least an ID column, a recall column,
#'   a food item column, and a numeric gram intake column.
#' @param id_col Character string. Column name in `recall_data` identifying survey
#'   respondents or household IDs.
#' @param recall_col Character string. Column name in `recall_data` identifying the recall occasion.
#' @param food_item_col Character string. Column name in `recall_data` containing reported food items.
#' @param actual_gram_intake_col Character string. Column name in `recall_data` containing
#'   numeric gram intakes (default `"actual_gram_intake"`).
#' @param use_fct_db Logical. If `TRUE`, mapping is automatically pulled from the built-in
#'   food composition database (fct_db). Cannot be used simultaneously with `nutrient_map_data`.
#'   Default is `FALSE`.
#' @param nutrient_map_data Optional. A user-supplied nutrient database (must contain per-100g values). Required if `use_fct_db = FALSE`.
#' @param nutrient_map_food Character string. Column name in `nutrient_map_data` containing food item names.
#' @param ecf_col Character string. Column name in nutrient database for edible conversion factor.
#'   Default `"Edible conversion factor"`.
#' @param nutrient_cols Character vector of nutrient columns (per 100g edible portion)
#'   to calculate actual intakes for. If `use_fct_db = TRUE`,
#'   defaults to a hardcoded set of nutrient columns from the built-in database.
#' @param output_level Character string, either `"recall_level"` (default) or `"food_level"`.
#'   indicating whether the output should be aggregated by recall or left at the food item level for debugging purposes.
#'
#' @details
#' - Assumes nutrient values in the FCT are expressed per 100g edible portion.
#' - Actual intakes are computed for each food item per observation as: \eqn{(`nutrient_value` x `edible_conversion_factor` x `actual_gram_intake`)/100}.
#' - Blank nutrient values are treated as `NA` (missing), not zero. This ensures that missing data
#'   is not confused with true zero intake.
#' - If unmapped food items are found, a warning is issued.
#'
#' @return A tibble with nutrient intakes per recall (default) or per food item (if `output_level = "food_level"`).
#' Columns include:
#' \itemize{
#'   \item Food-level output: nutrient intakes per food item within each recall.
#'   \item Recall-level output: total nutrient intakes per recall.
#' }
#'
#' @examples
#' \dontrun{
#' # Example with built-in fct_db
#' recall <- tibble::tibble(
#'   survey_id = c(1, 1),
#'   recall_id = c(1, 2),
#'   food_item = c("Beans, broad, dry, raw", "Orange (chungwa), pulp, raw"),
#'   actual_gram_intake = c(150, 200)
#' )
#'
#' result <- compute_actual_nutrient_intake(
#'   recall_data   = recall,
#'   id_col        = "survey_id",
#'   recall_col    = "recall_id",
#'   food_item_col = "food_item",
#'   use_fct_db    = TRUE
#' )
#'
#' # Example with custom mapping
#' recall <- tibble::tibble(
#'   id = c(1, 1, 2),
#'   recall_day = c(1, 1, 1),
#'   food = c("Ugali", "Beans", "Soda"),
#'   grams = c(200, 50, 300)
#' )
#'
#' nutrient_map <- tibble::tibble(
#'   item = c("Ugali", "Beans", "Soda"),
#'   ecf = c(1, 1, 1),
#'   Energy.kcal = c(110, 330, 40), # kcal per 100g
#'   Protein.g = c(2, 21, 0), # g per 100g
#'   Fat.g = c(0.5, 1.5, 0) # g per 100g
#' )
#'
#' result <- compute_actual_nutrient_intake(
#'   recall_data = recall,
#'   id_col = "id",
#'   recall_col = "recall_day",
#'   food_item_col = "food",
#'   actual_gram_intake_col = "grams",
#'   use_fct_db = FALSE,
#'   nutrient_map_data = nutrient_map,
#'   nutrient_map_food = "item",
#'   ecf_col = "ecf",
#'   nutrient_cols = c("Energy.kcal", "Protein.g", "Fat.g"),
#'   output_level = "recall_level"
#' )
#' }
#'
#' @export
compute_actual_nutrient_intake <- function(recall_data,
                                           id_col,
                                           recall_col,
                                           food_item_col,
                                           actual_gram_intake_col = "actual_gram_intake",
                                           use_fct_db = FALSE,
                                           nutrient_map_data = NULL,
                                           nutrient_map_food = NULL,
                                           ecf_col = NULL,
                                           nutrient_cols = NULL,
                                           output_level = c("recall_level", "food_level")) {
  output_level <- match.arg(output_level)

  # --- 1) Validate inputs ---
  stopifnot(is.data.frame(recall_data))
  stopifnot(all(c(id_col, recall_col, food_item_col, actual_gram_intake_col) %in% names(recall_data)))

  if (use_fct_db && !is.null(nutrient_map_data)) {
    stop("You cannot set both `use_fct_db = TRUE` and provide `nutrient_map_data`. Choose one.")
  }

  # --- 2) Load mapping ---
  if (use_fct_db) {
    db_url <- "https://raw.githubusercontent.com/tosmartak/dietrecallkitR/main/data-raw/fct_db.csv"
    fct_db <- suppressMessages(readr::read_csv(db_url, show_col_types = FALSE)) |>
      dplyr::mutate(food_item = stringr::str_trim(food_item))

    nutrient_cols <- c(
      "Energy(kJ)", "Energy (kcal)", "Water(g)", "Protein(g)", "Fat(g)",
      "Carbohydrate available (g)", "Fibre(g)", "Ash(g)",
      "Ca(mg)", "Fe(mg)", "Mg(mg)", "P(mg)", "K(mg)", "Na(mg)",
      "Zn(mg)", "Se(mcg)", "Vit A-RAE(mcg)", "Vit A-RE(mcg)",
      "Retinol (mcg)", "b-carotene equivalent (mcg)", "Thiamin (mg)",
      "Riboflavin (mg)", "Niacin(mg)", "Dietary Folate Eq.(mcg)",
      "Food folate (mcg)", "Vit B12 (mcg)", "Vit C(mg)"
    )
    ecf_col <- "Edible conversion factor"
    required_cols <- c("food_item", ecf_col, nutrient_cols)
    nutrient_map <- fct_db |> dplyr::select(dplyr::all_of(required_cols))
  } else {
    if (is.null(nutrient_map_data) || is.null(nutrient_map_food) || is.null(ecf_col) || is.null(nutrient_cols)) {
      stop("When `use_fct_db = FALSE`, you must provide `nutrient_map_data`, `nutrient_map_food`, `ecf_col`, and `nutrient_cols`.")
    }
    stopifnot(is.data.frame(nutrient_map_data))
    stopifnot(all(c(nutrient_map_food, ecf_col, nutrient_cols) %in% names(nutrient_map_data)))

    nutrient_map <- nutrient_map_data |>
      dplyr::select(
        food_item = !!rlang::sym(nutrient_map_food),
        !!rlang::sym(ecf_col),
        dplyr::all_of(nutrient_cols)
      ) |>
      dplyr::mutate(food_item = stringr::str_trim(food_item))
  }

  # --- 3) Aggregate recall data by food ---
  recall_agg <- recall_data |>
    dplyr::group_by(!!rlang::sym(id_col), !!rlang::sym(recall_col), !!rlang::sym(food_item_col)) |>
    dplyr::summarise(actual_gram_intake = sum(!!rlang::sym(actual_gram_intake_col), na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(food_item = stringr::str_trim(!!rlang::sym(food_item_col)))

  # --- 4) Merge recall with nutrient composition ---
  merged <- recall_agg |>
    dplyr::left_join(nutrient_map, by = "food_item")

  # Warn on unmapped foods
  unmapped <- merged |>
    dplyr::filter(is.na(!!rlang::sym(ecf_col))) |>
    dplyr::distinct(food_item)
  if (nrow(unmapped) > 0) {
    warning("Unmapped food items:\n", paste0("- ", unmapped$food_item, collapse = "\n"))
  }

  # --- 5) Compute nutrient intakes ---
  nutrient_intakes <- merged |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(dplyr::all_of(nutrient_cols), ~ {
      if (is.na(.x) || is.na(!!rlang::sym(ecf_col)) || is.na(actual_gram_intake)) {
        NA_real_
      } else {
        (.x * !!rlang::sym(ecf_col) * actual_gram_intake) / 100
      }
    }, .names = "{.col}_intake")) |>
    dplyr::ungroup()

  # --- 6) Output ---
  if (output_level == "food_level") {
    out <- nutrient_intakes |>
      dplyr::select(!!rlang::sym(id_col), !!rlang::sym(recall_col), food_item, actual_gram_intake, dplyr::ends_with("_intake"))
  } else {
    out <- nutrient_intakes |>
      dplyr::group_by(!!rlang::sym(id_col), !!rlang::sym(recall_col)) |>
      dplyr::summarise(
        actual_gram_intake = sum(actual_gram_intake, na.rm = TRUE),
        dplyr::across(dplyr::ends_with("_intake"), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  return(tibble::as_tibble(out))
}
