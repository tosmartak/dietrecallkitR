#' Get Non-Gram Foods
#'
#' Extracts unique food items from dietary recall data where units are not
#' measured in grams ("g from scale" or "g from photobook"). Combines results from
#' `food_details` and `food_ingredients_group` and links them with
#' household location information from `maintable`.
#'
#' Optionally, the results can be exported to Excel. The exported file contains
#' all relevant columns, with two extra empty columns (`amount`, `gram`)
#' for user input. Users are expected to fill in only these two columns and
#' should avoid modifying the location column, `food_item`, or `unit`.
#'
#' @param maintable A data frame with survey-level information (must include
#'   `key` and the specified `location_col`).
#' @param food_details A data frame with food details (must include
#'   `key`, `desc_of_food`, and `unit_qty_food_consumed`).
#' @param food_ingredients A data frame with food ingredients (must include
#'   `key`, `food_ingredients_used`, and `food_ingredient_unit`).
#' @param location_col Character string. Name of the location column in `maintable`
#'   (e.g., `subcounty`, `district`). Must be provided explicitly.
#' @param key Character string. The column name that uniquely links
#'   `maintable`, `food_details`, and `food_ingredients`.
#'   Typically `survey_id`.
#' @param export_path Optional. File path to export results as an Excel file.
#'   If supplied, the Excel file will be created with all required columns and
#'   empty `amount` and `gram` fields for later user input.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{`location_col`}{Location identifier column, as specified in maintable}
#'   \item{food_item}{Food item consumed or ingredient used}
#'   \item{unit}{Original unit recorded (not grams)}
#'   \item{amount}{Empty column for later input}
#'   \item{gram}{Empty column for later input}
#' }
#'
#' @details
#' If all food items in the dataset are recorded in grams
#' ("g from scale" or "g from photobook"), the function will return
#' an empty tibble with the correct columns, print a message to the user,
#' and skip Excel export (even if `export_path` is provided).
#'
#' @examples
#' data("dietrecall_example")
#' get_non_gram_foods(
#'   maintable = dietrecall_example$maintable,
#'   food_details = dietrecall_example$food_details,
#'   food_ingredients = dietrecall_example$food_ingredients_group,
#'   location_col = "subcounty",
#'   key = "survey_id"
#' )
#'
#' @export
get_non_gram_foods <- function(maintable,
                               food_details,
                               food_ingredients,
                               location_col,
                               key = "survey_id",
                               export_path = NULL) {
  stopifnot(is.data.frame(maintable), is.data.frame(food_details), is.data.frame(food_ingredients))
  stopifnot(location_col %in% names(maintable))
  stopifnot(key %in% names(maintable))
  stopifnot(key %in% names(food_details))
  stopifnot(key %in% names(food_ingredients))

  banned_units <- c("g from scale", "g from photobook")

  # ---- food_details ----
  df1 <- food_details |>
    dplyr::filter(
      !is.na(desc_of_food),
      !unit_qty_food_consumed %in% banned_units
    ) |>
    dplyr::left_join(
      maintable |> dplyr::select(!!rlang::sym(key), !!rlang::sym(location_col)),
      by = key
    ) |>
    dplyr::select(
      !!rlang::sym(location_col),
      food_item = desc_of_food,
      unit = unit_qty_food_consumed
    )

  # ---- food_ingredients ----
  df2 <- food_ingredients |>
    dplyr::filter(!food_ingredient_unit %in% banned_units) |>
    dplyr::left_join(
      maintable |> dplyr::select(!!rlang::sym(key), !!rlang::sym(location_col)),
      by = key
    ) |>
    dplyr::select(
      !!rlang::sym(location_col),
      food_item = food_ingredients_used,
      unit = food_ingredient_unit
    )

  # ---- Combine + clean ----
  combined <- dplyr::bind_rows(df1, df2) |>
    dplyr::mutate(food_item = stringr::str_trim(food_item)) |>
    dplyr::distinct(!!rlang::sym(location_col), food_item, unit)

  final <- combined |>
    dplyr::mutate(amount = NA_real_, gram = NA_real_) |>
    tibble::as_tibble()

  # ---- Early exit if no non-gram foods ----
  if (nrow(final) == 0) {
    message(
      "All food items in the dataset are recorded in grams ",
      '("g from scale" or "g from photobook").\n',
      "No non-gram food items were found."
    )
    return(final)
  }

  # ---- Optional Excel export ----
  if (!is.null(export_path)) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "non_gram_foods")
    openxlsx::writeData(wb, "non_gram_foods", final)
    openxlsx::saveWorkbook(wb, export_path, overwrite = TRUE)
  }

  return(final)
}
