#' Harmonize Food Items and Ingredients
#'
#' Produces a unified dataset of foods and ingredients reported in a dietary recall,
#' keeping only the survey identifier (`key`) and the harmonized food item name.
#'
#' @param food_details A data frame with food-level information (must include
#'   the column specified in `key` and `desc_of_food`).
#' @param food_ingredients A data frame with ingredient-level information (must include
#'   the column specified in `key` and `food_ingredients_used`).
#' @param key Character string. The column name that uniquely links
#'   records across `food_details` and `food_ingredients`.
#'   Defaults to `"survey_id"`.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{<key>}{Survey identifier (column name matches `key` argument)}
#'   \item{food_item}{Name of food or ingredient}
#' }
#'
#' @details
#' - Foods with `NA` in `desc_of_food` are excluded.
#' - No deduplication is forced: if the same item appears in both
#'   foods and ingredients, both records are returned.
#' - This function is primarily used for harmonizing raw item names before
#'   linking to external food composition or coding systems.
#'
#' @examples
#' data("dietrecall_example")
#' result <- harmonize_food_and_ingredients(
#'   food_details = dietrecall_example$food_details,
#'   food_ingredients = dietrecall_example$food_ingredients_group,
#'   key = "survey_id"
#' )
#' head(result)
#'
#' @export
harmonize_food_and_ingredients <- function(food_details,
                                           food_ingredients,
                                           key = "survey_id") {
  stopifnot(is.data.frame(food_details), is.data.frame(food_ingredients))
  stopifnot(key %in% names(food_details), key %in% names(food_ingredients))

  # --- Process food_details ---
  fd_clean <- food_details |>
    dplyr::filter(!is.na(desc_of_food)) |>
    dplyr::select(
      !!rlang::sym(key),
      food_item = desc_of_food
    ) |>
    dplyr::mutate(food_item = stringr::str_trim(food_item))

  # --- Process food_ingredients ---
  fig_clean <- food_ingredients |>
    dplyr::select(
      !!rlang::sym(key),
      food_item = food_ingredients_used
    ) |>
    dplyr::mutate(food_item = stringr::str_trim(food_item))

  # --- Coerce key to character to avoid bind_rows error ---
  fd_clean[[key]] <- as.character(fd_clean[[key]])
  fig_clean[[key]] <- as.character(fig_clean[[key]])

  # --- Combine ---
  final <- dplyr::bind_rows(fd_clean, fig_clean) |>
    tibble::as_tibble()

  return(final)
}
