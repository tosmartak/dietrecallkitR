#' Get Non-Gram Foods
#'
#' Extracts unique food items from a dietary recall Excel file where units are
#' not measured in grams (scale or photobook). Combines results from the
#' \code{food_details} and \code{food_ingredients_group} sheets and links them
#' with household location information from the \code{maintable}.
#'
#' Optionally, the results can be exported to Excel. The exported file contains
#' all relevant columns, with two extra empty columns (\code{amount}, \code{gram})
#' for user input. Users are expected to fill in only these two columns and
#' should avoid modifying the location column, \code{food_item}, or \code{unit}.
#'
#' @param filepath Path to the Excel file containing the dietary recall sheets.
#' @param location_col Name of the location column in the maintable
#'   (e.g., subcounty, district). Defaults to \code{"subcounty"}.
#' @param maintable_sheet Name of the sheet containing the maintable.
#'   Defaults to \code{"maintable"}.
#' @param food_details_sheet Name of the sheet containing the food details.
#'   Defaults to \code{"food_details"}.
#' @param food_ingredients_sheet Name of the sheet containing food ingredients.
#'   Defaults to \code{"food_ingredients_group"}.
#' @param export_path Optional. File path to export results as an Excel file.
#'   If supplied, the Excel file will be created with all required columns and
#'   empty \code{amount} and \code{gram} fields for later user input.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{[location_col]}{Location identifier column, as specified in maintable}
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
#' and skip Excel export (even if \code{export_path} is provided).
#'
#' @examples
#' \dontrun{
#' # Return tibble only
#' get_non_gram_foods("dietary_recall_full.xlsx")
#'
#' # Return tibble and export to Excel
#' get_non_gram_foods("dietary_recall_full.xlsx", export_path = "non_gram_foods.xlsx")
#' }
#'
#' @export
get_non_gram_foods <- function(filepath,
                               location_col = "subcounty",
                               maintable_sheet = "maintable",
                               food_details_sheet = "food_details",
                               food_ingredients_sheet = "food_ingredients_group",
                               export_path = NULL) {
  stopifnot(file.exists(filepath))

  if (!requireNamespace("readxl", quietly = TRUE)) stop("Package 'readxl' is required but not installed.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required but not installed.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required but not installed.")

  # ---- Ensure sheets exist ----
  sheets <- readxl::excel_sheets(filepath)
  for (s in c(maintable_sheet, food_details_sheet, food_ingredients_sheet)) {
    if (!(s %in% sheets)) stop("Sheet '", s, "' not found in Excel file.")
  }

  # ---- Read sheets ----
  maintable <- readxl::read_excel(filepath, sheet = maintable_sheet)
  food_details <- readxl::read_excel(filepath, sheet = food_details_sheet)
  food_ingredients_group <- readxl::read_excel(filepath, sheet = food_ingredients_sheet)

  # ---- Validate subcounty column ----
  if (!location_col %in% names(maintable)) {
    stop("Column '", location_col, "' not found in maintable.")
  }

  # ---- food_details ----
  df1 <- food_details %>%
    dplyr::filter(
      !is.na(desc_of_food),
      !unit_qty_food_consumed %in% c("g from scale", "g from photobook")
    ) %>%
    dplyr::left_join(
      maintable %>% dplyr::select(survey_id, !!rlang::sym(location_col)),
      by = "survey_id"
    ) %>%
    dplyr::select(
      !!rlang::sym(location_col),
      food_item = desc_of_food,
      unit = unit_qty_food_consumed
    )

  # ---- food_ingredients_group ----
  df2 <- food_ingredients_group %>%
    dplyr::filter(!food_ingredient_unit %in% c("g from scale", "g from photobook")) %>%
    dplyr::left_join(
      maintable %>% dplyr::select(survey_id, !!rlang::sym(location_col)),
      by = "survey_id"
    ) %>%
    dplyr::select(
      !!rlang::sym(location_col),
      food_item = food_ingredients_used,
      unit = food_ingredient_unit
    )

  # ---- Combine + clean ----
  combined <- dplyr::bind_rows(df1, df2) %>%
    dplyr::mutate(food_item = stringr::str_trim(food_item)) %>%
    dplyr::distinct(!!rlang::sym(location_col), food_item, unit)

  final <- combined %>%
    dplyr::mutate(amount = NA_real_, gram = NA_real_)

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
  if (nrow(final) > 0 && !is.null(export_path)) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' is required for export but not installed.")
    }
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "non_gram_foods")
    openxlsx::writeData(wb, "non_gram_foods", final)
    openxlsx::saveWorkbook(wb, export_path, overwrite = TRUE)
  }

  return(final)
}
