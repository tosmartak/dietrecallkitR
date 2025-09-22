#' Get Unique Non-Gram Food Items
#'
#' Extracts unique food items from a dietary recall Excel file where units are
#' not measured in grams (scale or photobook). Combines results from the
#' \code{food_details} and \code{food_ingredients_group} sheets and links them
#' with household subcounty information from the \code{maintable}.
#'
#' Optionally, the results can be exported to Excel. The exported file contains
#' all relevant columns, with two extra empty columns (\code{amount}, \code{gram})
#' for user input. Users are expected to fill in only these two columns and
#' should avoid modifying \code{subcounty}, \code{food_item}, or \code{unit}.
#'
#' @param filepath Path to the Excel file containing the sheets
#'   \code{maintable}, \code{food_details}, and \code{food_ingredients_group}.
#' @param subcounty_col Name of the subcounty column in the maintable.
#'   Defaults to \code{"subcounty"}.
#' @param export_path Optional. File path to export results as an Excel file.
#'   If supplied, the Excel file will be created with all required columns and
#'   empty \code{amount} and \code{gram} fields for later user input.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{subcounty}{Subcounty identifier}
#'   \item{food_item}{Food item consumed or ingredient used}
#'   \item{unit}{Original unit recorded (not grams)}
#'   \item{amount}{Empty column for later input}
#'   \item{gram}{Empty column for later input}
#' }
#'
#' @examples
#' \dontrun{
#' # Write packaged example dataset to a temporary Excel file
#' tmpfile <- tempfile(fileext = ".xlsx")
#' openxlsx::write.xlsx(
#'   list(
#'     maintable = dietrecall_example$maintable,
#'     food_details = dietrecall_example$food_details,
#'     food_ingredients_group = dietrecall_example$food_ingredients_group
#'   ),
#'   tmpfile
#' )
#'
#' # Return tibble only
#' df <- get_non_gram_foods(tmpfile)
#' head(df)
#'
#' # Return tibble and export to Excel
#' out_file <- tempfile(fileext = ".xlsx")
#' get_non_gram_foods(tmpfile, export_path = out_file)
#'
#' # In the exported file:
#' # - Use the 'amount' and 'gram' columns for input
#' # - Do not modify 'subcounty', 'food_item', or 'unit'
#' }
#'
#' @export
get_non_gram_foods <- function(filepath,
                               subcounty_col = "subcounty",
                               export_path = NULL) {
  stopifnot(file.exists(filepath))

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required but not installed.")
  }

  maintable <- readxl::read_excel(filepath, sheet = "maintable")
  food_details <- readxl::read_excel(filepath, sheet = "food_details")
  food_ingredients_group <- readxl::read_excel(filepath, sheet = "food_ingredients_group")

  if (!subcounty_col %in% names(maintable)) {
    stop("Column '", subcounty_col, "' not found in maintable.")
  }

  # ---- food_details ----
  df1 <- food_details %>%
    dplyr::filter(
      (food_preparation_place == "Outside Home" | ready_to_eat == 1),
      !unit_qty_food_consumed %in% c("g from scale", "g from photobook")
    ) %>%
    dplyr::left_join(
      maintable %>% dplyr::select(survey_id, !!subcounty_col),
      by = "survey_id"
    ) %>%
    dplyr::select(
      subcounty = !!rlang::sym(subcounty_col),
      food_item = food_item_selected,
      unit = unit_qty_food_consumed
    )

  # ---- food_ingredients_group ----
  df2 <- food_ingredients_group %>%
    dplyr::filter(!food_ingredient_unit %in% c("g from scale", "g from photobook")) %>%
    dplyr::left_join(
      maintable %>% dplyr::select(survey_id, !!subcounty_col),
      by = "survey_id"
    ) %>%
    dplyr::select(
      subcounty = !!rlang::sym(subcounty_col),
      food_item = food_ingredients_used,
      unit = food_ingredient_unit
    )

  # ---- Combine + clean ----
  combined <- dplyr::bind_rows(df1, df2) %>%
    dplyr::mutate(food_item = stringr::str_trim(food_item)) %>%
    dplyr::distinct(subcounty, food_item, unit)

  final <- combined %>%
    dplyr::mutate(amount = NA_real_, gram = NA_real_)

  # ---- Optional Excel export ----
  if (!is.null(export_path)) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' is required for export but not installed.")
    }

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "unique_food_items")
    openxlsx::writeData(wb, "unique_food_items", final)

    # Write files into excel
    openxlsx::saveWorkbook(wb, export_path, overwrite = TRUE)
  }

  return(final)
}
