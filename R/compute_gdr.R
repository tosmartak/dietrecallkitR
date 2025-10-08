#' Compute Global Dietary Recommendations (GDR) Score
#'
#' Computes the Global Dietary Recommendation (GDR) score from cleaned dietary recall data.
#' Uses either the package FCT database (online CSV) or a user-supplied mapping.
#'
#' @param recall_data A data frame containing at least the ID column and the food item column.
#' @param id_col Character. Name of the identifier column in `recall_data`
#'   (e.g., `"survey_id"`).
#' @param food_item_col Character. Column in `recall_data` containing food item names.
#' @param use_fct_db Logical, default = FALSE. If TRUE, uses the package's
#'   online FCT database (`gdr_category` column).
#' @param gdr_map_data Optional. A user-provided mapping data frame with
#'   food items and GDR categories. Required if `use_fct_db = FALSE`.
#' @param gdr_map_food Character. Column name in `gdr_map_data` that matches food items.
#' @param gdr_map_col Character. Column name in `gdr_map_data` containing the GDR category.
#'
#' @return A tibble with:
#' - The ID column (as provided in `id_col`)
#' - One-hot encoded GDR group indicators
#' - `gdrplus`  = sum of positive GDR categories
#' - `gdrminus` = sum of negative GDR categories
#' - `gdr_score` = net score (gdrplus - gdrminus + 9)
#'
#' @details
#' - Valid GDR+ categories: Whole Grains, Legumes, Nuts And Seeds,
#'   Vitamin A-Rich Orange Vegetables, Roots And Tubers,
#'   Dark Green Leafy Vegetables, Other Vegetables,
#'   Vitamin A-Rich Fruits, Citrus Fruits, Other Fruits.
#' - Valid GDR– categories: Sodas/Sugar-Sweetened Beverages,
#'   Baked/Grain-Based Sweets, Other Sweets, Processed Meat,
#'   Unprocessed Red Meat, Deep-Fried Foods,
#'   Fast Food And Instant Noodles, Packaged Ultra-Processed Salty Snacks.
#' - Processed Meat is scored as 2 if consumed.
#' - Warns if unmapped items are found in `recall_data` and ignored in subsequent computation.
#' - Errors if invalid GDR categories are found in mapping.
#'
#' @examples
#' \dontrun{
#' # Using online FCT DB
#' result <- compute_gdr(
#'   recall_data   = dietrecall_example$food_details,
#'   id_col        = "survey_id",
#'   food_item_col = "desc_of_food",
#'   use_fct_db    = TRUE
#' )
#'
#' # Using custom mapping
#' gdr_map <- data.frame(
#'   item = c("Ugali", "Cola"),
#'   category = c("Whole Grains", "Sodas/Sugar-Sweetened Beverages")
#' )
#'
#' result <- compute_gdr(
#'   recall_data   = dietrecall_example$food_details,
#'   id_col        = "survey_id",
#'   food_item_col = "desc_of_food",
#'   use_fct_db    = FALSE,
#'   gdr_map_data  = gdr_map,
#'   gdr_map_food  = "item",
#'   gdr_map_col   = "category"
#' )
#' }
#'
#' @export
compute_gdr <- function(recall_data,
                        id_col,
                        food_item_col,
                        use_fct_db = FALSE,
                        gdr_map_data = NULL,
                        gdr_map_food = NULL,
                        gdr_map_col = NULL) {
  # --- 1) Validate inputs ---
  stopifnot(is.data.frame(recall_data))
  stopifnot(id_col %in% names(recall_data))
  stopifnot(food_item_col %in% names(recall_data))

  if (use_fct_db && !is.null(gdr_map_data)) {
    stop("You cannot set both `use_fct_db = TRUE` and provide `gdr_map_data`. Choose one.")
  }

  if (!use_fct_db) {
    if (is.null(gdr_map_data) || is.null(gdr_map_food) || is.null(gdr_map_col)) {
      stop("When `use_fct_db = FALSE`, you must provide `gdr_map_data`, `gdr_map_food`, and `gdr_map_col`.")
    }
    stopifnot(is.data.frame(gdr_map_data))
    stopifnot(all(c(gdr_map_food, gdr_map_col) %in% names(gdr_map_data)))
  }

  # --- 2) Load mapping ---
  if (use_fct_db) {
    db_url <- "https://raw.githubusercontent.com/tosmartak/dietrecallkitR/main/data-raw/fct_db.csv"
    raw_map <- suppressMessages(readr::read_csv(db_url, show_col_types = FALSE))

    gdr_map <- raw_map |>
      dplyr::filter(gdr_category != "No GDR Assignment") |>
      dplyr::select(food_item, gdr = gdr_category) |>
      dplyr::mutate(
        food_item = stringr::str_trim(food_item),
        gdr = stringr::str_trim(gdr)
      )
  } else {
    gdr_map <- gdr_map_data |>
      dplyr::select(
        food_item = !!rlang::sym(gdr_map_food),
        gdr = !!rlang::sym(gdr_map_col)
      ) |>
      dplyr::mutate(
        food_item = stringr::str_trim(food_item),
        gdr = stringr::str_trim(gdr)
      )
  }

  # --- 3) Prepare recall data ---
  recall <- recall_data |>
    dplyr::select(
      !!rlang::sym(id_col),
      food_item = !!rlang::sym(food_item_col)
    ) |>
    dplyr::mutate(food_item = stringr::str_trim(food_item))

  # --- 4) Merge recall with mapping + warn on unmapped ---
  merged <- recall |>
    dplyr::left_join(gdr_map, by = "food_item")

  # Warn unmapped items
  unmapped <- merged |>
    dplyr::filter(is.na(gdr)) |>
    dplyr::distinct(food_item)

  if (nrow(unmapped) > 0) {
    warning("Unmapped food items:\n", paste0("- ", unmapped$food_item, collapse = "\n"))
  }

  # --- 5) Handle empty / no mapped foods ---
  if (nrow(merged |> dplyr::filter(!is.na(gdr))) == 0) {
    warning("No mapped foods found. Returning only scores with zeros.")
    out <- recall |>
      dplyr::select(!!rlang::sym(id_col)) |>
      dplyr::distinct()
    out$gdrplus <- 0
    out$gdrminus <- 0
    out$gdr_score <- 0
    return(tibble::as_tibble(out))
  }

  # --- 6) Split semicolon-separated categories ---
  merged <- merged |>
    tidyr::separate_rows(gdr, sep = ";") |>
    dplyr::mutate(gdr = stringr::str_to_title(stringr::str_trim(gdr)))

  # --- 7) Validate GDR categories ---
  GDRplus <- c(
    "Whole Grains", "Legumes", "Nuts And Seeds",
    "Vitamin A-Rich Orange Vegetables, Roots And Tubers",
    "Dark Green Leafy Vegetables", "Other Vegetables",
    "Vitamin A-Rich Fruits", "Citrus Fruits", "Other Fruits"
  )
  GDRminus <- c(
    "Sodas/Sugar-Sweetened Beverages", "Baked/Grain-Based Sweets", "Other Sweets",
    "Processed Meat", "Unprocessed Red Meat", "Deep-Fried Foods",
    "Fast Food And Instant Noodles", "Packaged Ultra-Processed Salty Snacks"
  )
  valid_categories <- c(GDRplus, GDRminus)

  invalid <- merged |>
    dplyr::filter(!is.na(gdr), !gdr %in% valid_categories) |>
    dplyr::distinct(gdr)

  if (nrow(invalid) > 0) {
    stop("Invalid GDR categories found: ", paste(invalid$gdr, collapse = ", "))
  }

  # --- 8) Dummy encode ---
  gdr_wide <- merged |>
    dplyr::filter(!is.na(gdr)) |>
    fastDummies::dummy_cols(
      select_columns = "gdr",
      remove_first_dummy = FALSE,
      split = NULL,
      remove_selected_columns = TRUE
    ) |>
    dplyr::select(-food_item)

  # --- 9) Group by ID and aggregate ---
  gdr_grouped <- gdr_wide |>
    dplyr::group_by(!!rlang::sym(id_col)) |>
    dplyr::summarise(
      dplyr::across(dplyr::starts_with("gdr_"), ~ max(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  # --- 10) Ensure all expected GDR columns exist ---
  all_expected <- paste0("gdr_", valid_categories)
  for (col in all_expected) {
    if (!(col %in% names(gdr_grouped))) {
      gdr_grouped[[col]] <- 0
    }
  }

  # --- 11) Reorder columns ---
  gdr_grouped <- gdr_grouped |>
    dplyr::select(!!rlang::sym(id_col), dplyr::all_of(all_expected))

  # --- 12) Special rule: Processed Meat = 2 if consumed ---
  gdr_grouped <- gdr_grouped |>
    dplyr::mutate(`gdr_Processed Meat` = ifelse(`gdr_Processed Meat` == 1, 2, 0))

  # --- 12) Compute scores ---
  gdr_grouped <- gdr_grouped |>
    dplyr::mutate(
      gdrplus   = rowSums(dplyr::across(dplyr::all_of(paste0("gdr_", GDRplus))), na.rm = TRUE),
      gdrminus  = rowSums(dplyr::across(dplyr::all_of(paste0("gdr_", GDRminus))), na.rm = TRUE),
      gdr_score = gdrplus - gdrminus + 9
    ) |>
    janitor::clean_names()

  return(tibble::as_tibble(gdr_grouped))
}
