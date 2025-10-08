#' Compute the Global Dietary Quality Score (GDQS)
#'
#' This function computes the Global Dietary Quality Score (GDQS) from individual
#' 24-hour dietary recall data. It maps reported food items to GDQS categories,
#' aggregates intakes, applies scoring rules for each category, and calculates
#' summary indices (GDQS+, GDQS-, overall GDQS, and risk classification).
#' The function can either use the built-in food composition database (fct_db)
#' from the dietrecallkitR GitHub repository or a user-supplied mapping table.
#'
#' @param recall_data A data frame containing at least an ID column, a food item column,
#'   and a numeric gram intake column.
#' @param id_col Character string. Column name in `recall_data` identifying survey
#'   respondents or household IDs.
#' @param food_item_col Character string. Column name in `recall_data` with reported food items.
#' @param actual_gram_intake_col Character string. Column name in `recall_data` containing
#'   numeric gram intakes (default is `"actual_gram_intake"`).
#' @param use_fct_db Logical. If `TRUE`, mapping is automatically pulled from the built-in
#'   food composition database (fct_db). Cannot be used simultaneously with `gdqs_map_data`.
#' @param gdqs_map_data Optional. A user-supplied mapping data frame that links food items
#'   to GDQS categories. Required if `use_fct_db = FALSE`.
#' @param gdqs_map_food Character string. Column name in `gdqs_map_data` with food item names.
#' @param gdqs_map_col Character string. Column name in `gdqs_map_data` with GDQS category names.
#'
#' @return A tibble with one row per respondent (ID). Output columns include:
#' \itemize{
#'   \item `gdqs_<category>`: Numeric intakes per GDQS category.
#'   \item `gdqs_<category>_consumed`: Binary indicator (0/1) if category consumed.
#'   \item `gdqs_<category>_points`: Points allocated per GDQS scoring rules.
#'   \item `gdqs_plus`: Sum of points from healthy GDQS categories.
#'   \item `gdqs_minus`: Sum of points from unhealthy GDQS categories.
#'   \item `gdqs_overall`: Total GDQS score (`gdqs_plus + gdqs_minus`).
#'   \item `gdqs_risk`: Risk classification: "High" (<15), "Moderate" (15–22), or "Low" (≥23).
#' }
#'
#' @details
#' The GDQS scoring algorithm follows established cutoffs for each category.
#' Foods can belong to multiple GDQS categories, in which case their gram intake
#' is divided equally across categories. If no mapped foods are found, the function
#' returns zeros and assigns `gdqs_risk = "High"`. Unmapped food items are listed
#' via a warning message.
#'
#' @examples
#' \dontrun{
#' # Example with built-in fct_db
#' recall <- tibble::tibble(
#'   survey_id = 1:3,
#'   food_item = c("Beans, broad, dry, raw", "Sausage", "Orange (chungwa), pulp, raw"),
#'   actual_gram_intake = c(50, 100, 150)
#' )
#' result <- compute_gdqs(
#'   recall_data   = recall,
#'   id_col        = "survey_id",
#'   food_item_col = "food_item",
#'   use_fct_db    = TRUE
#' )
#'
#' # Example with custom mapping
#' recall <- tibble::tibble(
#'   id = c(1, 2),
#'   food = c("Ugali", "Beans"),
#'   grams = c(200, 50)
#' )
#' gdqs_map <- tibble::tibble(
#'   item = c("Ugali", "Beans"),
#'   gdqs = c("Whole Grains", "Legumes")
#' )
#' result <- compute_gdqs(
#'   recall_data = recall,
#'   id_col = "id",
#'   food_item_col = "food",
#'   actual_gram_intake_col = "grams",
#'   use_fct_db = FALSE,
#'   gdqs_map_data = gdqs_map,
#'   gdqs_map_food = "item",
#'   gdqs_map_col = "gdqs"
#' )
#' }
#'
#' @export
compute_gdqs <- function(recall_data,
                         id_col,
                         food_item_col,
                         actual_gram_intake_col = "actual_gram_intake",
                         use_fct_db = FALSE,
                         gdqs_map_data = NULL,
                         gdqs_map_food = NULL,
                         gdqs_map_col = NULL) {
  # --- 1) Validate inputs ---
  stopifnot(is.data.frame(recall_data))
  stopifnot(id_col %in% names(recall_data))
  stopifnot(food_item_col %in% names(recall_data))
  stopifnot(actual_gram_intake_col %in% names(recall_data))

  if (use_fct_db && !is.null(gdqs_map_data)) {
    stop("You cannot set both `use_fct_db = TRUE` and provide `gdqs_map_data`. Choose one.")
  }

  if (!use_fct_db) {
    if (is.null(gdqs_map_data) || is.null(gdqs_map_food) || is.null(gdqs_map_col)) {
      stop("When `use_fct_db = FALSE`, you must provide `gdqs_map_data`, `gdqs_map_food`, and `gdqs_map_col`.")
    }
    stopifnot(is.data.frame(gdqs_map_data))
    stopifnot(all(c(gdqs_map_food, gdqs_map_col) %in% names(gdqs_map_data)))
  }

  # --- 2) Load mapping ---
  if (use_fct_db) {
    db_url <- "https://raw.githubusercontent.com/tosmartak/dietrecallkitR/main/data-raw/fct_db.csv"
    raw_map <- suppressMessages(readr::read_csv(db_url, show_col_types = FALSE))
    gdqs_map <- raw_map |>
      dplyr::filter(gdqs_category != "No GDQS Assignment") |>
      dplyr::select(food_item, gdqs = gdqs_category) |>
      dplyr::mutate(
        food_item = stringr::str_trim(food_item),
        gdqs = stringr::str_trim(gdqs)
      )
  } else {
    gdqs_map <- gdqs_map_data |>
      dplyr::select(
        food_item = !!rlang::sym(gdqs_map_food),
        gdqs = !!rlang::sym(gdqs_map_col)
      ) |>
      dplyr::mutate(
        food_item = stringr::str_trim(food_item),
        gdqs = stringr::str_trim(gdqs)
      )
  }

  # --- 3) Prepare recall data ---
  recall <- recall_data |>
    dplyr::select(
      !!rlang::sym(id_col),
      food_item = !!rlang::sym(food_item_col),
      intake = !!rlang::sym(actual_gram_intake_col)
    ) |>
    dplyr::mutate(food_item = stringr::str_trim(food_item))

  # --- 4) Merge recall with mapping ---
  merged <- recall |> dplyr::left_join(gdqs_map, by = "food_item")

  unmapped <- merged |>
    dplyr::filter(is.na(gdqs)) |>
    dplyr::distinct(food_item)
  if (nrow(unmapped) > 0) {
    warning("Unmapped food items:\n", paste0("- ", unmapped$food_item, collapse = "\n"))
  }

  # --- 5) Handle empty case ---
  if (nrow(merged |> dplyr::filter(!is.na(gdqs))) == 0) {
    warning("No mapped foods found. Returning zero scores only.")
    out <- recall |>
      dplyr::select(!!rlang::sym(id_col)) |>
      dplyr::distinct()
    out$gdqs_plus <- 0
    out$gdqs_minus <- 0
    out$gdqs_overall <- 0
    out$gdqs_risk <- "High"
    return(tibble::as_tibble(out))
  }

  # --- 6) Split multi-category foods & divide intake ---
  merged <- merged |>
    dplyr::filter(!is.na(gdqs)) |>
    dplyr::mutate(gdqs = stringr::str_split(gdqs, ";")) |>
    tidyr::unnest(gdqs) |>
    dplyr::mutate(gdqs = stringr::str_to_title(stringr::str_trim(gdqs))) |>
    dplyr::group_by(!!rlang::sym(id_col), food_item) |>
    dplyr::mutate(intake = intake / dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(gdqs != "Others")

  # --- 7) Validate GDQS categories ---
  gdqs_healthy <- c(
    "Citrus Fruits", "Deep Orange Fruits", "Other Fruits",
    "Dark Green Leafy Vegetables", "Cruciferous Vegetables", "Deep Orange Vegetables",
    "Other Vegetables", "Legumes", "Deep Orange Tubers",
    "Nuts And Seeds", "Whole Grains", "Liquid Oils",
    "Fish And Shellfish", "Poultry And Game Meat", "Low Fat Dairy", "Eggs"
  )
  gdqs_unhealthy <- c(
    "High Fat Dairy", "Red Meat", "Processed Meat", "Refined Grains And Baked Goods",
    "Sweets And Ice Cream", "Sugar-Sweetened Beverages", "Juice",
    "White Roots And Tubers", "Purchased Deep-Fried Foods"
  )
  valid_gdqs <- c(gdqs_healthy, gdqs_unhealthy)

  invalid <- merged |>
    dplyr::filter(!gdqs %in% valid_gdqs) |>
    dplyr::distinct(gdqs)
  if (nrow(invalid) > 0) {
    stop("Invalid GDQS categories found: ", paste(invalid$gdqs, collapse = ", "))
  }

  # --- 8) Aggregate to ID x Category ---
  df_group <- merged |>
    dplyr::group_by(!!rlang::sym(id_col), gdqs) |>
    dplyr::summarise(intake = sum(intake, na.rm = TRUE), .groups = "drop")

  # --- 9) Pivot wide and ensure expected columns ---
  df_wide <- df_group |>
    tidyr::pivot_wider(names_from = gdqs, values_from = intake, values_fill = 0)

  all_expected <- valid_gdqs
  for (col in all_expected) {
    if (!(col %in% names(df_wide))) df_wide[[col]] <- 0
  }
  df_wide <- df_wide |>
    dplyr::select(!!rlang::sym(id_col), dplyr::all_of(all_expected))

  # --- 10) Add consumed flags ---
  intake_cols <- setdiff(names(df_wide), id_col)
  for (col in intake_cols) {
    df_wide[[paste0(col, "_consumed")]] <- ifelse(df_wide[[col]] > 0, 1, 0)
  }

  # --- 11) Scoring system ---
  gdqs_points <- list(
    "Citrus Fruits" = list(ranges = c(0, 24, 70, Inf), values = c(0, 1, 2)),
    "Deep Orange Fruits" = list(ranges = c(0, 25, 124, Inf), values = c(0, 1, 2)),
    "Other Fruits" = list(ranges = c(0, 27, 108, Inf), values = c(0, 1, 2)),
    "Dark Green Leafy Vegetables" = list(ranges = c(0, 13, 38, Inf), values = c(0, 2, 4)),
    "Cruciferous Vegetables" = list(ranges = c(0, 13, 37, Inf), values = c(0, 0.25, 0.5)),
    "Deep Orange Vegetables" = list(ranges = c(0, 9, 46, Inf), values = c(0, 0.25, 0.5)),
    "Other Vegetables" = list(ranges = c(0, 23, 115, Inf), values = c(0, 0.25, 0.5)),
    "Legumes" = list(ranges = c(0, 9, 43, Inf), values = c(0, 2, 4)),
    "Deep Orange Tubers" = list(ranges = c(0, 12, 64, Inf), values = c(0, 0.25, 0.5)),
    "Nuts And Seeds" = list(ranges = c(0, 7, 14, Inf), values = c(0, 2, 4)),
    "Whole Grains" = list(ranges = c(0, 8, 14, Inf), values = c(0, 1, 2)),
    "Liquid Oils" = list(ranges = c(0, 2, 7.51, Inf), values = c(0, 1, 2)),
    "Fish And Shellfish" = list(ranges = c(0, 14, 72, Inf), values = c(0, 1, 2)),
    "Poultry And Game Meat" = list(ranges = c(0, 16, 45, Inf), values = c(0, 1, 2)),
    "Low Fat Dairy" = list(ranges = c(0, 33, 133, Inf), values = c(0, 1, 2)),
    "Eggs" = list(ranges = c(0, 6, 33, Inf), values = c(0, 1, 2)),
    "High Fat Dairy" = list(ranges = c(0, 35, 143, 735, Inf), values = c(0, 1, 2, 0)),
    "Red Meat" = list(ranges = c(0, 9, 47, Inf), values = c(0, 1, 0)),
    "Processed Meat" = list(ranges = c(0, 9, 31, Inf), values = c(2, 1, 0)),
    "Refined Grains And Baked Goods" = list(ranges = c(0, 7, 34, Inf), values = c(2, 1, 0)),
    "Sweets And Ice Cream" = list(ranges = c(0, 13, 38, Inf), values = c(2, 1, 0)),
    "Sugar-Sweetened Beverages" = list(ranges = c(0, 57, 181, Inf), values = c(2, 1, 0)),
    "Juice" = list(ranges = c(0, 36, 145, Inf), values = c(2, 1, 0)),
    "White Roots And Tubers" = list(ranges = c(0, 27, 108, Inf), values = c(2, 1, 0)),
    "Purchased Deep-Fried Foods" = list(ranges = c(0, 9, 46, Inf), values = c(2, 1, 0))
  )

  for (cat in names(gdqs_points)) {
    col <- cat
    pts_col <- paste0(col, "_points")
    if (col %in% names(df_wide)) {
      ranges <- gdqs_points[[cat]]$ranges
      values <- gdqs_points[[cat]]$values
      df_wide[[pts_col]] <- cut(df_wide[[col]],
        breaks = ranges,
        labels = values,
        include.lowest = TRUE,
        right = FALSE
      ) |>
        as.character() |>
        as.numeric()
    }
  }

  # --- 12) Compute GDQS summary ---
  df_wide <- df_wide |>
    dplyr::mutate(
      gdqs_plus = rowSums(dplyr::across(paste0(gdqs_healthy, "_points")), na.rm = TRUE),
      gdqs_minus = rowSums(dplyr::across(paste0(gdqs_unhealthy, "_points")), na.rm = TRUE),
      gdqs_overall = gdqs_plus + gdqs_minus,
      gdqs_risk = dplyr::case_when(
        gdqs_overall < 15 ~ "High",
        gdqs_overall < 23 ~ "Moderate",
        TRUE ~ "Low"
      )
    )

  # --- 13) Add gdqs_ prefix (except ID and summary cols) ---
  summary_cols <- c(id_col, "gdqs_plus", "gdqs_minus", "gdqs_overall", "gdqs_risk")
  df_prefixed <- df_wide |>
    dplyr::rename_with(~ paste0("gdqs_", .x), setdiff(names(df_wide), summary_cols)) |>
    janitor::clean_names()

  # --- 14) Add _g_intake suffix to raw intake columns ---
  intake_cols <- c(
    "gdqs_citrus_fruits", "gdqs_deep_orange_fruits", "gdqs_other_fruits",
    "gdqs_dark_green_leafy_vegetables", "gdqs_cruciferous_vegetables",
    "gdqs_deep_orange_vegetables", "gdqs_other_vegetables", "gdqs_legumes",
    "gdqs_deep_orange_tubers", "gdqs_nuts_and_seeds", "gdqs_whole_grains",
    "gdqs_liquid_oils", "gdqs_fish_and_shellfish", "gdqs_poultry_and_game_meat",
    "gdqs_low_fat_dairy", "gdqs_eggs", "gdqs_high_fat_dairy", "gdqs_red_meat",
    "gdqs_processed_meat", "gdqs_refined_grains_and_baked_goods",
    "gdqs_sweets_and_ice_cream", "gdqs_sugar_sweetened_beverages",
    "gdqs_juice", "gdqs_white_roots_and_tubers",
    "gdqs_purchased_deep_fried_foods"
  )

  df_prefixed <- df_prefixed |>
    dplyr::rename_with(~ paste0(.x, "_g_intake"), dplyr::any_of(intake_cols))

  return(tibble::as_tibble(df_prefixed))
}
