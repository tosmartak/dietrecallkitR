#' Compute Minimum Dietary Diversity (MDD)
#'
#' Computes Minimum Dietary Diversity (MDD) and Dietary Diversity Score (DDS)
#' for women, men, adolescents, or children, from cleaned dietary recall data.
#'
#' @param recall_data A data frame of recall data containing at least the ID
#'   column and the food item column.
#' @param id_col Character. Name of the identifier column in `recall_data`
#'   (e.g., `"survey_id"`).
#' @param food_item_col Character. Name of the column in `recall_data` containing food item names.
#' @param life_stage One of `"woman"`, `"man"`, `"adolescent"`, `"child"`.
#' @param use_fct_db Logical, default = FALSE. If TRUE, uses the package's
#'   online FCT mapping database for food group classification.
#' @param fg_map_data Optional. A user-provided mapping data frame with
#'   food items and food groups. Required if `use_fct_db = FALSE`.
#' @param fg_map_food Character. Column name in `fg_map_data` that matches food items.
#' @param fg_map_col Character. Column name in `fg_map_data` that contains the food group.
#' @param breastfeeding_data Optional. Data frame with ID and breastfeeding status (binary).
#'   Only relevant if `life_stage = "child"`.
#' @param breastfeeding_col Character. Column name in `breastfeeding_data`
#'   that contains breastfeeding status.
#'
#' @return A tibble with:
#' - The ID column (as provided in `id_col`)
#' - One-hot encoded food group indicators
#' - `dds_<life_stage>` = Dietary Diversity Score
#' - `mdd_<life_stage>` = Binary indicator of meeting MDD
#'
#' @details
#' - For children: threshold is ≥5 if breastfeeding included, ≥4 otherwise.
#' - For adults (woman, man, adolescent): threshold is always ≥5.
#' - Warns if unmapped food items are found in recall data.
#' - Final result for children returns the expected 7 food groups and adults returns the expected 10 food group, even if not mapped to any food item
#' - While `Others` food group are part of the result, the column is not included in the summation of food groups for dds
#'
#' @examples
#' \dontrun{
#' # Using online FCT DB
#' result <- compute_mdd(
#'   recall_data = dietrecall_example$food_details,
#'   id_col = "survey_id",
#'   food_item_col = "desc_of_food",
#'   life_stage = "woman",
#'   use_fct_db = TRUE
#' )
#'
#' # Using custom mapping
#' fg_map <- data.frame(
#'   item = c("Ugali", "Milk"),
#'   group = c("Cereals", "Dairy")
#' )
#'
#' result <- compute_mdd(
#'   recall_data = dietrecall_example$food_details,
#'   id_col = "survey_id",
#'   food_item_col = "desc_of_food",
#'   life_stage = "child",
#'   use_fct_db = FALSE,
#'   fg_map_data = fg_map,
#'   fg_map_food = "item",
#'   fg_map_col = "group",
#'   breastfeeding_data = dietrecall_example$maintable,
#'   breastfeeding_col = "child_breastfed"
#' )
#' }
#'
#' @export
compute_mdd <- function(recall_data,
                        id_col,
                        food_item_col,
                        life_stage,
                        use_fct_db = FALSE,
                        fg_map_data = NULL,
                        fg_map_food = NULL,
                        fg_map_col = NULL,
                        breastfeeding_data = NULL,
                        breastfeeding_col = NULL) {
  # --- 1) Validate inputs ---
  stopifnot(is.data.frame(recall_data))
  stopifnot(id_col %in% names(recall_data))
  stopifnot(food_item_col %in% names(recall_data))
  stopifnot(life_stage %in% c("woman", "man", "adolescent", "child"))

  if (use_fct_db && !is.null(fg_map_data)) {
    stop("You cannot set both `use_fct_db = TRUE` and provide `fg_map_data`. Choose one.")
  }

  if (!use_fct_db) {
    if (is.null(fg_map_data) || is.null(fg_map_food) || is.null(fg_map_col)) {
      stop("When `use_fct_db = FALSE`, you must provide `fg_map_data`, `fg_map_food`, and `fg_map_col`.")
    }
    stopifnot(is.data.frame(fg_map_data))
    stopifnot(all(c(fg_map_food, fg_map_col) %in% names(fg_map_data)))
  }

  if (life_stage == "child" && !is.null(breastfeeding_data)) {
    stopifnot(is.data.frame(breastfeeding_data))
    stopifnot(id_col %in% names(breastfeeding_data))
    stopifnot(!is.null(breastfeeding_col))
    stopifnot(breastfeeding_col %in% names(breastfeeding_data))
  }
  if (life_stage != "child" && !is.null(breastfeeding_data)) {
    stop("Breastfeeding data should only be provided for `life_stage = 'child'`.")
  }

  # --- 2) Load mapping ---
  group_col <- paste0("mdd_", life_stage)

  if (use_fct_db) {
    db_url <- "https://raw.githubusercontent.com/tosmartak/dietrecallkitR/main/data-raw/fct_db.csv"
    raw_map <- suppressMessages(readr::read_csv(db_url, show_col_types = FALSE))

    needed <- if (life_stage == "child") "mdd_child" else "mdd_adult"

    fg_map <- raw_map |>
      dplyr::select(
        food_item,
        selected = !!rlang::sym(needed)
      ) |>
      dplyr::mutate(
        food_item = stringr::str_trim(food_item),
        selected  = stringr::str_trim(selected)
      )
    names(fg_map)[names(fg_map) == "selected"] <- group_col
  } else {
    fg_map <- fg_map_data |>
      dplyr::select(
        food_item = !!rlang::sym(fg_map_food),
        selected  = !!rlang::sym(fg_map_col)
      ) |>
      dplyr::mutate(
        food_item = stringr::str_trim(food_item),
        selected  = stringr::str_trim(selected)
      )
    names(fg_map)[names(fg_map) == "selected"] <- group_col
  }

  # --- 3) Prepare recall data (only the columns we need) ---
  recall <- recall_data |>
    dplyr::select(
      !!rlang::sym(id_col),
      food_item = !!rlang::sym(food_item_col)
    ) |>
    dplyr::mutate(
      food_item = stringr::str_trim(food_item)
    )

  # --- 4) Merge recall with mapping + warn on unmapped ---
  merged <- recall |>
    dplyr::left_join(fg_map, by = "food_item")

  unmapped <- merged |>
    dplyr::filter(is.na(!!rlang::sym(group_col))) |>
    dplyr::distinct(food_item)

  if (nrow(unmapped) > 0) {
    warning("Unmapped food items:\n", paste0("- ", unmapped$food_item, collapse = "\n"))
  }

  # --- 5) If no mapped categories at all, return zeros with full column set ---
  has_any_mapped <- nrow(merged |> dplyr::filter(!is.na(!!rlang::sym(group_col)))) > 0

  # Expected MDD category lists (verbatim as provided)
  expected_child <- c(
    "Grains, White Roots And Tubers And Plantains",
    "Legumes And Nuts",
    "Milk And Dairy Products",
    "Meat, Poultry And Fish",
    "Eggs",
    "Vitamin A-Rich Fruits And Vegetables",
    "Other Fruits And Vegetables",
    "Others"
  )
  expected_adult <- c(
    "Grains, White Roots And Tubers And Plantains",
    "Pulses (Beans, Peas And Lentils)",
    "Nuts And Seeds",
    "Milk And Dairy Products",
    "Meat, Poultry And Fish",
    "Eggs",
    "Dark Green Leafy Vegetables",
    "Vitamin A-Rich Fruits And Vegetables",
    "Other Vegetables",
    "Other Fruits",
    "Others"
  )
  expected <- if (life_stage == "child") expected_child else expected_adult

  if (!has_any_mapped) {
    warning("No mapped foods found. Returning only scores with zeros.")
    # Build skeleton with all expected mdd_* columns = 0
    out <- recall |>
      dplyr::select(!!rlang::sym(id_col)) |>
      dplyr::distinct()

    for (cat in expected) {
      out[[paste0(group_col, "_", cat)]] <- 0
    }

    # Optionally join breastfeeding (child only)
    if (life_stage == "child" && !is.null(breastfeeding_data)) {
      bf <- breastfeeding_data |>
        dplyr::select(!!rlang::sym(id_col), !!rlang::sym(breastfeeding_col))
      out <- out |>
        dplyr::left_join(bf, by = id_col)
    }

    # Reorder: id + all expected
    out <- out |>
      dplyr::select(!!rlang::sym(id_col), dplyr::all_of(paste0(group_col, "_", expected)), dplyr::everything())

    # Compute DDS / MDD (excluding "Others")
    dds_col <- paste0("dds_", life_stage)
    mdd_col <- paste0("mdd_", life_stage)
    score_cols <- paste0(group_col, "_", setdiff(expected, "Others"))

    if (life_stage == "child" && !is.null(breastfeeding_col)) {
      out[[dds_col]] <- rowSums(dplyr::select(out, dplyr::all_of(c(score_cols, breastfeeding_col))), na.rm = TRUE)
      threshold <- 5
    } else if (life_stage == "child") {
      out[[dds_col]] <- rowSums(dplyr::select(out, dplyr::all_of(score_cols)), na.rm = TRUE)
      threshold <- 4
    } else {
      out[[dds_col]] <- rowSums(dplyr::select(out, dplyr::all_of(score_cols)), na.rm = TRUE)
      threshold <- 5
    }

    out[[mdd_col]] <- ifelse(out[[dds_col]] >= threshold, 1, 0)

    return(tibble::as_tibble(janitor::clean_names(out)))
  }

  # --- 6) Split semicolon-separated categories BEFORE dummy encoding ---
  split_df <- merged |>
    dplyr::rename(grp = !!rlang::sym(group_col)) |>
    tidyr::separate_rows(grp, sep = ";") |>
    dplyr::mutate(grp = stringr::str_to_title(stringr::str_trim(grp)))

  # --- 7) Validate MDD categories against expected list ---
  detected <- split_df |>
    dplyr::filter(!is.na(grp)) |>
    dplyr::distinct(grp) |>
    dplyr::pull(grp)

  invalid <- setdiff(detected, expected)
  if (length(invalid) > 0) {
    stop("Invalid MDD categories found: ", paste(invalid, collapse = ", "))
  }

  # --- 8) Dummy encode on 'grp' then rename back to 'mdd_*' prefix ---
  dds_wide <- split_df |>
    dplyr::filter(!is.na(grp)) |>
    fastDummies::dummy_cols(
      select_columns = "grp",
      remove_first_dummy = FALSE,
      split = NULL,
      remove_selected_columns = TRUE
    ) |>
    dplyr::select(-food_item)

  # rename grp_* -> mdd_<life_stage>_*
  dds_wide <- dds_wide |>
    dplyr::rename_with(
      ~ sub("^grp_", paste0(group_col, "_"), .x),
      dplyr::starts_with("grp_")
    )

  # --- 9) Group by ID and take max across food-group dummies ---
  dds_grouped <- dds_wide |>
    dplyr::group_by(!!rlang::sym(id_col)) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::starts_with(paste0(group_col, "_")),
        ~ max(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  # --- 10) Ensure all expected MDD columns exist (fill with 0) ---
  for (cat in expected) {
    colname <- paste0(group_col, "_", cat)
    if (!(colname %in% names(dds_grouped))) {
      dds_grouped[[colname]] <- 0
    }
  }

  # --- 11) Reorder: id + expected cols exactly as listed ---
  dds_grouped <- dds_grouped |>
    dplyr::select(!!rlang::sym(id_col), dplyr::all_of(paste0(group_col, "_", expected)), dplyr::everything())

  # --- 12) Add breastfeeding if child ---
  if (life_stage == "child" && !is.null(breastfeeding_data)) {
    bf <- breastfeeding_data |>
      dplyr::select(!!rlang::sym(id_col), !!rlang::sym(breastfeeding_col))
    dds_grouped <- dds_grouped |>
      dplyr::left_join(bf, by = id_col)
  }

  # --- 13) Compute DDS and MDD (exclude "Others") ---
  dds_col <- paste0("dds_", life_stage)
  mdd_col <- paste0("mdd_", life_stage)
  score_cols <- paste0(group_col, "_", setdiff(expected, "Others"))

  if (life_stage == "child" && !is.null(breastfeeding_col)) {
    sum_cols <- c(score_cols, breastfeeding_col)
    dds_grouped[[dds_col]] <- rowSums(dplyr::select(dds_grouped, dplyr::all_of(sum_cols)), na.rm = TRUE)
    threshold <- 5
  } else if (life_stage == "child") {
    dds_grouped[[dds_col]] <- rowSums(dplyr::select(dds_grouped, dplyr::all_of(score_cols)), na.rm = TRUE)
    threshold <- 4
  } else {
    dds_grouped[[dds_col]] <- rowSums(dplyr::select(dds_grouped, dplyr::all_of(score_cols)), na.rm = TRUE)
    threshold <- 5
  }

  dds_grouped[[mdd_col]] <- ifelse(dds_grouped[[dds_col]] >= threshold, 1, 0)

  return(tibble::as_tibble(janitor::clean_names(dds_grouped)))
}
