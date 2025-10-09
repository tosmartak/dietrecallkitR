#' Compute Dietary Species Richness (DSR)
#'
#' Computes the Dietary Species Richness (DSR) score from cleaned dietary recall data.
#' The function counts the number of distinct species consumed per respondent or recall ID.
#' Users can either use the package FCT database (online CSV) or supply their own mapping file.
#'
#' @param recall_data A data frame containing at least the ID column and the food item column.
#' @param id_col Character. Name of the identifier column in `recall_data`
#'   (e.g., `"survey_id"`).
#' @param food_item_col Character. Column in `recall_data` containing food item names.
#' @param use_fct_db Logical, default = FALSE. If TRUE, uses the package's
#'   online FCT database (`species` column).
#' @param dsr_map_data Optional. A user-provided mapping data frame with
#'   food items and species names. Required if `use_fct_db = FALSE`.
#' @param dsr_map_food Character. Column name in `dsr_map_data` that matches food items.
#' @param dsr_map_col Character. Column name in `dsr_map_data` containing species names.
#' @param add_prefix Optional character. Prefix to add to the output column name (e.g., `"child"` will produce `child_dsr`).
#'
#' @return A tibble with:
#' - The ID column (as provided in `id_col`)
#' - One column for the computed dietary species richness (`dsr` or prefixed name)
#'
#' @details
#' - If `use_fct_db = TRUE`, the function pulls from the online FCT database used in `dietrecallkitR`.
#' - Foods with `"No Species Assignment"` or `NA` species are excluded from the count.
#' - Duplicate species per ID are counted once.
#' - Returns warnings for unmapped items.
#'
#' @examples
#' # Using online FCT DB
#' recall_sample <- data.frame(
#'   survey_id = c(1, 1, 2, 2, 3),
#'   desc_of_food = c(
#'     "Rabbit meat, raw",
#'     "Ghee (cow milk)",
#'     "Cheese, cheddar, regular fat",
#'     "Tuna, grilled",
#'     "Rabbit meat, raw"
#'   )
#' )
#'
#' result <- compute_dsr(
#'   recall_data   = recall_sample,
#'   id_col        = "survey_id",
#'   food_item_col = "desc_of_food",
#'   use_fct_db    = TRUE
#' )
#' print(result)
#'
#' # Using custom mapping
#' species_map <- data.frame(
#'   item = c("Rabbit meat, raw", "Tuna, grilled", "Cheese, cheddar, regular fat", "Ghee (cow milk)"),
#'   species = c("Rabbit", "Tuna", "Cow", "Cow")
#' )
#'
#' result <- compute_dsr(
#'   recall_data   = recall_sample,
#'   id_col        = "survey_id",
#'   food_item_col = "desc_of_food",
#'   use_fct_db    = FALSE,
#'   dsr_map_data  = species_map,
#'   dsr_map_food  = "item",
#'   dsr_map_col   = "species",
#'   add_prefix    = "child"
#' )
#' print(result)
#'
#' @export
compute_dsr <- function(recall_data,
                        id_col,
                        food_item_col,
                        use_fct_db = FALSE,
                        dsr_map_data = NULL,
                        dsr_map_food = NULL,
                        dsr_map_col = NULL,
                        add_prefix = NULL) {
  # --- 1) Validate inputs ---
  stopifnot(is.data.frame(recall_data))
  stopifnot(id_col %in% names(recall_data))
  stopifnot(food_item_col %in% names(recall_data))

  if (use_fct_db && !is.null(dsr_map_data)) {
    stop("You cannot set both `use_fct_db = TRUE` and provide `dsr_map_data`. Choose one.")
  }

  if (!use_fct_db) {
    if (is.null(dsr_map_data) || is.null(dsr_map_food) || is.null(dsr_map_col)) {
      stop("When `use_fct_db = FALSE`, you must provide `dsr_map_data`, `dsr_map_food`, and `dsr_map_col`.")
    }
    stopifnot(is.data.frame(dsr_map_data))
    stopifnot(all(c(dsr_map_food, dsr_map_col) %in% names(dsr_map_data)))
  }

  # --- 2) Load mapping ---
  if (use_fct_db) {
    db_url <- "https://raw.githubusercontent.com/tosmartak/dietrecallkitR/main/data-raw/fct_db.csv"
    raw_map <- suppressMessages(readr::read_csv(db_url, show_col_types = FALSE))

    dsr_map <- raw_map |>
      dplyr::filter(species != "No Species Assignment") |>
      dplyr::select(food_item, species) |>
      dplyr::mutate(
        food_item = stringr::str_trim(food_item),
        species = stringr::str_trim(species)
      )
  } else {
    dsr_map <- dsr_map_data |>
      dplyr::select(
        food_item = !!rlang::sym(dsr_map_food),
        species = !!rlang::sym(dsr_map_col)
      ) |>
      dplyr::mutate(
        food_item = stringr::str_trim(food_item),
        species = stringr::str_trim(species)
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
    dplyr::left_join(dsr_map, by = "food_item")

  # Warn unmapped items
  unmapped <- merged |>
    dplyr::filter(is.na(species)) |>
    dplyr::distinct(food_item)

  if (nrow(unmapped) > 0) {
    warning("Unmapped food items:\n", paste0("- ", unmapped$food_item, collapse = "\n"))
  }

  # --- 5) Handle empty / no mapped foods ---
  if (nrow(merged |> dplyr::filter(!is.na(species))) == 0) {
    warning("No mapped species found. Returning zero DSR for all IDs.")
    out <- recall |>
      dplyr::select(!!rlang::sym(id_col)) |>
      dplyr::distinct()
    out[[ifelse(is.null(add_prefix), "dsr", paste0(add_prefix, "_dsr"))]] <- 0
    return(tibble::as_tibble(out))
  }

  # --- 6) Split semicolon-separated species ---
  merged <- merged |>
    tidyr::separate_rows(species, sep = ";") |>
    dplyr::mutate(species = stringr::str_trim(species)) |>
    dplyr::filter(!is.na(species) & species != "")

  # --- 7) Deduplicate and count species per ID ---
  dsr_result <- merged |>
    dplyr::select(!!rlang::sym(id_col), species) |>
    dplyr::filter(!is.na(species) & species != "") |>
    dplyr::distinct() |>
    dplyr::group_by(!!rlang::sym(id_col)) |>
    dplyr::summarise(dsr = dplyr::n_distinct(species), .groups = "drop")

  # --- 7b) Ensure all IDs appear, even if no valid species ---
  all_ids <- recall |>
    dplyr::select(!!rlang::sym(id_col)) |>
    dplyr::distinct()

  dsr_result <- all_ids |>
    dplyr::left_join(dsr_result, by = id_col) |>
    dplyr::mutate(dsr = dplyr::if_else(is.na(dsr), 0L, dsr))

  # --- 8) Add prefix if specified ---
  if (!is.null(add_prefix)) {
    new_col <- paste0(add_prefix, "_dsr")
    names(dsr_result)[names(dsr_result) == "dsr"] <- new_col
  }

  return(tibble::as_tibble(dsr_result))
}
