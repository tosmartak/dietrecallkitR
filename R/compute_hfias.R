#' Compute Household Food Insecurity Access (HFIA) Score and Category
#'
#' This function computes the HFIA score and categorical classification
#' (1-4) based on the standard HFIAS indicator framework developed by
#' FANTA (Food and Nutrition Technical Assistance Project). The function
#' assumes that the HFIAS-related variables are provided in the exact order
#' they appear in the HFIAS questionnaire (occurrence and frequency pairs).
#'
#' @param data A data frame or tibble containing HFIAS-related variables.
#' @param hfia_cols A character vector specifying the ordered column names
#'   corresponding to the HFIAS questions. These should include both occurrence
#'   (Yes/No) and frequency responses, alternating in sequence.
#'
#' @details
#' The function expects 18 columns for the standard HFIAS tool:
#' nine occurrence questions and nine corresponding frequency-of-occurrence
#' questions, ordered as:
#' \enumerate{
#'   \item Occurrence Q1
#'   \item Frequency Q1
#'   \item Occurrence Q2
#'   \item Frequency Q2
#'   \item ... up to Q9 and its frequency
#' }
#'
#' Expected frequency responses:
#' \itemize{
#'   \item "Rarely (1-2 times)" = 1
#'   \item "Sometimes (3 to 10 times)" = 2
#'   \item "Often (more than 10 times)" = 3
#' }
#' and occurrence responses:
#' \itemize{
#'   \item "Yes" = 1
#'   \item "No" = 0
#' }
#'
#' Numeric inputs are accepted directly (0-3). Missing values are automatically
#' replaced with 0, as frequency questions are typically skipped when occurrence
#' is "No." After conversion, the function checks for any unexpected text entries
#' and raises an informative error listing them for the user to clean.
#'
#' **Outputs:**
#' \itemize{
#'   \item \code{hfia_score} - The total sum of the nine frequency columns.
#'   \item \code{hfia_category} - A categorical classification:
#'     \describe{
#'       \item{1}{Food secure}
#'       \item{2}{Mildly food insecure}
#'       \item{3}{Moderately food insecure}
#'       \item{4}{Severely food insecure}
#'     }
#' }
#'
#' @return A tibble containing the original data with two added columns:
#'   \code{hfia_score} and \code{hfia_category}.
#'
#' @references
#' Coates, J., Swindale, A., & Bilinsky, P. (2007). Household Food Insecurity
#' Access Scale (HFIAS) for Measurement of Food Access: Indicator Guide (v3).
#' Washington, D.C.: Food and Nutrition Technical Assistance Project (FANTA).
#'
#' @examples
#' # Example dataset with respondent ID and realistic skip patterns
#' df <- tibble::tibble(
#'   id = 1:3,
#'   Q1 = c("Yes", "Yes", "No"),
#'   F1 = c("Rarely (1-2 times)", "Often (more than 10 times)", NA),
#'   Q2 = c("No", "Yes", "No"),
#'   F2 = c(NA, "Sometimes (3 to 10 times)", NA),
#'   Q3 = c("No", "Yes", "No"),
#'   F3 = c(NA, "Rarely (1-2 times)", NA),
#'   Q4 = c("No", "Yes", "No"),
#'   F4 = c(NA, "Rarely (1-2 times)", NA),
#'   Q5 = c("No", "Yes", "No"),
#'   F5 = c(NA, "Often (more than 10 times)", NA),
#'   Q6 = c("No", "Yes", "No"),
#'   F6 = c(NA, "Rarely (1-2 times)", NA),
#'   Q7 = c("No", "Yes", "No"),
#'   F7 = c(NA, "Rarely (1-2 times)", NA),
#'   Q8 = c("Yes", "Yes", "No"),
#'   F8 = c("Sometimes (3 to 10 times)", "Rarely (1-2 times)", NA),
#'   Q9 = c("No", "Yes", "No"),
#'   F9 = c(NA, "Rarely (1-2 times)", NA)
#' )
#'
#' # Compute HFIA score and category using the relevant columns only
#' compute_hfias(
#'   df,
#'   hfia_cols = c(
#'     "Q1", "F1", "Q2", "F2", "Q3", "F3", "Q4", "F4",
#'     "Q5", "F5", "Q6", "F6", "Q7", "F7", "Q8", "F8",
#'     "Q9", "F9"
#'   )
#' )
#'
#' @export
compute_hfias <- function(data, hfia_cols) {
  # --- Input validation --------------------------------------------------------
  if (!is.data.frame(data)) stop("`data` must be a data frame or tibble.")
  if (!all(hfia_cols %in% names(data))) {
    missing_cols <- setdiff(hfia_cols, names(data))
    stop(
      "The following columns are missing in `data`: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  if (!is.character(hfia_cols) || length(hfia_cols) < 18) {
    stop("`hfia_cols` must include all 18 HFIAS columns (9 occurrence + 9 frequency).")
  }

  # --- Convert text responses to numeric equivalents --------------------------
  data_converted <- data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(hfia_cols),
        \(x) dplyr::case_when(
          x %in% c("Yes", "yes") ~ 1L,
          x %in% c("No", "no") ~ 0L,
          x == "Rarely (1-2 times)" ~ 1L,
          x == "Sometimes (3 to 10 times)" ~ 2L,
          x == "Often (more than 10 times)" ~ 3L,
          TRUE ~ suppressWarnings(as.integer(x))
        )
      )
    )

  # --- Post-conversion integrity check ----------------------------------------
  non_int_entries <- purrr::map_dfr(hfia_cols, \(col) {
    raw_vals <- data[[col]]
    num_vals <- data_converted[[col]]
    bad_idx <- which(is.na(num_vals) & !is.na(raw_vals))
    invalid <- unique(raw_vals[bad_idx])

    if (length(invalid) > 0) {
      tibble::tibble(column = col, unexpected_values = invalid)
    } else {
      tibble::tibble(column = character(), unexpected_values = character())
    }
  })

  if (nrow(non_int_entries) > 0) {
    stop(
      "Unexpected non-numeric or unrecognized text values found in the following columns:\n",
      paste0(
        apply(non_int_entries, 1, \(x) {
          paste0("* ", x["column"], ": ", x["unexpected_values"])
        }),
        collapse = "\n"
      ),
      "\nPlease clean these entries before passing to compute_hfias()."
    )
  }

  # --- Replace NAs with 0 after validation ------------------------------------
  data <- data_converted |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(hfia_cols),
        \(x) tidyr::replace_na(as.integer(x), 0L)
      )
    )

  # --- Compute HFIA score (sum of frequency columns) --------------------------
  freq_indices <- seq(2, length(hfia_cols), by = 2)
  data <- data |>
    dplyr::mutate(
      hfia_score = rowSums(dplyr::across(hfia_cols[freq_indices]), na.rm = TRUE)
    )

  # --- Compute HFIA category based on FANTA classification --------------------
  data <- data |>
    dplyr::mutate(
      hfia_category = dplyr::case_when(
        # Food Secure
        data[[hfia_cols[2]]] %in% c(0, 1) &
          data[[hfia_cols[3]]] == 0 & data[[hfia_cols[5]]] == 0 &
          data[[hfia_cols[7]]] == 0 & data[[hfia_cols[9]]] == 0 &
          data[[hfia_cols[11]]] == 0 & data[[hfia_cols[13]]] == 0 &
          data[[hfia_cols[15]]] == 0 & data[[hfia_cols[17]]] == 0 ~ 1L,

        # Mildly Food Insecure
        data[[hfia_cols[2]]] %in% c(2, 3) |
          data[[hfia_cols[4]]] %in% c(1, 2, 3) |
          data[[hfia_cols[6]]] == 1 |
          (data[[hfia_cols[8]]] == 1 & data[[hfia_cols[9]]] == 0 &
            data[[hfia_cols[11]]] == 0 & data[[hfia_cols[13]]] == 0 &
            data[[hfia_cols[15]]] == 0 & data[[hfia_cols[17]]] == 0) ~ 2L,

        # Moderately Food Insecure
        data[[hfia_cols[6]]] %in% c(2, 3) |
          data[[hfia_cols[8]]] %in% c(2, 3) |
          data[[hfia_cols[10]]] %in% c(1, 2) |
          (data[[hfia_cols[12]]] %in% c(1, 2) & data[[hfia_cols[13]]] == 0 &
            data[[hfia_cols[15]]] == 0 & data[[hfia_cols[17]]] == 0) ~ 3L,

        # Severely Food Insecure
        data[[hfia_cols[10]]] == 3 |
          data[[hfia_cols[12]]] == 3 |
          data[[hfia_cols[14]]] %in% c(1, 2, 3) |
          data[[hfia_cols[16]]] %in% c(1, 2, 3) |
          data[[hfia_cols[18]]] %in% c(1, 2, 3) ~ 4L,
        TRUE ~ 0L
      )
    )

  tibble::as_tibble(data)
}
