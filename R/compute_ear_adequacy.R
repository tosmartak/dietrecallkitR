#' Compute Nutrient Adequacy Based on EAR Cutoff Method
#'
#' This function computes nutrient adequacy using the Estimated Average Requirement (EAR)
#' cutoff method for individuals based on their age and specified life-stage group.
#' It references Dietary Reference Intake (DRI) EAR values from the Institute of Medicine (IOM)
#' and applies the cut-point approach to determine whether each individual meets or fails
#' the EAR for each nutrient.
#'
#' @param data A data frame containing age and nutrient intake data.
#' @param age_col Character string. Column name for age (in years or months depending on life group).
#' @param life_group Character string indicating the life-stage group.
#'   Must be one of `"Female"`, `"Male"`, `"Child"`, `"Pregnant"`, or `"Lactating"`.
#' @param nutrients A named character vector where names are nutrient names (e.g. `"Protein"`)
#'   and values are corresponding column names in the dataset.
#' @param include_ear_values Logical. If `TRUE`, includes EAR value columns in the output.
#'   Defaults to `FALSE`.
#'
#' @details
#' For each individual, the function:
#' 1. Looks up the EAR based on the provided age and life-stage group.
#' 2. Compares the individual’s intake to that EAR.
#' 3. Returns adequacy = 1 if intake ≥ EAR, otherwise 0.
#'
#' The function currently supports these nutrients:
#' Calcium, CHO, Protein, Vitamin A, Vitamin C, Vitamin D, Vitamin E,
#' Thiamin, Riboflavin, Niacin, Vitamin B6, Folate, Vitamin B12, Copper,
#' Iodine, Iron, Magnesium, Molybdenum, Phosphorus, Selenium, Zinc.
#'
#' Each nutrient produces two additional columns: one for EAR values and one for
#' adequacy indicators (1/0), each prefixed with the life group for clarity.
#' (e.g. `Female_Protein_g_ear`, `Female_Protein_g_adequacy`)
#'
#' @note
#' Ensure that the age column is expressed in **years** for adults and **months** for children,
#' as EAR reference values are age-unit specific.
#'
#' @return A tibble containing the original data plus adequacy columns.
#' If \code{include_ear_values = TRUE}, EAR columns are also included.
#'
#' @references
#' Institute of Medicine (US). Dietary Reference Intakes Tables, Appendix J. 2006.
#' https://www.ncbi.nlm.nih.gov/books/NBK545442/
#'
#' @examples
#' df <- tibble::tibble(
#'   age = c(10, 25, 35, 45),
#'   Protein_g = c(1.2, 0.7, 0.6, 0.8),
#'   Calcium_mg = c(900, 850, 700, 950)
#' )
#' nutrients <- c("Protein" = "Protein_g", "Calcium" = "Calcium_mg")
#' compute_ear_adequacy(df, age_col = "age", life_group = "Female", nutrients = nutrients)
#'
#' @export
compute_ear_adequacy <- function(data,
                                 age_col,
                                 life_group,
                                 nutrients,
                                 include_ear_values = FALSE) {
  # Normalize input arguments
  life_group <- stringr::str_to_title(life_group)

  # --- 1) Validate inputs ---
  if (!is.data.frame(data)) stop("`data` must be a data.frame.")
  if (!is.character(age_col) || length(age_col) != 1) {
    stop("`age_col` must be a single column name.")
  }
  if (!age_col %in% names(data)) {
    stop("`age_col` not found in `data`.")
  }
  if (!is.character(life_group) || !(life_group %in% c("Female", "Male", "Child", "Pregnant", "Lactating"))) {
    stop("`life_group` must be one of 'Female', 'Male', 'Child', 'Pregnant', or 'Lactating'.")
  }
  if (!is.character(nutrients) || is.null(names(nutrients))) {
    stop("`nutrients` must be a named character vector (e.g. c('Protein' = 'Protein_g')).")
  }

  # --- 2) Validate nutrient columns existence ---
  missing_cols <- setdiff(unname(nutrients), names(data))
  if (length(missing_cols) > 0) {
    stop(paste0("The following nutrient columns are missing in `data`: ", paste(missing_cols, collapse = ", ")))
  }

  # --- 3) Validate nutrient names (lookup support) ---
  ear_ref <- .ear_reference_table()
  supported_nutrients <- names(ear_ref[[life_group]]$values)
  invalid_nutrients <- setdiff(names(nutrients), supported_nutrients)
  if (length(invalid_nutrients) > 0) {
    stop(
      paste0(
        "Unsupported nutrient(s): ", paste(invalid_nutrients, collapse = ", "),
        ".\nSupported nutrients for ", life_group, " are: ",
        paste(supported_nutrients, collapse = ", "), "."
      )
    )
  }

  # --- 4) Helper function to get EAR ---
  get_ear_value <- function(age, nutrient) {
    age <- floor(age) # ensure completed years logic
    grp <- ear_ref[[life_group]]
    for (i in seq_along(grp$age_ranges)) {
      r <- grp$age_ranges[[i]]
      if (is.infinite(r[2])) {
        if (age >= r[1]) {
          return(grp$values[[nutrient]][i])
        }
      } else {
        if (age >= r[1] && age <= r[2]) {
          return(grp$values[[nutrient]][i])
        }
      }
    }
    return(NA_real_)
  }

  # --- 5) Compute EAR and adequacy ---
  age_vals <- data[[age_col]]
  for (nutrient in names(nutrients)) {
    nutrient_col <- nutrients[[nutrient]]
    ear_vec <- vapply(age_vals, get_ear_value, numeric(1), nutrient = nutrient)

    ear_col <- paste0(life_group, "_", nutrient_col, "_ear")
    adequacy_col <- paste0(life_group, "_", nutrient_col, "_adequacy")

    intake_vals <- data[[nutrient_col]]

    adequacy_vec <- rep(NA_real_, length(intake_vals))
    valid_idx <- which(!is.na(intake_vals) & !is.na(ear_vec))
    adequacy_vec[valid_idx] <- as.numeric(intake_vals[valid_idx] >= ear_vec[valid_idx])

    if (include_ear_values) data[[ear_col]] <- ear_vec
    data[[adequacy_col]] <- adequacy_vec
  }

  return(tibble::as_tibble(data))
}
