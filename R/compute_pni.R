#' Compute Probability of Nutrient Inadequacy (PNI)
#'
#' This function estimates the probability of nutrient inadequacy (PNI) for each
#' individual using the probability approach, following Institute of Medicine (IOM)
#' and Allen et al. (2006) guidelines. It combines EAR-based probability estimates
#' for general nutrients with bioavailability-adjusted reference values for zinc and
#' lookup-based probabilities for iron. Pregnant and lactating groups are not covered in Allen et al. (2006)
#' because their requirement distributions are not represented by the same model.
#'
#' @param data A data frame or tibble containing age and nutrient intake data.
#' @param age_col Character string. Column name for age (years for adults, months for children).
#' @param life_group Character string specifying the life-stage group.
#'   Must be one of `"Female"`, `"Male"`, `"Child"`, `"Pregnant"`, or `"Lactating"`.
#' @param nutrients A named character vector, where names are nutrient names
#'   (e.g. `"Protein"`) and values are corresponding column names in the dataset.
#' @param bioavailability Character string specifying dietary bioavailability
#'   for zinc and iron: one of `"low"`, `"moderate"`, or `"high"`. Defaults to `"moderate"`.
#' @param cv Numeric. Coefficient of variation (CV) used to approximate the
#'   standard deviation of the requirement distribution as a proportion of the
#'   EAR (i.e., SD = cv × EAR). Defaults to 0.1, following the convention
#'   applied by Allen et al. (2006) and IOM probability method assumptions.
#'   A smaller CV assumes less variability in nutrient requirements, while a
#'   larger CV increases the spread and thus changes the computed inadequacy
#'   probabilities.
#' @param include_ear_values Logical. If `TRUE`, includes nutrient EAR columns in the output.
#'
#' @details
#' For each nutrient:
#' - **General nutrients** use the normal probability approach:
#'   \deqn{PNI = 1 - Φ((intake - EAR) / (0.1 × EAR))}
#' - **Zinc** uses EAR values adjusted for bioavailability (FAO/WHO 2002).
#' - **Iron** uses skewed, lookup-based probability tables from Allen et al. (2006).
#'
#' The function currently supports these nutrients:
#' Calcium, CHO, Protein, Vitamin A, Vitamin C, Vitamin D, Vitamin E,
#' Thiamin, Riboflavin, Niacin, Vitamin B6, Folate, Vitamin B12, Copper,
#' Iodine, Iron (bioavailability must be selected), Magnesium, Molybdenum, Phosphorus, Selenium, Zinc (bioavailability must be selected).
#'
#' Each nutrient produces two columns: one for EAR values and one for PNI,
#' each prefixed with the life group (e.g., `Female_Protein_g_ear`, `Female_Protein_g_pni`).
#'
#' @return A tibble containing the original data plus the computed PNI and optionally
#' the EAR values for each nutrient.
#'
#' @references
#' Institute of Medicine (US). Dietary Reference Intakes Tables. 2006.
#' Allen, L. et al. (2006). An interactive 24-hour recall for assessing the adequacy
#' of iron and zinc intakes in developing countries. HarvestPlus Technical Monograph 8.
#'
#' @examples
#' df <- tibble::tibble(age = c(25, 10, 30), Protein_g = c(0.7, 0.6, 0.8))
#' nutrients <- c("Protein" = "Protein_g")
#' compute_pni(df, "age", "Female", nutrients)
#'
#' @export
compute_pni <- function(data,
                        age_col,
                        life_group,
                        nutrients,
                        bioavailability = "moderate",
                        cv = 0.1,
                        include_ear_values = TRUE) {
  # Normalize input arguments
  bioavailability <- tolower(bioavailability)
  life_group <- stringr::str_to_title(life_group)

  # ---------------------------------------------------------------------------
  # 1. Input validation
  # ---------------------------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.")
  }
  if (nrow(data) == 0) {
    warning("`data` has 0 rows. Returning input unchanged.")
    return(tibble::as_tibble(data))
  }
  if (!is.character(age_col) || length(age_col) != 1) {
    stop("`age_col` must be a single column name.")
  }
  if (!age_col %in% names(data)) {
    stop("`age_col` not found in `data`.")
  }
  if (!is.character(life_group) || length(life_group) != 1) {
    stop("`life_group` must be a single character value.")
  }
  if (!life_group %in% c("Female", "Male", "Child", "Pregnant", "Lactating")) {
    stop("`life_group` must be one of: 'Female', 'Male', 'Child', 'Pregnant', or 'Lactating'.")
  }
  if (!bioavailability %in% c("low", "moderate", "high")) {
    stop("`bioavailability` must be one of: 'low', 'moderate', or 'high'.")
  }
  if (!is.character(nutrients) || is.null(names(nutrients))) {
    stop("`nutrients` must be a named character vector (e.g., c('Protein' = 'Protein_g')).")
  }

  missing_cols <- setdiff(unname(nutrients), names(data))
  if (length(missing_cols) > 0) {
    stop(paste0("The following nutrient columns are missing in `data`: ", paste(missing_cols, collapse = ", ")))
  }

  # Validate nutrient names (excluding Iron and Zinc)
  general_nutrients <- setdiff(names(nutrients), c("Iron", "Zinc"))
  if (length(general_nutrients) > 0) {
    ear_ref <- .ear_reference_table()
    supported_nutrients <- names(ear_ref[[life_group]]$values)
    invalid_nutrients <- setdiff(general_nutrients, supported_nutrients)
    if (length(invalid_nutrients) > 0) {
      stop(
        paste0(
          "Unsupported nutrient(s): ", paste(invalid_nutrients, collapse = ", "),
          ".\nSupported nutrients for ", life_group, " are: ",
          paste(supported_nutrients, collapse = ", "), "."
        )
      )
    }
  }

  df <- tibble::as_tibble(data)
  age_vals <- df[[age_col]]

  # ---------------------------------------------------------------------------
  # 2. Loop through each nutrient
  # ---------------------------------------------------------------------------
  for (nutrient_name in names(nutrients)) {
    nutrient_col <- nutrients[[nutrient_name]]
    intake_vals <- df[[nutrient_col]]

    ear_col <- paste0(life_group, "_", nutrient_col, "_ear")
    pni_col <- paste0(life_group, "_", nutrient_col, "_pni")

    # Initialize output columns
    df[[ear_col]] <- NA_real_
    df[[pni_col]] <- NA_real_

    # -------------------------------------------------------------------------
    # Case 1: Iron – Use lookup-based probability (Allen et al. 2006)
    # -------------------------------------------------------------------------
    if (tolower(nutrient_name) == "iron") {
      df[[pni_col]] <- mapply(function(x, a) {
        .get_iron_pni(
          intake = x,
          age = a,
          life_group = life_group,
          bioavailability = bioavailability
        )
      }, intake_vals, age_vals)
      next
    }

    # -------------------------------------------------------------------------
    # Case 2: Zinc – Use bioavailability-specific EAR + + probability approach
    # -------------------------------------------------------------------------
    if (tolower(nutrient_name) == "zinc") {
      ears <- vapply(age_vals, function(a) {
        .get_zinc_ear(a, life_group, bioavailability)
      }, numeric(1))

      df[[ear_col]] <- ears
      df[[pni_col]] <- ifelse(
        is.na(intake_vals) | is.na(ears),
        NA_real_,
        1 - stats::pnorm(intake_vals, mean = ears, sd = cv * ears)
      )
      next
    }

    # -------------------------------------------------------------------------
    # Case 3: Other nutrients – Use general EAR reference + probability approach
    # -------------------------------------------------------------------------
    ears <- vapply(age_vals, function(a) {
      .get_ear(a, life_group, nutrient_name)
    }, numeric(1))

    df[[ear_col]] <- ears
    df[[pni_col]] <- ifelse(
      is.na(intake_vals) | is.na(ears),
      NA_real_,
      1 - stats::pnorm(intake_vals, mean = ears, sd = cv * ears)
    )
  }

  # ---------------------------------------------------------------------------
  # 3. Drop EAR columns if not requested
  # ---------------------------------------------------------------------------
  if (!include_ear_values) {
    df <- df[, !(names(df) %in% grep("_ear$", names(df), value = TRUE))]
  }

  return(df)
}
