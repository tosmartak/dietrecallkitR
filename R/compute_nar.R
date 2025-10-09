#' Compute Nutrient Adequacy Ratio (NAR) and Mean Adequacy Ratio (MAR)
#'
#' This function calculates nutrient adequacy ratios (NARs) for individual nutrients
#' and the mean adequacy ratio (MAR) across multiple nutrients using the
#' Recommended Nutrient Intake (RNI) as reference values. It accommodates
#' bioavailability adjustments for zinc and iron and automatically matches
#' RNI values based on age and life-stage group.
#'
#' @description
#' The Nutrient Adequacy Ratio (NAR) represents the adequacy of an individual's
#' usual intake relative to the Recommended Nutrient Intake (RNI):
#'
#' \deqn{NAR = min(Usual Intake / RNI, 1)}
#'
#' The Mean Adequacy Ratio (MAR) is the average of NARs across multiple nutrients:
#'
#' \deqn{MAR = (Σ NAR_i) / n}
#'
#' This approach is widely used to assess the overall diet quality of individuals
#' or population subgroups. Values are capped at 1 to prevent high intakes of one
#' nutrient from compensating for inadequacies in others.
#'
#' @param data A data frame or tibble containing dietary intake and age data.
#' @param age_col Character scalar. Name of the column representing the individual's age
#'   (in years for adults and months for children).
#' @param life_group Character scalar specifying the life-stage group.
#'   Must be one of `"Female"`, `"Male"`, `"Child"`, `"Pregnant"`, or `"Lactating"`.
#' @param nutrients A named character vector mapping nutrient names to their
#'   corresponding column names in the dataset.
#'   Example: `c("Protein" = "Protein_g", "Iron" = "Iron_mg")`
#' @param bioavailability Character scalar specifying the assumed dietary
#'   bioavailability for iron and zinc. Must be one of `"low"`, `"moderate"`, or `"high"`.
#'   Defaults to `"moderate"`.
#' @param include_rni_values Logical. If `TRUE`, the RNI columns are included in the
#'   output dataset. Defaults to `TRUE`.
#'
#' @details
#' **How RNIs Are Determined:**
#' - For general nutrients (e.g., Vitamin A, vitamin C, folate etc), RNIs are retrieved
#'   from the internal `.rni_reference()` table based on `life_group` and `age`.
#' - The function currently supports these nutrients: Vitamin A, Vitamin C, Vitamin D, Vitamin E,
#'   Thiamin, Riboflavin, Niacin, Vitamin B6, Folate, Vitamin B12, Calcium, Selenium, Iodine, Copper,
#'   Magnesium, Molybdenum, Phosphorus, Sodium, Potassium, Iron (bioavailability must be selected), Zinc (bioavailability must be selected).
#' - For **zinc** and **iron**, bioavailability-adjusted RNIs are obtained using
#'   `.get_zinc_rni()` and `.get_iron_rni()`, respectively.
#'
#' **Units:**
#' Ensure that the intake units in your dataset correspond to those used in the RNI
#' reference values (e.g., mg/day, µg/day, g/day).
#'
#' **Mean Adequacy Ratio (MAR):**
#' - Computed as the mean of all nutrient adequacy ratios per individual.
#' - The column name includes the life group prefix
#'   (e.g., `"Female_mean_adequacy_ratio"`).
#'
#' @return
#' A tibble containing:
#' - The original dataset columns.
#' - Computed NAR columns for each nutrient (e.g., `Female_Protein_g_adequacy_ratio`).
#' - Optional RNI columns for each nutrient (if `include_rni_values = TRUE`).
#' - A summary column representing the life group–specific MAR
#'   (e.g., `Female_mean_adequacy_ratio`).
#'
#' @examples
#' df <- tibble::tibble(
#'   age = c(10, 25, 35),
#'   Calcium_g = c(1000, 500, 800),
#'   Iron_mg = c(7, 12, 9)
#' )
#'
#' nutrients <- c("Calcium" = "Calcium_g", "Iron" = "Iron_mg")
#'
#' compute_nar(
#'   data = df,
#'   age_col = "age",
#'   life_group = "Female",
#'   nutrients = nutrients,
#'   bioavailability = "moderate",
#'   include_rni_values = TRUE
#' )
#'
#' @references
#' FAO/WHO (2002). *Human Vitamin and Mineral Requirements*.
#' Allen, L. et al. (2006). *An Interactive 24-Hour Recall for Assessing the Adequacy of Iron and Zinc Intakes in Developing Countries*. HarvestPlus Technical Monograph 8.
#' Institute of Medicine (2006). *Dietary Reference Intakes: The Essential Guide to Nutrient Requirements*.
#'
#' @export
compute_nar <- function(data,
                        age_col,
                        life_group,
                        nutrients,
                        bioavailability = "moderate",
                        include_rni_values = TRUE) {
  # Normalize input arguments
  bioavailability <- tolower(bioavailability)
  life_group <- stringr::str_to_title(life_group)

  # ---------------------------------------------------------------------------
  # 1. Input validation
  # ---------------------------------------------------------------------------
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!age_col %in% names(data)) stop("`age_col` not found in `data`.")
  if (!life_group %in% c("Female", "Male", "Child", "Pregnant", "Lactating")) {
    stop("Invalid `life_group`. Must be one of: Female, Male, Child, Pregnant, Lactating.")
  }
  if (!bioavailability %in% c("low", "moderate", "high")) {
    stop("Invalid `bioavailability`. Must be 'low', 'moderate', or 'high'.")
  }
  if (length(nutrients) == 0 || is.null(names(nutrients))) {
    stop("`nutrients` must be a named character vector (e.g., c('Protein' = 'Protein_g')).")
  }

  df <- tibble::as_tibble(data)
  age_vals <- df[[age_col]]

  # ---------------------------------------------------------------------------
  # 2. Loop through each nutrient to compute RNI and NAR
  # ---------------------------------------------------------------------------
  for (nutrient_name in names(nutrients)) {
    nutrient_col <- nutrients[[nutrient_name]]
    intake_vals <- df[[nutrient_col]]

    rni_col <- paste0(life_group, "_", nutrient_col, "_rni")
    nar_col <- paste0(life_group, "_", nutrient_col, "_adequacy_ratio")

    # Retrieve RNI values
    if (tolower(nutrient_name) == "zinc") {
      rni_vals <- vapply(age_vals, function(a) {
        val <- tryCatch(.get_zinc_rni(a, life_group, bioavailability), error = function(e) NA_real_)
        if (is.null(val) || length(val) == 0 || !is.finite(val)) NA_real_ else val
      }, numeric(1))
    } else if (tolower(nutrient_name) == "iron") {
      rni_vals <- vapply(age_vals, function(a) {
        val <- tryCatch(.get_iron_rni(a, life_group, bioavailability), error = function(e) NA_real_)
        if (is.null(val) || length(val) == 0 || !is.finite(val)) NA_real_ else val
      }, numeric(1))
    } else {
      rni_vals <- vapply(age_vals, function(a) {
        val <- tryCatch(.get_rni(a, life_group, nutrient_name), error = function(e) NA_real_)
        if (is.null(val) || length(val) == 0 || !is.finite(val)) NA_real_ else val
      }, numeric(1))
    }

    df[[rni_col]] <- rni_vals

    # Compute NAR (capped at 1)
    df[[nar_col]] <- ifelse(
      is.na(intake_vals) | is.na(rni_vals),
      NA_real_,
      pmin(intake_vals / rni_vals, 1)
    )
  }

  # ---------------------------------------------------------------------------
  # 3. Compute Mean Adequacy Ratio (MAR)
  # ---------------------------------------------------------------------------
  nar_cols <- grep("_adequacy_ratio$", names(df), value = TRUE)
  mar_col <- paste0(life_group, "_mean_adequacy_ratio")
  df[[mar_col]] <- rowMeans(df[nar_cols], na.rm = TRUE)

  # ---------------------------------------------------------------------------
  # 4. Drop RNI columns if not requested
  # ---------------------------------------------------------------------------
  if (!include_rni_values) {
    df <- df[, !(names(df) %in% grep("_rni$", names(df), value = TRUE))]
  }

  return(df)
}
