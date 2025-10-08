#' Internal: Iron probability of inadequacy reference (Allen et al. 2006)
#'
#' Provides probability lookup tables for Iron intake adequacy
#' based on life group and dietary bioavailability (5%, 10%, 15%).
#'
#' @return A nested list structure, where each top-level element corresponds
#'   to a specific subgroup (e.g., "Child_1_3", "Menstruating_Women").
#'   Each subgroup contains data frames of probability, intake range,
#'   and bioavailability level.
#' @keywords internal
.iron_pni_reference <- function() {
  # Helper to simplify data frame creation
  make_tbl <- function(prob, min_vals, max_vals) {
    data.frame(
      prob = prob,
      min = min_vals,
      max = max_vals
    )
  }

  list(
    # -----------------------------------------------------------------
    # 1. CHILDREN 1–3 years
    # -----------------------------------------------------------------
    "Child_1_3" = list(
      "low" = make_tbl(
        prob = c(1.00, 0.96, 0.93, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.15, 0.08, 0.04, 0.00),
        min_vals = c(-Inf, 3.6, 4.5, 5.5, 7.1, 8.3, 9.6, 10.8, 12.2, 13.8, 15.8, 18.9, 21.8, 24.5),
        max_vals = c(3.6, 4.5, 5.5, 7.1, 8.3, 9.6, 10.8, 12.2, 13.8, 15.8, 18.9, 21.8, 24.5, Inf)
      ),
      "moderate" = make_tbl(
        prob = c(1.00, 0.96, 0.93, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.15, 0.08, 0.04, 0.00),
        min_vals = c(-Inf, 1.8, 2.3, 2.8, 3.6, 4.2, 4.8, 5.4, 6.1, 6.9, 7.9, 9.5, 10.9, 12.3),
        max_vals = c(1.8, 2.3, 2.8, 3.6, 4.2, 4.8, 5.4, 6.1, 6.9, 7.9, 9.5, 10.9, 12.3, Inf)
      ),
      "high" = make_tbl(
        prob = c(1.00, 0.96, 0.93, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.15, 0.08, 0.04, 0.00),
        min_vals = c(-Inf, 1.3, 1.5, 1.8, 2.4, 2.8, 3.2, 3.6, 4.1, 4.6, 5.3, 6.3, 7.3, 8.2),
        max_vals = c(1.3, 1.5, 1.8, 2.4, 2.8, 3.2, 3.6, 4.1, 4.6, 5.3, 6.3, 7.3, 8.2, Inf)
      )
    ),

    # -----------------------------------------------------------------
    # 2. CHILDREN 4–8 years
    # -----------------------------------------------------------------
    "Child_4_8" = list(
      "low" = make_tbl(
        prob = c(1.00, 0.96, 0.93, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.15, 0.08, 0.04, 0.00),
        min_vals = c(-Inf, 4.8, 5.9, 7.4, 9.5, 11.3, 13.0, 14.8, 16.7, 19.0, 21.9, 26.3, 30.4, 34.3),
        max_vals = c(4.8, 5.9, 7.4, 9.5, 11.3, 13.0, 14.8, 16.7, 19.0, 21.9, 26.3, 30.4, 34.3, Inf)
      ),
      "moderate" = make_tbl(
        prob = c(1.00, 0.96, 0.93, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.15, 0.08, 0.04, 0.00),
        min_vals = c(-Inf, 2.4, 3.0, 3.7, 4.8, 5.7, 6.5, 7.4, 8.4, 9.5, 11.0, 13.2, 15.2, 17.2),
        max_vals = c(2.4, 3.0, 3.7, 4.8, 5.7, 6.5, 7.4, 8.4, 9.5, 11.0, 13.2, 15.2, 17.2, Inf)
      ),
      "high" = make_tbl(
        prob = c(1.00, 0.96, 0.93, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.15, 0.08, 0.04, 0.00),
        min_vals = c(-Inf, 1.6, 2.0, 2.4, 3.2, 3.8, 4.3, 4.9, 5.6, 6.3, 7.3, 8.8, 9.5, 10.9),
        max_vals = c(1.6, 2.0, 2.4, 3.2, 3.8, 4.3, 4.9, 5.6, 6.3, 7.3, 8.8, 9.5, 10.9, Inf)
      )
    ),

    # -----------------------------------------------------------------
    # 3. FEMALES 14–18 years
    # -----------------------------------------------------------------
    "Female_14_18" = list(
      "low" = make_tbl(
        prob = c(1.00, 0.96, 0.93, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.15, 0.08, 0.04, 0.00),
        min_vals = c(-Inf, 16.2, 17.7, 19.6, 22.1, 24.1, 26.0, 27.8, 29.7, 32.1, 35.2, 40.4, 45.9, 51.8),
        max_vals = c(16.2, 17.7, 19.6, 22.1, 24.1, 26.0, 27.8, 29.7, 32.1, 35.2, 40.4, 45.9, 51.8, Inf)
      ),
      "moderate" = make_tbl(
        prob = c(1.00, 0.96, 0.93, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.15, 0.08, 0.04, 0.00),
        min_vals = c(-Inf, 8.1, 8.8, 9.8, 11.1, 12.0, 13.0, 13.9, 14.8, 16.1, 17.6, 20.2, 23.0, 25.9),
        max_vals = c(8.1, 8.8, 9.8, 11.1, 12.0, 13.0, 13.9, 14.8, 16.1, 17.6, 20.2, 23.0, 25.9, Inf)
      ),
      "high" = make_tbl(
        prob = c(1.00, 0.96, 0.93, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.15, 0.08, 0.04, 0.00),
        min_vals = c(-Inf, 5.4, 5.9, 6.5, 7.4, 8.0, 8.7, 9.3, 9.9, 10.7, 11.7, 13.5, 15.3, 17.3),
        max_vals = c(5.4, 5.9, 6.5, 7.4, 8.0, 8.7, 9.3, 9.9, 10.7, 11.7, 13.5, 15.3, 17.3, Inf)
      )
    ),

    # -----------------------------------------------------------------
    # 4. MENSTRUATING WOMEN (Adult 19–50 years)
    # -----------------------------------------------------------------
    "Menstruating_Women" = list(
      "low" = make_tbl(
        prob = c(1.00, 0.96, 0.93, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.15, 0.08, 0.04, 0.00),
        min_vals = c(-Inf, 15.0, 16.7, 18.7, 21.4, 23.6, 25.7, 27.8, 30.2, 33.2, 37.3, 45.0, 53.5, 63.0),
        max_vals = c(15.0, 16.7, 18.7, 21.4, 23.6, 25.7, 27.8, 30.2, 33.2, 37.3, 45.0, 53.5, 63.0, Inf)
      ),
      "moderate" = make_tbl(
        prob = c(1.00, 0.96, 0.93, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.15, 0.08, 0.04, 0.00),
        min_vals = c(-Inf, 7.5, 8.4, 9.4, 10.7, 11.8, 12.9, 13.9, 15.1, 16.6, 18.7, 22.5, 26.7, 31.5),
        max_vals = c(7.5, 8.4, 9.4, 10.7, 11.8, 12.9, 13.9, 15.1, 16.6, 18.7, 22.5, 26.7, 31.5, Inf)
      ),
      "high" = make_tbl(
        prob = c(1.00, 0.96, 0.93, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.15, 0.08, 0.04, 0.00),
        min_vals = c(-Inf, 5.0, 5.6, 6.2, 7.1, 7.9, 8.6, 9.3, 10.1, 11.1, 12.4, 15.0, 17.8, 21.0),
        max_vals = c(5.0, 5.6, 6.2, 7.1, 7.9, 8.6, 9.3, 10.1, 11.1, 12.4, 15.0, 17.8, 21.0, Inf)
      )
    )
  )
}
#' Internal: Retrieve iron probability of inadequacy for an individual intake
#'
#' Uses the reference probability tables from Allen et al. (2006)
#' to determine the probability of inadequacy based on usual iron intake,
#' age group, and dietary bioavailability.
#'
#' @param intake Numeric vector of usual iron intakes (mg/day).
#' @param age Numeric vector of ages (in years for women, months for children).
#' @param life_group Character scalar: "Child", "Female", etc.
#' @param bioavailability Character scalar: one of "low", "moderate", or "high".
#' @return Numeric vector of probabilities (0–1), same length as `intake`.
#' @keywords internal
.get_iron_pni <- function(intake, age, life_group, bioavailability) {
  # Validate inputs -------------------------------------------------------------
  if (!is.numeric(intake)) stop("`intake` must be numeric.")
  if (!is.numeric(age)) stop("`age` must be numeric.")

  ref <- .iron_pni_reference()

  # Determine which subgroup applies -------------------------------------------
  subgroup <- dplyr::case_when(
    life_group == "Child" & age >= 12 & age <= 47 ~ "Child_1_3", # 12–47 months
    life_group == "Child" & age >= 48 & age <= 95 ~ "Child_4_8", # 48–95 months
    life_group == "Female" & age >= 14 & age <= 18 ~ "Female_14_18",
    life_group == "Female" & age > 18 & age <= 50 ~ "Menstruating_Women",
    TRUE ~ NA_character_
  )

  # Initialize result
  probs <- rep(NA_real_, length(intake))

  # For each observation -------------------------------------------------------
  for (i in seq_along(intake)) {
    sg <- subgroup[i]
    if (is.na(sg)) {
      probs[i] <- NA_real_
      next
    }

    ref_tbl <- ref[[sg]][[bioavailability]]

    # Match intake within range
    match_row <- ref_tbl[which(intake[i] >= ref_tbl$min & intake[i] < ref_tbl$max), ]

    if (nrow(match_row) == 1) {
      probs[i] <- match_row$prob
    } else {
      # If no match (e.g., below/above range), assign 1 or 0 respectively
      if (!is.na(intake[i])) {
        below_min <- intake[i] < min(ref_tbl$min)
        above_max <- intake[i] > max(ref_tbl$max)

        if (below_min) {
          probs[i] <- 1
        }
        if (above_max) {
          probs[i] <- 0
        }
        if (!below_min && !above_max) {
          probs[i] <- NA_real_
        }
      } else {
        probs[i] <- NA_real_
      }
    }
  }

  return(probs)
}
