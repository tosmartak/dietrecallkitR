#' Internal: Zinc EAR values adjusted for bioavailability
#'
#' Returns a nested list of zinc EARs (mg/day) based on life group and bioavailability.
#' Bioavailability levels: "low" (5%), "moderate" (10%), "high" (15%).
#' Age is assumed to be in years for adults and months for children.
#'
#' @keywords internal
.zinc_ear_bioavailability <- function() {
  list(
    "Child" = list(
      age_ranges = list(c(12, 47), c(48, 83)), # 1–3y, 4–6y in months
      values = list(
        "low"       = c(6.9, 8.0),
        "moderate"  = c(3.4, 4.0),
        "high"      = c(2.0, 2.4)
      )
    ),
    "Female" = list(
      age_ranges = list(c(19, 50)),
      values = list(
        "low"       = 8.2,
        "moderate"  = 4.1,
        "high"      = 2.5
      )
    ),
    "Male" = list(
      age_ranges = list(c(19, 50)),
      values = list(
        "low"       = 11.7,
        "moderate"  = 5.8,
        "high"      = 3.5
      )
    ),
    "Pregnant" = list(
      age_ranges = list(c(14, 50)), # covers all pregnant stages
      values = list(
        "low"       = 11.7,
        "moderate"  = 5.8,
        "high"      = 3.5
      )
    ),
    "Lactating" = list(
      age_ranges = list(c(14, 50)), # covers early lactation
      values = list(
        "low"       = 15.8,
        "moderate"  = 7.9,
        "high"      = 4.8
      )
    )
  )
}
#' Internal: Get zinc EAR for a specific age, life group, and bioavailability
#' @keywords internal
.get_zinc_ear <- function(age, life_group, bioavailability) {
  if (!is.numeric(age)) stop("`age` must be numeric.")

  zinc_ref <- .zinc_ear_bioavailability()
  grp <- zinc_ref[[life_group]]
  if (is.null(grp)) {
    return(NA_real_)
  }

  # Floor to avoid decimal-year misclassification
  age <- floor(age)

  for (i in seq_along(grp$age_ranges)) {
    r <- grp$age_ranges[[i]]
    if (age >= r[1] && age <= r[2]) {
      return(grp$values[[bioavailability]][i])
    }
  }
  return(NA_real_)
}
