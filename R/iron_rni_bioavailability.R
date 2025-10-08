#' Internal: Iron RNI values adjusted for bioavailability
#'
#' Returns RNI values (mg/day) for iron across life groups, adjusted by
#' bioavailability (low = 5%, moderate = 10%, high = 15%).
#'
#' @keywords internal
.iron_rni_bioavailability <- function() {
  list(
    "Child" = list(
      age_ranges = list(c(7, 12), c(13, 47), c(48, 95)), # months
      values = list(
        "low"      = c(11.6, 11.6, 12.6),
        "moderate" = c(5.8, 5.8, 6.3),
        "high"     = c(3.9, 3.9, 4.2)
      )
    ),
    "Female" = list(
      age_ranges = list(c(14, 18), c(19, 50), c(51, 70), c(71, Inf)),
      values = list(
        "low"      = c(49.0, 58.8, 26.2, 26.2),
        "moderate" = c(24.5, 29.4, 13.1, 13.1),
        "high"     = c(16.3, 19.6, 8.7, 8.7)
      )
    ),
    "Male" = list(
      age_ranges = list(c(14, 18), c(19, 50), c(51, 70), c(71, Inf)),
      values = list(
        "low"      = c(30.4, 27.4, 27.4, 27.4),
        "moderate" = c(16.7, 13.7, 13.7, 13.7),
        "high"     = c(12.1, 9.1, 9.1, 9.1)
      )
    ),
    "Pregnant" = list(
      age_ranges = list(c(14, 50)),
      values = list(
        "low"      = 51.0,
        "moderate" = 51.0,
        "high"     = 51.0
      )
    ),
    "Lactating" = list(
      age_ranges = list(c(14, 50)),
      values = list(
        "low"      = 30.0,
        "moderate" = 15.0,
        "high"     = 10.0
      )
    )
  )
}
#' Internal: Get iron RNI for a specific age, life group, and bioavailability
#' @keywords internal
.get_iron_rni <- function(age, life_group, bioavailability) {
  if (!is.numeric(age)) stop("`age` must be numeric.")

  iron_ref <- .iron_rni_bioavailability()
  grp <- iron_ref[[life_group]]
  if (is.null(grp)) {
    return(NA_real_)
  }

  age <- floor(age)
  for (i in seq_along(grp$age_ranges)) {
    r <- grp$age_ranges[[i]]
    if (age >= r[1] && age <= r[2]) {
      return(grp$values[[bioavailability]][i])
    }
  }
  return(NA_real_)
}
