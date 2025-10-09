#' Internal: Zinc RNI values adjusted for bioavailability
#'
#' Returns RNI values (mg/day) for zinc across life groups, adjusted by
#' bioavailability (low = 5%, moderate = 10%, high = 15%).
#'
#' @keywords internal
.zinc_rni_bioavailability <- function() {
  list(
    "Child" = list(
      age_ranges = list(c(12, 47), c(48, 83)), # 1-3 years and 4 - 6 years
      values = list(
        "low"      = c(8.3, 9.6),
        "moderate" = c(4.1, 4.8),
        "high"     = c(2.4, 2.9)
      )
    ),
    "Female" = list(
      age_ranges = list(c(19, 50)),
      values = list(
        "low"      = 9.8,
        "moderate" = 4.9,
        "high"     = 3.0
      )
    ),
    "Male" = list(
      age_ranges = list(c(19, 50)),
      values = list(
        "low"      = 14.0,
        "moderate" = 7.0,
        "high"     = 4.2
      )
    ),
    "Pregnant" = list(
      age_ranges = list(c(14, 50)),
      values = list(
        "low"      = 14.0,
        "moderate" = 7.0,
        "high"     = 4.2
      )
    ),
    "Lactating" = list(
      age_ranges = list(c(14, 50)),
      values = list(
        "low"      = 19.0,
        "moderate" = 9.5,
        "high"     = 5.8
      )
    )
  )
}
#' Internal: Get zinc RNI for a specific age, life group, and bioavailability
#' @keywords internal
.get_zinc_rni <- function(age, life_group, bioavailability) {
  if (!is.numeric(age)) stop("`age` must be numeric.")

  zinc_ref <- .zinc_rni_bioavailability()
  grp <- zinc_ref[[life_group]]
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
