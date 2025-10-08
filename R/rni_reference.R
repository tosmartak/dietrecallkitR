#' Internal: RNI Reference Table
#'
#' Provides Recommended Nutrient Intake (RNI) values by life group and age range.
#' Bioavailability for zinc and iron is handled separately.
#' @keywords internal
.rni_reference_table <- function() {
  list(
    "Female" = list(
      age_ranges = list(c(9, 13), c(14, 18), c(19, 30), c(31, 50), c(51, 70), c(71, Inf)),
      values = list(
        "Vitamin A" = c(600, 700, 700, 700, 700, 700),
        "Vitamin C" = c(45, 65, 75, 75, 75, 75),
        "Vitamin D" = c(15, 15, 15, 15, 15, 20),
        "Vitamin E" = c(11, 15, 15, 15, 15, 15),
        "Thiamin" = c(0.9, 1.0, 1.1, 1.1, 1.1, 1.1),
        "Riboflavin" = c(0.9, 1.0, 1.1, 1.1, 1.1, 1.1),
        "Niacin" = c(12, 14, 14, 14, 14, 14),
        "Vitamin B6" = c(1.0, 1.2, 1.3, 1.3, 1.5, 1.5),
        "Folate" = c(300, 400, 400, 400, 400, 400),
        "Vitamin B12" = c(1.8, 2.4, 2.4, 2.4, 2.4, 2.4),
        "Calcium" = c(1300, 1300, 1000, 1000, 1200, 1200),
        "Selenium" = c(40, 55, 55, 55, 55, 55),
        "Iodine" = c(120, 150, 150, 150, 150, 150),
        "Copper" = c(700, 890, 900, 900, 900, 900),
        "Magnesium" = c(240, 360, 310, 320, 320, 320),
        "Molybdenum" = c(34, 43, 45, 45, 45, 45),
        "Phosphorus" = c(1250, 1250, 700, 700, 700, 700),
        "Sodium" = c(1200, 1500, 1500, 1500, 1500, 1500),
        "Potassium" = c(2300, 2300, 2600, 2600, 2600, 2600)
      )
    ),
    "Male" = list(
      age_ranges = list(c(9, 13), c(14, 18), c(19, 30), c(31, 50), c(51, 70), c(71, Inf)),
      values = list(
        "Vitamin A" = c(600, 900, 900, 900, 900, 900),
        "Vitamin C" = c(45, 75, 90, 90, 90, 90),
        "Vitamin D" = c(15, 15, 15, 15, 15, 20),
        "Vitamin E" = c(11, 15, 15, 15, 15, 15),
        "Thiamin" = c(0.9, 1.2, 1.2, 1.2, 1.2, 1.2),
        "Riboflavin" = c(0.9, 1.3, 1.3, 1.3, 1.3, 1.3),
        "Niacin" = c(12, 16, 16, 16, 16, 16),
        "Vitamin B6" = c(1.0, 1.3, 1.3, 1.3, 1.7, 1.7),
        "Folate" = c(300, 400, 400, 400, 400, 400),
        "Vitamin B12" = c(1.8, 2.4, 2.4, 2.4, 2.4, 2.4),
        "Calcium" = c(1300, 1300, 1000, 1000, 1000, 1200),
        "Selenium" = c(40, 55, 55, 55, 55, 55),
        "Iodine" = c(120, 150, 150, 150, 150, 150),
        "Copper" = c(700, 890, 900, 900, 900, 900),
        "Magnesium" = c(240, 410, 400, 420, 420, 420),
        "Molybdenum" = c(34, 43, 45, 45, 45, 45),
        "Phosphorus" = c(1250, 1250, 700, 700, 700, 700),
        "Sodium" = c(1200, 1500, 1500, 1500, 1500, 1500),
        "Potassium" = c(2500, 3000, 3400, 3400, 3400, 3400)
      )
    ),
    "Child" = list(
      age_ranges = list(c(0, 6), c(7, 12), c(13, 47), c(48, 96)),
      values = list(
        "Vitamin A" = c(NA, 500, 300, 400),
        "Vitamin C" = c(NA, 50, 15, 25),
        "Vitamin D" = c(NA, 10, 15, 15),
        "Vitamin E" = c(NA, 5, 6, 7),
        "Thiamin" = c(NA, 0.3, 0.5, 0.6),
        "Riboflavin" = c(NA, 0.4, 0.5, 0.6),
        "Niacin" = c(NA, 4, 6, 8),
        "Vitamin B6" = c(NA, 0.3, 0.5, 0.6),
        "Folate" = c(NA, 80, 150, 200),
        "Vitamin B12" = c(NA, 0.5, 0.9, 1.2),
        "Calcium" = c(NA, 260, 700, 1000),
        "Selenium" = c(NA, 20, 20, 30),
        "Iodine" = c(NA, 130, 90, 90),
        "Copper" = c(NA, 220, 340, 440),
        "Magnesium" = c(NA, 75, 80, 130),
        "Molybdenum" = c(NA, 3, 17, 22),
        "Phosphorus" = c(NA, 275, 460, 500),
        "Sodium" = c(NA, 370, 800, 1000),
        "Potassium" = c(NA, 860, 2000, 2300)
      )
    ),
    "Pregnant" = list(
      age_ranges = list(c(14, 18), c(19, 30), c(31, 50)),
      values = list(
        "Vitamin A" = c(750, 770, 770),
        "Vitamin C" = c(80, 85, 85),
        "Vitamin D" = c(15, 15, 15),
        "Vitamin E" = c(15, 15, 15),
        "Thiamin" = c(1.4, 1.4, 1.4),
        "Riboflavin" = c(1.4, 1.4, 1.4),
        "Niacin" = c(18, 18, 18),
        "Vitamin B6" = c(1.9, 1.9, 1.9),
        "Folate" = c(600, 600, 600),
        "Vitamin B12" = c(2.6, 2.6, 2.6),
        "Calcium" = c(1300, 1000, 1000),
        "Selenium" = c(60, 60, 60),
        "Iodine" = c(220, 220, 220),
        "Copper" = c(1000, 1000, 1000),
        "Magnesium" = c(400, 350, 360),
        "Molybdenum" = c(50, 50, 50),
        "Phosphorus" = c(1250, 700, 700),
        "Sodium" = c(1500, 1500, 1500),
        "Potassium" = c(2600, 2900, 2900)
      )
    ),
    "Lactating" = list(
      age_ranges = list(c(14, 18), c(19, 30), c(31, 50)),
      values = list(
        "Vitamin A" = c(1200, 1300, 1300),
        "Vitamin C" = c(115, 120, 120),
        "Vitamin D" = c(15, 15, 15),
        "Vitamin E" = c(19, 19, 19),
        "Thiamin" = c(1.4, 1.4, 1.4),
        "Riboflavin" = c(1.6, 1.6, 1.6),
        "Niacin" = c(17, 17, 17),
        "Vitamin B6" = c(2.0, 2.0, 2.0),
        "Folate" = c(500, 500, 500),
        "Vitamin B12" = c(2.8, 2.8, 2.8),
        "Calcium" = c(1300, 1000, 1000),
        "Selenium" = c(70, 70, 70),
        "Iodine" = c(290, 290, 290),
        "Copper" = c(1300, 1300, 1300),
        "Magnesium" = c(360, 310, 320),
        "Molybdenum" = c(50, 50, 50),
        "Phosphorus" = c(1250, 700, 700),
        "Sodium" = c(1500, 1500, 1500),
        "Potassium" = c(2500, 2800, 2800)
      )
    )
  )
}
#' Internal: Retrieve RNI for a specific nutrient, age, and life group
#' @keywords internal
.get_rni <- function(age, life_group, nutrient) {
  rni_ref <- .rni_reference_table()
  grp <- rni_ref[[life_group]]
  if (is.null(grp)) {
    return(NA_real_)
  }
  if (is.na(age) || !is.numeric(age)) {
    return(NA_real_)
  }

  age <- floor(age)
  for (i in seq_along(grp$age_ranges)) {
    r <- grp$age_ranges[[i]]
    val_vec <- grp$values[[nutrient]]
    if (is.null(val_vec) || length(val_vec) < i) next

    val_i <- val_vec[i]
    if (is.na(val_i)) next

    if (is.infinite(r[2])) {
      if (age >= r[1]) {
        return(val_i)
      }
    } else if (age >= r[1] && age <= r[2]) {
      return(val_i)
    }
  }
  return(NA_real_)
}
