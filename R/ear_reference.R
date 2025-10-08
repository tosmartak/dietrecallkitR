#' --- Internal reference table (can be reused by other functions) ---
#' @keywords internal
.ear_reference_table <- function() {
  list(
    "Female" = list(
      age_ranges = list(c(9, 13), c(14, 18), c(19, 30), c(31, 50), c(51, 70), c(71, Inf)),
      values = list(
        "Calcium" = c(1100, 1100, 800, 800, 1000, 1000),
        "CHO" = c(100, 100, 100, 100, 100, 100),
        "Protein" = c(0.76, 0.71, 0.66, 0.66, 0.66, 0.66),
        "Vitamin A" = c(420, 485, 500, 500, 500, 500),
        "Vitamin C" = c(39, 56, 60, 60, 60, 60),
        "Vitamin D" = c(10, 10, 10, 10, 10, 10),
        "Vitamin E" = c(9, 12, 12, 12, 12, 12),
        "Thiamin" = c(0.7, 0.9, 0.9, 0.9, 0.9, 0.9),
        "Riboflavin" = c(0.8, 0.9, 0.9, 0.9, 0.9, 0.9),
        "Niacin" = c(9, 11, 11, 11, 11, 11),
        "Vitamin B6" = c(0.8, 1.0, 1.1, 1.1, 1.3, 1.3),
        "Folate" = c(250, 330, 320, 320, 320, 320),
        "Vitamin B12" = c(1.5, 2.0, 2.0, 2.0, 2.0, 2.0),
        "Copper" = c(540, 685, 700, 700, 700, 700),
        "Iodine" = c(73, 95, 95, 95, 95, 95),
        "Iron" = c(5.7, 7.9, 8.1, 8.1, 5.0, 5.0),
        "Magnesium" = c(200, 300, 255, 265, 265, 265),
        "Molybdenum" = c(26, 33, 34, 34, 34, 34),
        "Phosphorus" = c(1055, 1055, 580, 580, 580, 580),
        "Selenium" = c(35, 45, 45, 45, 45, 45),
        "Zinc" = c(7.0, 7.3, 6.8, 6.8, 6.8, 6.8)
      )
    ),
    "Male" = list(
      age_ranges = list(c(9, 13), c(14, 18), c(19, 30), c(31, 50), c(51, 70), c(71, Inf)),
      values = list(
        "Calcium" = c(1100, 1100, 800, 800, 800, 1000),
        "CHO" = c(100, 100, 100, 100, 100, 100),
        "Protein" = c(0.76, 0.73, 0.66, 0.66, 0.66, 0.66),
        "Vitamin A" = c(445, 630, 625, 625, 625, 625),
        "Vitamin C" = c(39, 63, 75, 75, 75, 75),
        "Vitamin D" = c(10, 10, 10, 10, 10, 10),
        "Vitamin E" = c(9, 12, 12, 12, 12, 12),
        "Thiamin" = c(0.7, 1.0, 1.0, 1.0, 1.0, 1.0),
        "Riboflavin" = c(0.8, 1.1, 1.1, 1.1, 1.1, 1.1),
        "Niacin" = c(9, 12, 12, 12, 12, 12),
        "Vitamin B6" = c(0.8, 1.1, 1.1, 1.1, 1.4, 1.4),
        "Folate" = c(250, 330, 320, 320, 320, 320),
        "Vitamin B12" = c(1.5, 2.0, 2.0, 2.0, 2.0, 2.0),
        "Copper" = c(540, 685, 700, 700, 700, 700),
        "Iodine" = c(73, 95, 95, 95, 95, 95),
        "Iron" = c(5.9, 7.7, 6.0, 6.0, 6.0, 6.0),
        "Magnesium" = c(200, 340, 330, 350, 350, 350),
        "Molybdenum" = c(26, 33, 34, 34, 34, 34),
        "Phosphorus" = c(1055, 1055, 580, 580, 580, 580),
        "Selenium" = c(35, 45, 45, 45, 45, 45),
        "Zinc" = c(7.0, 8.5, 9.4, 9.4, 9.4, 9.4)
      )
    ),
    "Child" = list(
      age_ranges = list(c(0, 6), c(7, 12), c(13, 47), c(48, 96)),
      values = list(
        "Calcium" = c(NA, NA, 500, 800),
        "CHO" = c(NA, NA, 100, 100),
        "Protein" = c(NA, 1.0, 0.87, 0.76),
        "Vitamin A" = c(NA, NA, 210, 275),
        "Vitamin C" = c(NA, NA, 13, 22),
        "Vitamin D" = c(NA, NA, 10, 10),
        "Vitamin E" = c(NA, NA, 5, 6),
        "Thiamin" = c(NA, NA, 0.4, 0.5),
        "Riboflavin" = c(NA, NA, 0.4, 0.5),
        "Niacin" = c(NA, NA, 5, 6),
        "Vitamin B6" = c(NA, NA, 0.4, 0.5),
        "Folate" = c(NA, NA, 120, 160),
        "Vitamin B12" = c(NA, NA, 0.7, 1.0),
        "Copper" = c(NA, NA, 260, 340),
        "Iodine" = c(NA, NA, 65, 65),
        "Iron" = c(NA, 6.9, 3.0, 4.1),
        "Magnesium" = c(NA, NA, 65, 110),
        "Molybdenum" = c(NA, NA, 13, 17),
        "Phosphorus" = c(NA, NA, 380, 405),
        "Selenium" = c(NA, NA, 17, 23),
        "Zinc" = c(NA, 2.5, 2.5, 4.0)
      )
    ),
    "Pregnant" = list(
      age_ranges = list(c(14, 18), c(19, 30), c(31, 50)),
      values = list(
        "Calcium" = c(1000, 800, 800),
        "CHO" = c(135, 135, 135),
        "Protein" = c(0.88, 0.88, 0.88),
        "Vitamin A" = c(530, 550, 550),
        "Vitamin C" = c(66, 70, 70),
        "Vitamin D" = c(10, 10, 10),
        "Vitamin E" = c(12, 12, 12),
        "Thiamin" = c(1.2, 1.2, 1.2),
        "Riboflavin" = c(1.2, 1.2, 1.2),
        "Niacin" = c(14, 14, 14),
        "Vitamin B6" = c(1.6, 1.6, 1.6),
        "Folate" = c(520, 520, 520),
        "Vitamin B12" = c(2.2, 2.2, 2.2),
        "Copper" = c(785, 800, 800),
        "Iodine" = c(160, 160, 160),
        "Iron" = c(23, 22, 22),
        "Magnesium" = c(335, 290, 300),
        "Molybdenum" = c(40, 40, 40),
        "Phosphorus" = c(1055, 580, 580),
        "Selenium" = c(49, 49, 49),
        "Zinc" = c(10.5, 9.5, 9.5)
      )
    ),
    "Lactating" = list(
      age_ranges = list(c(14, 18), c(19, 30), c(31, 50)),
      values = list(
        "Calcium" = c(1000, 800, 800),
        "CHO" = c(160, 160, 160),
        "Protein" = c(1.05, 1.05, 1.05),
        "Vitamin A" = c(885, 900, 900),
        "Vitamin C" = c(96, 100, 100),
        "Vitamin D" = c(10, 10, 10),
        "Vitamin E" = c(16, 16, 16),
        "Thiamin" = c(1.2, 1.2, 1.2),
        "Riboflavin" = c(1.3, 1.3, 1.3),
        "Niacin" = c(13, 13, 13),
        "Vitamin B6" = c(1.7, 1.7, 1.7),
        "Folate" = c(450, 450, 450),
        "Vitamin B12" = c(2.4, 2.4, 2.4),
        "Copper" = c(985, 1000, 1000),
        "Iodine" = c(209, 209, 209),
        "Iron" = c(7, 6.5, 6.5),
        "Magnesium" = c(300, 255, 265),
        "Molybdenum" = c(35, 36, 36),
        "Phosphorus" = c(1055, 580, 580),
        "Selenium" = c(59, 59, 59),
        "Zinc" = c(10.9, 10.4, 10.4)
      )
    )
  )
}
#' Internal: Retrieve EAR for a specific nutrient, age, and life group
#'
#' Looks up EAR value from the internal reference table based on age and life group.
#'
#' @param age Numeric. Age in years (for adults) or months (for children).
#' @param life_group Character scalar. One of "Female", "Male", "Child", "Pregnant", "Lactating".
#' @param nutrient Character scalar. Nutrient name as it appears in the reference table.
#'
#' @return Numeric EAR value for the given life group and nutrient. Returns NA if not found or out of range.
#' @keywords internal
.get_ear <- function(age, life_group, nutrient) {
  # Validate life group
  ear_ref <- .ear_reference_table()
  grp <- ear_ref[[life_group]]
  if (is.null(grp)) {
    return(NA_real_)
  }

  # Handle missing or invalid age
  if (is.na(age) || !is.numeric(age)) {
    return(NA_real_)
  }

  # Floor age to handle decimal-year input
  age <- floor(age)

  # Match to age range
  for (i in seq_along(grp$age_ranges)) {
    r <- grp$age_ranges[[i]]

    # Handle open-ended range safely
    if (is.infinite(r[2])) {
      if (!is.na(age) && age >= r[1]) {
        return(grp$values[[nutrient]][i])
      }
    } else {
      if (!is.na(age) && age >= r[1] && age <= r[2]) {
        return(grp$values[[nutrient]][i])
      }
    }
  }

  # Return NA if not within any defined range
  return(NA_real_)
}
