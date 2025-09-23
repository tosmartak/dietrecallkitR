# scripts/02_compute_actual_g_intake.R
# Prototype to compute actual gram intake from dietary recall Excel + non-gram conversions

library(dplyr)
library(readxl)
library(stringr)

# ---- 1) Load files ----
filepath <- file.path("data-raw", "dietary_recall_full.xlsx")
non_gram_file <- file.path("data-raw", "non_gram_foods_conversion_data.xlsx")

maintable <- read_excel(filepath, sheet = "maintable")
food_details <- read_excel(filepath, sheet = "food_details")
food_ingredients <- read_excel(filepath, sheet = "food_ingredients_group")

# Non-gram foods conversion (user completed) – may or may not exist
if (file.exists(non_gram_file)) {
  non_gram_foods <- read_excel(non_gram_file, sheet = 1) %>%
    mutate(gram_per_unit = gram / amount)
} else {
  non_gram_foods <- NULL
}

# ---- 2) Check for non-gram units if no conversion file ----
units_fd <- unique(food_details$unit_qty_food_consumed)
units_fig <- unique(food_ingredients$food_ingredient_unit)
unique_units <- unique(c(units_fd, units_fig))

banned_units <- c("g from scale", "g from photobook")
other_units <- setdiff(unique_units, banned_units)

if (is.null(non_gram_foods) && length(other_units) > 0) {
  stop(
    "Non-gram units found: ", paste(other_units, collapse = ", "),
    ". Please provide the non-gram foods conversion sheet."
  )
}

# ---- 3) Validate subcounty consistency if conversion file is provided ----
if (!is.null(non_gram_foods)) {
  subcounty_main <- sort(unique(maintable$subcounty))
  subcounty_non <- sort(unique(non_gram_foods$subcounty))
  
  if (!identical(subcounty_main, subcounty_non)) {
    stop(
      "Mismatch in subcounty values between maintable and non_gram_foods.\n",
      "Maintable subcounties: ", paste(subcounty_main, collapse = ", "), "\n",
      "Non-gram foods subcounties: ", paste(subcounty_non, collapse = ", ")
    )
  }
}

# ---- 4) Process food_details ----
fd_clean <- food_details %>%
  filter(!is.na(desc_of_food)) %>%
  left_join(maintable %>% select(survey_id, subcounty), by = "survey_id") %>%
  select(
    survey_id,
    subcounty,
    food_item = desc_of_food,
    amt_consumed = qty_food_consumed,
    unit = unit_qty_food_consumed,
    prop_consumed = food_item_price_prop_consumed
  )

# Merge gram_per_unit (if available)
if (!is.null(non_gram_foods)) {
  fd_clean <- fd_clean %>%
    left_join(non_gram_foods %>% select(subcounty, food_item, unit, gram_per_unit),
              by = c("subcounty", "food_item", "unit"))
} else {
  fd_clean$gram_per_unit <- NA_real_
}

# ---- Check for missing conversions ----
missing_conv <- fd_clean %>%
  filter(!unit %in% c("g from scale", "g from photobook"),
         is.na(gram_per_unit)) %>%
  distinct(food_item, unit)

if (nrow(missing_conv) > 0) {
  warning(
    "Some non-gram foods do not have gram_per_unit assigned:\n",
    paste0("- ", missing_conv$food_item, " [", missing_conv$unit, "]", collapse = "\n")
  )
}

fd_clean <- fd_clean %>%
  mutate(
    prop_consumed = ifelse(is.na(prop_consumed), 1, prop_consumed),
    actual_gram_intake = dplyr::case_when(
      unit %in% banned_units ~ amt_consumed,
      TRUE ~ amt_consumed * gram_per_unit * prop_consumed
    )
  )

# ---- 5) Process food_ingredients_group ----
fig_clean <- food_ingredients %>%
  left_join(maintable %>% select(survey_id, subcounty), by = "survey_id") %>%
  select(
    survey_id,
    food_details_rowid,
    subcounty,
    food_item = food_ingredients_used,
    amt_consumed = food_ingredient_amt,
    unit = food_ingredient_unit,
    prop_consumed = food_ingredient_price_prop_used
  ) %>%
  # Add cooking context from raw food_details
  left_join(food_details %>% select(
    survey_id,
    food_details_rowid,
    amt_of_food_cooked,
    qty_food_consumed
  ), by = c("survey_id", "food_details_rowid"))

# Merge gram_per_unit (if available)
if (!is.null(non_gram_foods)) {
  fig_clean <- fig_clean %>%
    left_join(non_gram_foods %>% select(subcounty, food_item, unit, gram_per_unit),
              by = c("subcounty", "food_item", "unit"))
} else {
  fig_clean$gram_per_unit <- NA_real_
}

# ---- Check for missing conversions ----
missing_conv <- fig_clean %>%
  filter(!unit %in% c("g from scale", "g from photobook"),
         is.na(gram_per_unit)) %>%
  distinct(food_item, unit)

if (nrow(missing_conv) > 0) {
  warning(
    "Some non-gram foods do not have gram_per_unit assigned:\n",
    paste0("- ", missing_conv$food_item, " [", missing_conv$unit, "]", collapse = "\n")
  )
}

fig_clean <- fig_clean %>%
  mutate(
    prop_consumed = ifelse(is.na(prop_consumed), 1, prop_consumed),
    gram_intake = dplyr::case_when(
      unit %in% banned_units ~ amt_consumed,
      TRUE ~ amt_consumed * gram_per_unit * prop_consumed
    ),
    actual_gram_intake = (gram_intake * qty_food_consumed) / amt_of_food_cooked
  )

# ---- 6) Combine cleaned datasets ----
final <- bind_rows(
  fd_clean %>%
    select(survey_id, food_item, amt_consumed, unit,
           prop_consumed, gram_per_unit, actual_gram_intake),
  fig_clean %>%
    select(survey_id, food_item, amt_consumed, unit,
           prop_consumed, gram_per_unit, actual_gram_intake)
)

print(final, n = 20)
