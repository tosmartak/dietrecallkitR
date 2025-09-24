# data-raw/make_non_gram_foods_conversion.R
# Script to generate a realistic example of non_gram_foods_conversion
# based on dietrecall_example and get_non_gram_foods() logic.
# Run once from repo root with: source("data-raw/make_non_gram_foods_conversion.R")

library(dplyr)
library(stringr)

# Load packaged example
data("dietrecall_example", package = "dietrecallkit")

maintable <- dietrecall_example$maintable
food_details <- dietrecall_example$food_details
food_ingredients_group <- dietrecall_example$food_ingredients_group

location_col <- "subcounty" # keep consistent with dietrecall_example

# --- food_details ---
df1 <- food_details %>%
  filter(
    !is.na(desc_of_food),
    !unit_qty_food_consumed %in% c("g from scale", "g from photobook")
  ) %>%
  left_join(
    maintable %>% select(survey_id, !!sym(location_col)),
    by = "survey_id"
  ) %>%
  select(
    !!sym(location_col),
    food_item = desc_of_food,
    unit = unit_qty_food_consumed
  )

# --- food_ingredients_group ---
df2 <- food_ingredients_group %>%
  filter(!food_ingredient_unit %in% c("g from scale", "g from photobook")) %>%
  left_join(
    maintable %>% select(survey_id, !!sym(location_col)),
    by = "survey_id"
  ) %>%
  select(
    !!sym(location_col),
    food_item = food_ingredients_used,
    unit = food_ingredient_unit
  )

# --- Combine + clean ---
non_gram_foods_conversion <- bind_rows(df1, df2) %>%
  mutate(food_item = str_trim(food_item)) %>%
  distinct(!!sym(location_col), food_item, unit, .keep_all = FALSE) %>% # keep only unique combos
  mutate(
    amount = 1, # dummy example
    gram = sample(c(50, 100, 200), n(), replace = TRUE) # simple ratio
  )

# Save for use in tests
usethis::use_data(non_gram_foods_conversion, overwrite = TRUE)

message("non_gram_foods_conversion created and saved to data/")
