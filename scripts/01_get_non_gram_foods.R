# scripts/01_unique_foods.R
# Prototype script to extract unique non-gram food items

library(dplyr)
library(stringr)

# ---- 1) Load example data (for testing) ----
data("dietrecall_example", package = "dietrecallkit")

maintable <- dietrecall_example$maintable
food_details <- dietrecall_example$food_details
food_ingredients_group <- dietrecall_example$food_ingredients_group

# ---- 2) Process food_details ----
df1 <- food_details %>%
  filter(
    (food_preparation_place == "Outside Home" | ready_to_eat == 1),
    !unit_qty_food_consumed %in% c("g from scale", "g from photobook")
  ) %>%
  left_join(maintable %>% select(survey_id, subcounty), by = "survey_id") %>%
  select(subcounty, food_item = food_item_selected, unit = unit_qty_food_consumed)

# ---- 3) Process food_ingredients_group ----
df2 <- food_ingredients_group %>%
  filter(!food_ingredient_unit %in% c("g from scale", "g from photobook")) %>%
  left_join(maintable %>% select(survey_id, subcounty), by = "survey_id") %>%
  select(subcounty, food_item = food_ingredients_used, unit = food_ingredient_unit)

# ---- 4) Combine + clean ----
combined <- bind_rows(df1, df2) %>%
  mutate(food_item = str_trim(food_item)) %>%
  distinct(subcounty, food_item, unit)

# ---- 5) Add extra columns ----
final <- combined %>%
  mutate(
    amount = NA_real_,
    gram = NA_real_
    )