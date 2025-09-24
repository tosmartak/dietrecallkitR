# R/globals.R assuming these variables are the ones with issues.
utils::globalVariables(c(
  "desc_of_food", "qty_food_consumed", "unit_qty_food_consumed",
  "food_item_price_prop_consumed", "food_item", "unit",
  "gram_per_unit", "prop_consumed", "food_details_rowid",
  "food_ingredients_used", "food_ingredient_amt", "food_ingredient_unit",
  "food_ingredient_price_prop_used", "amt_of_food_cooked", "gram_intake",
  "amt_consumed", "actual_gram_intake", "gram", "amount"
))
