# scripts/03_compute_gdr_prototype.R
# Prototype to compute Global Dietary Recommendation (GDR)

library(dplyr)
library(readr)
library(stringr)
library(fastDummies)
library(tidyr)
library(janitor)


# ---- 1) Example recall_data ----
data("dietrecall_example", package = "dietrecallkit")

recall_data <- dietrecall_example$food_details %>%
  filter(!is.na(desc_of_food)) %>%
  select(survey_id, food_item = desc_of_food)

# ---- 2) Load mapping (from fct_db) ----
db_url <- "https://raw.githubusercontent.com/tosmartak/dietrecallkitR/main/data-raw/fct_db.csv"

gdr_map <- suppressMessages(read_csv(db_url, show_col_types = FALSE)) %>%
  filter(gdr_category != "No GDR Assignment") %>%
  select(food_item, gdr = gdr_category) %>%
  mutate(
    food_item = str_trim(food_item),
    gdr = str_trim(gdr)
  )

# ---- 3) Merge recall with mapping ----
merged <- recall_data %>%
  left_join(gdr_map, by = "food_item")

# ---- 4) Split semicolon-separated categories ----
merged <- merged %>%
  mutate(
    gdr = str_split(gdr, ";")
  ) %>%
  unnest(gdr) %>%
  mutate(gdr = str_to_title(str_trim(gdr)))

# ---- 5) Validation of categories ----
GDRplus <- c(
  "Whole Grains", "Legumes", "Nuts And Seeds",
  "Vitamin A-Rich Orange Vegetables, Roots And Tubers",
  "Dark Green Leafy Vegetables", "Other Vegetables",
  "Vitamin A-Rich Fruits", "Citrus Fruits", "Other Fruits"
)

GDRminus <- c(
  "Sodas/Sugar-Sweetened Beverages", "Baked/Grain-Based Sweets", "Other Sweets",
  "Processed Meat", "Unprocessed Red Meat", "Deep-Fried Foods",
  "Fast Food And Instant Noodles", "Packaged Ultra-Processed Salty Snacks"
)

valid_categories <- c(GDRplus, GDRminus)

invalid <- merged %>%
  filter(!is.na(gdr), !gdr %in% valid_categories) %>%
  distinct(gdr)

if (nrow(invalid) > 0) {
  stop("Invalid GDR categories found: ", paste(invalid$gdr, collapse = ", "))
}

# ---- 6) Dummy encode ----
gdr_wide <- merged %>%
  filter(!is.na(gdr)) %>%
  fastDummies::dummy_cols(
    select_columns = "gdr",
    remove_first_dummy = FALSE,
    split = NULL,
    remove_selected_columns = TRUE
  ) %>%
  select(-food_item)

# ---- 7) Group by ID ----
gdr_grouped <- gdr_wide %>%
  group_by(survey_id) %>%
  summarise(across(starts_with("gdr_"), ~ max(.x, na.rm = TRUE)), .groups = "drop")

# ---- 8) Ensure all expected GDR columns exist ----
all_expected <- paste0("gdr_", valid_categories)
for (col in all_expected) {
  if (!(col %in% names(gdr_grouped))) {
    gdr_grouped[[col]] <- 0
  }
}

# ---- 9) Reorder columns: id_col + valid_categories ----
gdr_grouped <- gdr_grouped %>%
  select(survey_id, all_of(all_expected))

# ---- 10) Special rule: Processed Meat = 2 if consumed ----
gdr_grouped <- gdr_grouped %>%
  mutate(`gdr_Processed Meat` = ifelse(`gdr_Processed Meat` == 1, 2, 0))

# ---- 11) Compute scores ----
gdr_grouped <- gdr_grouped %>%
  mutate(
    gdrplus   = rowSums(across(all_of(paste0("gdr_", GDRplus))), na.rm = TRUE),
    gdrminus  = rowSums(across(all_of(paste0("gdr_", GDRminus))), na.rm = TRUE),
    gdr_score = gdrplus - gdrminus + 9
  ) %>%
  clean_names()

# ---- Final output ----
print(gdr_grouped, width = Inf)

