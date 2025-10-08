# scripts/04_compute_gdqs_prototype.R
# Prototype: Compute Global Dietary Quality Score (GDQS)

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(janitor)
library(tibble)

# ---- 1) Simulated recall_data ----
set.seed(123) # reproducible values

recall_data <- tibble::tibble(
  survey_id = 1:4,
  food_item = c(
    "Meat Samosa",
    "Biscuits and Cookies, Savoury",
    "Tangerine (sandara), juice",
    "Rabbit meat, stewed"
  ),
  actual_gram_intake = c(50, 30, 200, 120) # some sample gram intakes
)

# ---- 2) Load mapping ----
db_url <- "https://raw.githubusercontent.com/tosmartak/dietrecallkitR/main/data-raw/fct_db.csv"
gdqs_map <- suppressMessages(read_csv(db_url, show_col_types = FALSE)) %>%
  filter(gdqs_category != "No GDQS Assignment") %>%
  select(food_item, gdqs = gdqs_category) %>%
  mutate(
    food_item = str_trim(food_item),
    gdqs = str_trim(gdqs)
  )

# ---- 3) Merge recall with mapping ----
merged <- recall_data %>%
  left_join(gdqs_map, by = "food_item")

# Warn on unmapped
unmapped <- merged %>%
  filter(is.na(gdqs)) %>%
  distinct(food_item)
if (nrow(unmapped) > 0) {
  warning("Unmapped food items:\n", paste0("- ", unmapped$food_item, collapse = "\n"))
}

# ---- 4) Handle empty case ----
if (nrow(merged %>% filter(!is.na(gdqs))) == 0) {
  warning("No mapped foods found. Returning zero scores only.")
  out <- recall_data %>%
    select(survey_id) %>%
    distinct()
  out$gdqs_plus <- 0
  out$gdqs_minus <- 0
  out$gdqs_overall <- 0
  out$gdqs_risk <- "High"
  print(out)
  quit(save = "no")
}

# ---- 5) Split multi-category foods ----
merged <- merged %>%
  filter(!is.na(gdqs)) %>%
  tidyr::separate_rows(gdqs, sep = ";") %>%
  mutate(gdqs = str_to_title(str_trim(gdqs)))

# Divide intake for split items
merged <- merged %>%
  group_by(survey_id, food_item) %>%
  mutate(actual_gram_intake = actual_gram_intake / n()) %>%
  ungroup()

# Remove “Others”
merged <- merged %>% filter(gdqs != "Others")

# ---- 6) Validate GDQS categories ----
gdqs_healthy <- c(
  "Citrus Fruits", "Deep Orange Fruits", "Other Fruits",
  "Dark Green Leafy Vegetables", "Cruciferous Vegetables", "Deep Orange Vegetables",
  "Other Vegetables", "Legumes", "Deep Orange Tubers",
  "Nuts And Seeds", "Whole Grains", "Liquid Oils",
  "Fish And Shellfish", "Poultry And Game Meat", "Low Fat Dairy", "Eggs"
)

gdqs_unhealthy <- c(
  "High Fat Dairy", "Red Meat", "Processed Meat", "Refined Grains And Baked Goods",
  "Sweets And Ice Cream", "Sugar-Sweetened Beverages", "Juice",
  "White Roots And Tubers", "Purchased Deep-Fried Foods"
)

valid_gdqs <- c(gdqs_healthy, gdqs_unhealthy)

invalid <- merged %>%
  filter(!gdqs %in% valid_gdqs) %>%
  distinct(gdqs)

if (nrow(invalid) > 0) {
  stop("Invalid GDQS categories found: ", paste(invalid$gdqs, collapse = ", "))
}

# ---- 7) Aggregate to ID x GDQS Category (Sum intake) ----
df_group <- merged %>%
  group_by(survey_id, gdqs) %>%
  summarise(intake = sum(actual_gram_intake, na.rm = TRUE), .groups = "drop")

# ---- 8) Pivot wide ----
df_wide <- df_group %>%
  pivot_wider(names_from = gdqs, values_from = intake, values_fill = 0)

# ---- 9) Ensure all expected GDQS columns exist ----
all_expected <- valid_gdqs
for (col in all_expected) {
  if (!(col %in% names(df_wide))) {
    df_wide[[col]] <- 0
  }
}

# --- 10) Reorder columns (id first, then expected gdqs categories) ---
df_wide <- df_wide |>
  dplyr::select(survey_id, dplyr::all_of(all_expected))

# ---- 11) Add consumed flags ----
intake_cols <- setdiff(names(df_wide), "survey_id")
for (col in intake_cols) {
  df_wide[[paste0(col, "_consumed")]] <- ifelse(df_wide[[col]] > 0, 1, 0)
}

# ---- 12) Scoring system ----
# (Here we’ll translate your Python dict gdqs_points_criteria into an R list with cutpoints & values)

gdqs_points <- list(
  "Citrus Fruits" = list(ranges = c(0, 24, 70, Inf), values = c(0, 1, 2)),
  "Deep Orange Fruits" = list(ranges = c(0, 25, 124, Inf), values = c(0, 1, 2)),
  "Other Fruits" = list(ranges = c(0, 27, 108, Inf), values = c(0, 1, 2)),
  "Dark Green Leafy Vegetables" = list(ranges = c(0, 13, 38, Inf), values = c(0, 2, 4)),
  "Cruciferous Vegetables" = list(ranges = c(0, 13, 37, Inf), values = c(0, 0.25, 0.5)),
  "Deep Orange Vegetables" = list(ranges = c(0, 9, 46, Inf), values = c(0, 0.25, 0.5)),
  "Other Vegetables" = list(ranges = c(0, 23, 115, Inf), values = c(0, 0.25, 0.5)),
  "Legumes" = list(ranges = c(0, 9, 43, Inf), values = c(0, 2, 4)),
  "Deep Orange Tubers" = list(ranges = c(0, 12, 64, Inf), values = c(0, 0.25, 0.5)),
  "Nuts And Seeds" = list(ranges = c(0, 7, 14, Inf), values = c(0, 2, 4)),
  "Whole Grains" = list(ranges = c(0, 8, 14, Inf), values = c(0, 1, 2)),
  "Liquid Oils" = list(ranges = c(0, 2, 7.51, Inf), values = c(0, 1, 2)),
  "Fish And Shellfish" = list(ranges = c(0, 14, 72, Inf), values = c(0, 1, 2)),
  "Poultry And Game Meat" = list(ranges = c(0, 16, 45, Inf), values = c(0, 1, 2)),
  "Low Fat Dairy" = list(ranges = c(0, 33, 133, Inf), values = c(0, 1, 2)),
  "Eggs" = list(ranges = c(0, 6, 33, Inf), values = c(0, 1, 2)),
  "High Fat Dairy" = list(ranges = c(0, 35, 143, 735, Inf), values = c(0, 1, 2, 0)),
  "Red Meat" = list(ranges = c(0, 9, 47, Inf), values = c(0, 1, 0)),
  "Processed Meat" = list(ranges = c(0, 9, 31, Inf), values = c(2, 1, 0)),
  "Refined Grains And Baked Goods" = list(ranges = c(0, 7, 34, Inf), values = c(2, 1, 0)),
  "Sweets And Ice Cream" = list(ranges = c(0, 13, 38, Inf), values = c(2, 1, 0)),
  "Sugar-Sweetened Beverages" = list(ranges = c(0, 57, 181, Inf), values = c(2, 1, 0)),
  "Juice" = list(ranges = c(0, 36, 145, Inf), values = c(2, 1, 0)),
  "White Roots And Tubers" = list(ranges = c(0, 27, 108, Inf), values = c(2, 1, 0)),
  "Purchased Deep-Fried Foods" = list(ranges = c(0, 9, 46, Inf), values = c(2, 1, 0))
)

for (cat in names(gdqs_points)) {
  col <- cat
  pts_col <- paste0(cat, "_points")
  if (col %in% names(df_wide)) {
    ranges <- gdqs_points[[cat]]$ranges
    values <- gdqs_points[[cat]]$values
    df_wide[[pts_col]] <- cut(df_wide[[col]],
                              breaks = ranges,
                              labels = values,
                              include.lowest = TRUE,
                              right = FALSE) %>%
      as.character() %>%
      as.numeric()
  }
}

# ---- 13) Compute GDQS summary ----
df_wide <- df_wide %>%
  mutate(
    gdqs_plus = rowSums(across(paste0(gdqs_healthy, "_points")), na.rm = TRUE),
    gdqs_minus = rowSums(across(paste0(gdqs_unhealthy, "_points")), na.rm = TRUE),
    gdqs_overall = gdqs_plus + gdqs_minus,
    gdqs_risk = case_when(
      gdqs_overall < 15 ~ "High",
      gdqs_overall < 23 ~ "Moderate",
      TRUE ~ "Low"
    )
  )

summary_cols <- c("survey_id", "gdqs_plus", "gdqs_minus", "gdqs_overall", "gdqs_risk")

df_prefixed2 <- df_wide |>
  dplyr::rename_with(~ paste0("gdqs_", .x),
                     setdiff(names(df_wide), summary_cols)) |>
  janitor::clean_names()

# --- 14) Add _g_intake suffix to raw intake columns ---
intake_cols <- c(
  "gdqs_citrus_fruits", "gdqs_deep_orange_fruits", "gdqs_other_fruits",
  "gdqs_dark_green_leafy_vegetables", "gdqs_cruciferous_vegetables",
  "gdqs_deep_orange_vegetables", "gdqs_other_vegetables", "gdqs_legumes",
  "gdqs_deep_orange_tubers", "gdqs_nuts_and_seeds", "gdqs_whole_grains",
  "gdqs_liquid_oils", "gdqs_fish_and_shellfish", "gdqs_poultry_and_game_meat",
  "gdqs_low_fat_dairy", "gdqs_eggs", "gdqs_high_fat_dairy", "gdqs_red_meat",
  "gdqs_processed_meat", "gdqs_refined_grains_and_baked_goods",
  "gdqs_sweets_and_ice_cream", "gdqs_sugar_sweetened_beverages",
  "gdqs_juice", "gdqs_white_roots_and_tubers",
  "gdqs_purchased_deep_fried_foods"
)

df_prefixed2 <- df_prefixed2 |>
  dplyr::rename_with(~ paste0(.x, "_g_intake"), any_of(intake_cols))

print(df_prefixed2)


# test_gdqs <- compute_gdqs(recall_data = recall_data,
#                           id_col = "survey_id",
#                           food_item_col = "food_item",
#                           actual_gram_intake_col = "actual_gram_intake",
#                           use_fct_db = TRUE)