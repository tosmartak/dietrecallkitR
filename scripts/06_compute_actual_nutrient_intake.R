# scripts/05_compute_actual_nutrient_intake.R
# Prototype: Compute Actual Nutrient Intake from 24h recall

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(tibble)

# ---- 1) Simulated recall_data ----
set.seed(123)

recall_data <- tibble::tibble(
  survey_id = c(1, 1, 2, 2, 3, 3, 4),
  recall_id = c(1, 2, 1, 1, 1, 2, 1),
  food_item = c(
    "Beans, broad, dry, raw",
    "Orange (chungwa), pulp, raw",
    "Rabbit meat, stewed",
    "Meat Samosa",
    "Biscuits and Cookies, Savoury",
    "Tangerine (sandara), juice",
    "Kale (sukuma wiki) raw"
  ),
  actual_gram_intake = c(150, 200, 120, 80, 40, 180, 250)
)

# ---- 2) Load mapping (fct_db) ----
db_url <- "https://raw.githubusercontent.com/tosmartak/dietrecallkitR/main/data-raw/fct_db.csv"
fct_db <- suppressMessages(read_csv(db_url, show_col_types = FALSE)) %>%
  mutate(food_item = str_trim(food_item))

# ---- 3) Specify nutrient columns ----
nutrient_cols <- c(
  "Energy (kcal)", "Protein(g)", "Fat(g)", "Carbohydrate available (g)",
  "Fibre(g)", "Ca(mg)", "Fe(mg)", "Zn(mg)", "Vit A-RAE(mcg)", "Vit C(mg)"
)

required_cols <- c("food_item", "Edible conversion factor", nutrient_cols)

missing_cols <- setdiff(required_cols, names(fct_db))
if (length(missing_cols) > 0) {
  stop("Missing columns in fct_db: ", paste(missing_cols, collapse = ", "))
}

# ---- 4) Aggregate recall data (id + recall + food_item) ----
recall_agg <- recall_data %>%
  group_by(survey_id, recall_id, food_item) %>%
  summarise(actual_gram_intake = sum(actual_gram_intake, na.rm = TRUE), .groups = "drop")

# ---- 5) Merge with FCT ----
merged <- recall_agg %>%
  left_join(fct_db %>%
              select(all_of(required_cols)),
            by = "food_item")

# Warn on unmapped items
unmapped <- merged %>%
  filter(is.na(`Edible conversion factor`)) %>%
  distinct(food_item)
if (nrow(unmapped) > 0) {
  warning("Unmapped food items:\n", paste0("- ", unmapped$food_item, collapse = "\n"))
}

# ---- 6) Compute nutrient intakes ----
nutrient_intakes <- merged %>%
  rowwise() %>%
  mutate(across(all_of(nutrient_cols), ~ {
    if (is.na(.x) || is.na(`Edible conversion factor`) || is.na(actual_gram_intake)) {
      NA_real_
    } else {
      (.x * `Edible conversion factor` * actual_gram_intake) / 100
    }
  }, .names = "{.col}_intake")) %>%
  ungroup()

# ---- 7) Select final output ----
# Food level summary
out <- nutrient_intakes %>%
  select(survey_id, recall_id, food_item, actual_gram_intake,
         ends_with("_intake"))

# Recall level summary
out2 <- nutrient_intakes %>%
  group_by(survey_id, recall_id) %>%
  summarise(
    actual_gram_intake = sum(actual_gram_intake, na.rm = TRUE),
    across(ends_with("_intake"), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

print(out)


# test_compute_ani <- compute_actual_nutrient_intake(recall_data = recall_data,
#                                                    id_col = "survey_id",
#                                                    recall_col = "recall_id",
#                                                    food_item_col = "food_item",
#                                                    actual_gram_intake_col = "actual_gram_intake",
#                                                    use_fct_db = TRUE)