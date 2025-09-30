# scripts/03_compute_mdd.R
# Prototype for computing Minimum Dietary Diversity (MDD)

library(dplyr)
library(fastDummies)
library(readr)
library(stringr)

# ---- 1) Inputs ----
recall_data <- dietrecall_example$food_details %>%
  filter(!is.na(desc_of_food))%>%
  select(survey_id, food_item = desc_of_food)

id_col <- "survey_id"
food_item_col <- "food_item"
life_stage <- "woman"   # options: woman, man, adolescent, child
use_fct_db <- TRUE
fct_db_url <- "https://raw.githubusercontent.com/tosmartak/dietrecallkitR/main/data-raw/fct_db.csv"

# Optional: breastfeeding
breastfeeding_data <- NULL
breastfeeding_col <- NULL

# ---- 2) Load mapping ----
if (use_fct_db) {
  fct_map <- read_csv(fct_db_url, show_col_types = FALSE)
  group_col <- ifelse(life_stage == "child", "mdd_child", "mdd_adult")
  
  fct_map <- fct_map %>%
    select(food_item, !!sym(group_col)) %>%
    rename(food_group = !!sym(group_col))
} else {
  stop("Prototype assumes use_fct_db = TRUE for now")
}

# Rename group col to mdd_<life_stage>
fct_map <- fct_map %>%
  rename(!!paste0("mdd_", life_stage) := food_group)

# ---- 3) Harmonize column names ----
if (food_item_col != "food_item") {
  recall_data <- recall_data %>%
    rename(food_item = !!sym(food_item_col))
}

# ---- 4) Merge recall with mapping ----
merged <- recall_data %>%
  left_join(fct_map, by = "food_item")

# Warn if unmapped items
unmapped <- merged %>%
  filter(is.na(!!sym(paste0("mdd_", life_stage)))) %>%
  distinct(food_item)

if (nrow(unmapped) > 0) {
  warning("Unmapped food items:\n",
          paste0("- ", unmapped$food_item, collapse = "\n"))
}

# ---- 5) One-hot encode food groups ----
group_col <- paste0("mdd_", life_stage)

dds_wide <- merged %>%
  filter(!is.na(!!sym(group_col))) %>%
  dummy_cols(select_columns = group_col,
             remove_first_dummy = FALSE,
             split = ";",
             remove_selected_columns = TRUE) %>%
  select(-food_item)

# ---- 6) Group by ID ----
dds_grouped <- dds_wide %>%
  group_by(!!sym(id_col)) %>%
  summarise(across(starts_with(paste0(group_col, "_")),
                   ~ max(.x, na.rm = TRUE)), .groups = "drop")

# ---- 7) Add breastfeeding if child ----
if (life_stage == "child" && !is.null(breastfeeding_data)) {
  dds_grouped <- dds_grouped %>%
    left_join(breastfeeding_data %>% select(!!sym(id_col), !!sym(breastfeeding_col)),
              by = id_col)
}

# ---- 8) Compute DDS and MDD ----
food_cols <- dds_grouped %>%
  select(starts_with(paste0(group_col, "_"))) %>%
  select(-matches("Others$")) %>%
  names()

# If breastfeeding is relevant, include it in the list of summation columns
if (life_stage == "child" && !is.null(breastfeeding_col)) {
  dds_grouped <- dds_grouped %>%
    mutate(
      !!paste0("dds_", life_stage) :=
        rowSums(across(all_of(c(food_cols, breastfeeding_col))), na.rm = TRUE),
      !!paste0("mdd_", life_stage) :=
        ifelse(!!sym(paste0("dds_", life_stage)) >= 5, 1, 0)
    )
} else if (life_stage == "child") {
  dds_grouped <- dds_grouped %>%
    mutate(
      !!paste0("dds_", life_stage) :=
        rowSums(across(all_of(food_cols)), na.rm = TRUE),
      !!paste0("mdd_", life_stage) :=
        ifelse(!!sym(paste0("dds_", life_stage)) >= 4, 1, 0)
    )
} else {
  dds_grouped <- dds_grouped %>%
    mutate(
      !!paste0("dds_", life_stage) :=
        rowSums(across(all_of(food_cols)), na.rm = TRUE),
      !!paste0("mdd_", life_stage) :=
        ifelse(!!sym(paste0("dds_", life_stage)) >= 5, 1, 0)
    )
}

# ---- 9) Final result ----
final <- dds_grouped
print(final)


