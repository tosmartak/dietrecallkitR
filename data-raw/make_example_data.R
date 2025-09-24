# data-raw/make_example_data.R
# Script to create lightweight example dataset for dietrecallkit
# Run once from repo root with: source("data-raw/make_example_data.R")

library(readxl)
library(dplyr)
library(usethis)

# ---- 1) File path ----
raw_file <- file.path("data-raw", "dietary_recall_full.xlsx")

if (!file.exists(raw_file)) {
  stop("Raw Excel file not found at: ", raw_file)
}

# ---- 2) Read sheets ----
maintable <- read_excel(raw_file, sheet = "maintable")
food_details <- read_excel(raw_file, sheet = "food_details")
food_ingredients_group <- read_excel(raw_file, sheet = "food_ingredients_group")

# ---- 3) Select and subset maintable ----
maintable_sub <- maintable %>%
  select(
    survey_id,
    household_id,
    survey_date,
    recall_number,
    county,
    subcounty,
    ward,
    cu,
    mothers_age_in_years
  ) %>%
  slice_head(n = 50)

ids <- maintable_sub$survey_id

# ---- 4) Filter dependent sheets by survey_id ----
food_details_sub <- food_details %>%
  filter(survey_id %in% ids)

food_ingredients_group_sub <- food_ingredients_group %>%
  filter(survey_id %in% ids)

# ---- 5) Bundle into a single list ----
dietrecall_example <- list(
  maintable = maintable_sub,
  food_details = food_details_sub,
  food_ingredients_group = food_ingredients_group_sub
)

# ---- 6) Save as internal dataset ----
usethis::use_data(dietrecall_example, overwrite = TRUE)

message("dietrecall_example created and saved to data/")
