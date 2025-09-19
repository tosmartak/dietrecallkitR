This folder contains scripts used to create datasets bundled with dietrecallkit.

## Rules

- Scripts (*.R) are tracked in Git

  - They document exactly how each dataset was created.
  - Example: make_example_data.R generates dietrecall_example.rda used in tests, README, and vignettes.

- Raw data files are NOT tracked
  - This keeps sensitive or heavy files out of version control.

- Generated datasets live in data/

  - Scripts in this folder save outputs with usethis::use_data().

  - Example:`dietrecall_example.rda` is available to users via `data("dietrecall_example")`.


## Workflow

1. Place raw data locally in this folder (ignored by Git).

2. Run the corresponding script (e.g. make_example_data.R).

3. The script generates and saves a processed .rda file in data/.

4. Document the dataset in R/data_doc.R.

## Example

``` r
# From package root:
source("data-raw/make_example_data.R")

# Verify the dataset is available
devtools::load_all()
data("dietrecall_example")
str(dietrecall_example)
```