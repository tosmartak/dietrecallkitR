# tests/testthat/helper-data.R
# Runs before every test file in testthat 3e

# Load packaged datasets into the testing environment
utils::data("dietrecall_example", package = "dietrecallkit")
utils::data("non_gram_foods_conversion", package = "dietrecallkit")
