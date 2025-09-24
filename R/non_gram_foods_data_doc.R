#' Example Non-Gram Foods Conversion Data
#'
#' A lightweight example dataset that replicates the structure of the
#' non-gram foods conversion sheet produced by
#' \code{\link{get_non_gram_foods}} and completed by the user.
#'
#' @description
#' This dataset is included for testing and demonstration purposes.
#' It provides mappings between units (e.g., cups, spoons) and their
#' equivalent weights in grams. It is used internally by
#' \code{\link{compute_actual_g_intake}} during testing to validate workflows.
#'
#' @docType data
#' @keywords datasets
#' @name non_gram_foods_conversion
#' @usage data(non_gram_foods_conversion)
#' @format A tibble with the following columns:
#' \describe{
#'   \item{subcounty}{Subcounty name (character)}
#'   \item{food_item}{Food item description (character)}
#'   \item{unit}{Original unit of measure (character, not grams)}
#'   \item{amount}{Reference amount in the specified unit (numeric)}
#'   \item{gram}{Equivalent gram weight for the reference amount (numeric)}
#' }
#'
#' @examples
#' data("non_gram_foods_conversion")
#' head(non_gram_foods_conversion)
"non_gram_foods_conversion"
