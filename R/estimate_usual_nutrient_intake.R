#' Estimate Usual Nutrient Intake using NRC method with adaptive repeater policy
#'
#' Estimates usual nutrient intake distributions from 24-hour recall data following
#' the NRC/IOM methodology (see \doi{10.17226/10666}). The method adjusts observed
#' intakes for within-person variability using variance-component shrinkage based
#' on respondents with repeated recalls. It is flexible and adaptive to different
#' replicate data quality scenarios through the `repeater_policy` argument.
#'
#' @param recall_data A data frame containing repeated 24-hour recall data, with one
#'   row per observation (respondent-day).
#' @param id_col Character scalar. The name of the column identifying respondents.
#'   Each unique ID represents one participant who may have one or more recall days.
#' @param nutrient_cols Character vector of one or more column names containing
#'   nutrient intake values to be processed (e.g. `"Energy.kcal_intake"`, `"Protein.g_intake"`).
#'   All must be numeric and non-negative.
#' @param transform Transformation applied prior to variance estimation to improve
#'   normality. Options are `"cuberoot"` (default), `"log"`, `"sqrt"`, or `"none"`.
#' @param jitter Logical; if TRUE, adds a deterministic small numeric offset after
#'   transformation to prevent ties (useful when values are identical after rounding).
#' @param warn_negative_between Logical; if TRUE, issues warnings when the estimated
#'   between-person variance component is negative before flooring to zero.
#' @param repeater_policy Character scalar specifying how strictly to enforce the
#'   minimum amount of replicate information:
#'   \itemize{
#'     \item `"auto"` – chooses a balanced adaptive rule based on available replicate
#'       information (default).
#'     \item `"strict"` – enforces higher thresholds for replicate data before adjusting.
#'     \item `"lenient"` – proceeds with adjustment even when replicate information is limited.
#'   }
#' @param detailed Logical; if TRUE, includes diagnostic columns such as observed mean,
#'   between- and within-person standard deviations, degrees of freedom, replicate
#'   count, and shrinkage ratio.
#'
#' @details
#' This function implements the NRC (1986) / IOM (2003) recommended approach for
#' adjusting observed 24-hour recall data to estimate the distribution of *usual*
#' nutrient intakes within a population. The workflow is:
#' \enumerate{
#'   \item Apply the chosen transformation (`transform`).
#'   \item Identify individuals with ≥2 recall days (repeaters).
#'   \item Estimate within- and between-person variance using ANOVA among repeaters.
#'   \item Derive shrinkage ratio = SD(between) / SD(observed).
#'   \item Shrink each individual's mean intake toward the population mean, adjusting
#'         for the ratio of within-to-between variation.
#'   \item Back-transform to original units.
#' }
#'
#' When no repeaters are available, observed means are returned unchanged.
#' If insufficient replicate information exists, the behaviour depends on `repeater_policy`.
#'
#' When the estimated between-person variance is non-identifiable (≤ 0),
#' the NRC adjustment is skipped and observed mean intakes are returned with a warning.
#'
#' @return A tibble containing one row per respondent and estimated usual intakes
#'   for each nutrient. If `detailed = TRUE`, additional columns include:
#'   \itemize{
#'     \item `*_observed_mean` – back-transformed observed mean intake.
#'     \item `*_sd_between`, `*_sd_observed` – variance components.
#'     \item `*_df_resid`, `*_R` – residual degrees of freedom and total replicate info.
#'     \item `*_shrink_ratio` – the shrinkage factor applied.
#'   }
#'
#' @references
#' Institute of Medicine (2003). *Dietary Reference Intakes: Applications in Dietary Planning.*
#' Washington (DC): National Academies Press. Appendix E.
#' (\url{https://www.ncbi.nlm.nih.gov/books/NBK221370/})
#'
#' @examples
#' # Example with Energy and Protein
#' df <- tibble::tibble(
#'   id = c(1, 1, 2, 2, 3),
#'   Energy.kcal_intake = c(1800, 2200, 1500, 1600, 2000),
#'   Protein.g_intake = c(55, 65, 40, 42, 50)
#' )
#'
#' estimate_usual_nutrient_intake(
#'   recall_data = df,
#'   id_col = "id",
#'   nutrient_cols = c("Energy.kcal_intake", "Protein.g_intake"),
#'   transform = "cuberoot"
#' )
#'
#' @export
estimate_usual_nutrient_intake <- function(recall_data,
                                           id_col,
                                           nutrient_cols,
                                           transform = c("cuberoot", "log", "sqrt", "none"),
                                           jitter = FALSE,
                                           warn_negative_between = TRUE,
                                           repeater_policy = c("auto", "strict", "lenient"),
                                           detailed = FALSE) {
  # --- 1) Validate inputs ---
  transform <- match.arg(transform)
  repeater_policy <- match.arg(repeater_policy)

  if (!is.data.frame(recall_data)) stop("`recall_data` must be a data.frame.")
  if (!is.character(id_col) || length(id_col) != 1) {
    stop("`id_col` must be the *name* of a single column (a character string), not the values themselves.")
  }
  if (!all(id_col %in% names(recall_data))) stop("`id_col` not found in `recall_data`.")
  if (!is.character(nutrient_cols) || length(nutrient_cols) < 1) {
    stop("`nutrient_cols` must be a non-empty character vector.")
  }
  if (!all(nutrient_cols %in% names(recall_data))) {
    missing_cols <- paste(setdiff(nutrient_cols, names(recall_data)), collapse = ", ")
    stop(paste0("Nutrient columns not found in `recall_data`: ", missing_cols))
  }
  for (nc in nutrient_cols) {
    if (!is.numeric(recall_data[[nc]])) stop(paste0("Column '", nc, "' must be numeric."))
    if (any(recall_data[[nc]] < 0, na.rm = TRUE)) stop(paste0("Column '", nc, "' contains negative values."))
  }

  # --- 2) Identify repeaters for variance estimation (n_i >= 2) ---
  n_days_tab_all <- table(recall_data[[id_col]])
  repeater_ids <- names(n_days_tab_all[n_days_tab_all >= 2])
  data_repeater <- recall_data[recall_data[[id_col]] %in% repeater_ids, , drop = FALSE]
  data_all <- recall_data

  # If there are repeaters, compute average replicate count among repeaters
  n_mean <- if (length(repeater_ids) > 0) {
    mean(as.numeric(n_days_tab_all[repeater_ids]), na.rm = TRUE)
  } else {
    NA_real_
  }

  # --- 3) Transformation helpers ---
  apply_transform <- function(x) {
    if (transform == "log") {
      return(log(x + .Machine$double.eps))
    } else if (transform == "sqrt") {
      return(sqrt(x))
    } else if (transform == "cuberoot") {
      return(x^(1 / 3))
    } else {
      return(x)
    }
  }
  inverse_transform <- function(x) {
    if (transform == "log") {
      return(exp(x))
    } else if (transform == "sqrt") {
      return(x^2)
    } else if (transform == "cuberoot") {
      return(x^3)
    } else {
      return(x)
    }
  }
  bt <- function(x) paste0("`", x, "`") # safe backticks for formulas

  # --- 4) Core loop per nutrient ---
  out_list <- vector("list", length(nutrient_cols))
  names(out_list) <- nutrient_cols

  for (nutrient in nutrient_cols) {
    # 4a) Transform all daily values (supports messy names via [[ ]])
    trans_name <- paste0("trans_", nutrient)
    data_all[[trans_name]] <- apply_transform(data_all[[nutrient]])
    if (jitter) {
      eps_seq <- seq_len(nrow(data_all)) * .Machine$double.eps
      data_all[[trans_name]] <- data_all[[trans_name]] + eps_seq
    }

    # 4b) Estimate variance components from repeaters only
    shrink_ratio <- 1
    sd_obs <- NA_real_
    sd_between <- NA_real_
    df_resid <- NA_real_
    R_info <- NA_real_

    if (nrow(data_repeater) > 0 && is.finite(n_mean) && n_mean > 0) {
      # compute transformed values for repeaters
      trans_rep <- apply_transform(data_repeater[[nutrient]])
      data_repeater[[trans_name]] <- if (jitter) {
        eps_seq_rep <- seq_len(nrow(data_repeater)) * .Machine$double.eps
        trans_rep + eps_seq_rep
      } else {
        trans_rep
      }

      # must have at least 2 distinct repeater ids to fit aov
      n_unique_rep_ids <- length(unique(data_repeater[[id_col]]))
      if (n_unique_rep_ids < 2) {
        warning(
          "Too few distinct repeaters for ", nutrient,
          " (", n_unique_rep_ids, " repeater id). Skipping adjustment and returning observed means."
        )
        shrink_ratio <- 1
      } else {
        # One-way ANOVA on transformed values with factor(id), safely quoted
        formula_str <- paste0(bt(trans_name), " ~ factor(", bt(id_col), ")")
        aov_model <- stats::aov(stats::as.formula(formula_str), data = data_repeater)
        aov_sum <- summary(aov_model)[[1]]

        # Extract MS terms and residual df
        msm <- aov_sum[1, "Mean Sq"]
        mse <- aov_sum["Residuals", "Mean Sq"]
        df_resid <- aov_sum["Residuals", "Df"]

        # Total replicate information R = sum(max(n_i - 1, 0)) over repeaters
        n_days_tab_rep <- table(data_repeater[[id_col]])
        R_info <- sum(pmax(as.numeric(n_days_tab_rep) - 1, 0))

        # Policy logic driven by df_resid and R_info
        low_info <- (is.finite(df_resid) && df_resid < 5) || (is.finite(R_info) && R_info < 5)
        mid_info <- (is.finite(df_resid) && df_resid < 15) || (is.finite(R_info) && R_info < 15)

        if (repeater_policy == "auto") {
          if (low_info) {
            warning(
              "Very limited replicate information for ", nutrient,
              " (df_resid = ", df_resid, ", R = ", R_info, "). Skipping adjustment and returning observed means."
            )
            shrink_ratio <- 1
          } else {
            if (mid_info) {
              warning(
                "Limited replicate information for ", nutrient,
                " (df_resid = ", df_resid, ", R = ", R_info, "). Proceeding with high-uncertainty adjustment."
              )
            }
            v_obs <- msm / n_mean
            v_between <- (msm - mse) / n_mean
            if (v_between <= 0) {
              if (warn_negative_between) {
                warning(
                  "Non-identifiable between-person variance for ", nutrient,
                  ". NRC adjustment skipped and observed means returned."
                )
              }
              shrink_ratio <- 1
              sd_obs <- sqrt(max(v_obs, 0))
              sd_between <- NA_real_
            } else {
              sd_obs <- sqrt(max(v_obs, 0))
              sd_between <- sqrt(v_between)
              shrink_ratio <- if (isTRUE(sd_obs == 0) || is.na(sd_obs)) 0 else sd_between / sd_obs
            }
          }
        } else if (repeater_policy == "strict") {
          if (mid_info) {
            warning(
              "Insufficient replicate information for ", nutrient,
              " under 'strict' policy (df_resid = ", df_resid, ", R = ", R_info, "). Skipping adjustment."
            )
            shrink_ratio <- 1
          } else {
            v_obs <- msm / n_mean
            v_between <- (msm - mse) / n_mean
            if (v_between <= 0) {
              if (warn_negative_between) {
                warning(
                  "Non-identifiable between-person variance for ", nutrient,
                  ". NRC adjustment skipped and observed means returned."
                )
              }
              shrink_ratio <- 1
              sd_obs <- sqrt(max(v_obs, 0))
              sd_between <- NA_real_
            } else {
              sd_obs <- sqrt(max(v_obs, 0))
              sd_between <- sqrt(v_between)
              shrink_ratio <- if (isTRUE(sd_obs == 0) || is.na(sd_obs)) 0 else sd_between / sd_obs
            }
          }
        } else { # lenient
          if (low_info) {
            warning(
              "Very low replicate information for ", nutrient,
              " (df_resid = ", df_resid, ", R = ", R_info, "). Proceeding but estimates may be unstable."
            )
          }
          v_obs <- msm / n_mean
          v_between <- (msm - mse) / n_mean
          if (v_between <= 0) {
            if (warn_negative_between) {
              warning(
                "Non-identifiable between-person variance for ", nutrient,
                ". NRC adjustment skipped and observed means returned."
              )
            }
            shrink_ratio <- 1
            sd_obs <- sqrt(max(v_obs, 0))
            sd_between <- NA_real_
          } else {
            sd_obs <- sqrt(max(v_obs, 0))
            sd_between <- sqrt(v_between)
            shrink_ratio <- if (isTRUE(sd_obs == 0) || is.na(sd_obs)) 0 else sd_between / sd_obs
          }
        }
      }
    } else {
      warning("No repeaters available for ", nutrient, ". Skipping adjustment and returning observed means.")
      shrink_ratio <- 1
    }

    # 4c) Compute respondent means on transformed scale for all respondents
    mean_trans_df <- stats::aggregate(
      x = data_all[[trans_name]],
      by = list(data_all[[id_col]]),
      FUN = mean,
      na.rm = TRUE
    )
    names(mean_trans_df) <- c(id_col, "mean_trans")

    # 4d) Shrink toward population mean on transformed scale
    mean_overall <- mean(mean_trans_df$mean_trans, na.rm = TRUE)
    adj_name <- paste0(nutrient, "_adj_trans")
    usual_name <- paste0(nutrient, "_usual")

    mean_trans_df[[adj_name]] <- ((mean_trans_df$mean_trans - mean_overall) * shrink_ratio) + mean_overall
    mean_trans_df[[usual_name]] <- inverse_transform(mean_trans_df[[adj_name]])

    # 4e) Optional diagnostics
    if (isTRUE(detailed)) {
      obs_mean_name <- paste0(nutrient, "_observed_mean")
      sd_between_name <- paste0(nutrient, "_sd_between")
      sd_observed_name <- paste0(nutrient, "_sd_observed")
      df_resid_name <- paste0(nutrient, "_df_resid")
      R_name <- paste0(nutrient, "_R")
      shrink_name <- paste0(nutrient, "_shrink_ratio")

      mean_trans_df[[obs_mean_name]] <- inverse_transform(mean_trans_df$mean_trans)
      mean_trans_df[[sd_between_name]] <- sd_between
      mean_trans_df[[sd_observed_name]] <- sd_obs
      mean_trans_df[[df_resid_name]] <- df_resid
      mean_trans_df[[R_name]] <- R_info
      mean_trans_df[[shrink_name]] <- shrink_ratio
    }

    keep_cols <- c(id_col, usual_name)
    if (isTRUE(detailed)) {
      keep_cols <- c(keep_cols, setdiff(names(mean_trans_df), c(id_col, "mean_trans", adj_name, usual_name)))
    }
    out_list[[nutrient]] <- mean_trans_df[keep_cols]
  }

  # --- 5) Merge nutrient-wise outputs by id and return as tibble ---
  out <- Reduce(function(x, y) merge(x, y, by = id_col, all = TRUE), out_list)
  tibble::as_tibble(out)
}
