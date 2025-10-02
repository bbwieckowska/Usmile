#' Calculate U-smile coefficients for model comparison (based on raw prediction data)
#'
#' Computes U-smile (User-centric Statistical Measures for Interpretable Learning Explanations) coefficients directly from the raw prediction data of two models.
#'
#' @param raw_data Data frame containing raw prediction data with columns:
#'   \itemize{
#'     \item y - binary outcome values (0/1)
#'     \item p_ref - predicted probabilities from reference model
#'     \item p - predicted probabilities from new model
#'   }
#' @param y_coef Type of coefficient to calculate:
#'   \itemize{
#'     \item "rLR" - Relative Likelihood Ratio
#'     \item "BA" - Brier Alteration (average absolute change)
#'     \item "RB" - Relative Brier (relative change)
#'   }
#' @param n_vars_diff Number of variables (degrees of freedom) for statistical tests
#' @param bootstrap_p Optional list with bootstrap p-values (from USboot_LRp)
#'
#' @return A list containing:
#' \itemize{
#'   \item results - Data frame with detailed comparison metrics
#'   \item plot_data - Data for visualization (used by USplot)
#' }
#'
#' @examples
#' \dontrun{
#' # Load Heart Disease datasets
#' data(heart_disease)
#' data(heart_disease_train)
#' data(heart_disease_test)
#'
#' # Prepare raw predictions from models
#' model_glm_ref <- glm(disease ~ age + sex + bp + chol, data = heart_disease_train, family = "binomial")
#' train_out_glm_ref <- USprep_mdl(model_glm_ref, dataset = NULL, testing = FALSE)
#' model_glm_new <- glm(disease ~ age + sex + bp + chol + cp, data = heart_disease_train, family = "binomial")
#' train_out_glm_new <- USprep_mdl(model_glm_new, dataset = NULL, testing = FALSE)
#'
#' # Combine raw data for comparison
#' combined_glm <- USbind_out(train_out_glm_ref, train_out_glm_new)
#' raw_compare <- combined_glm$comparison_df
#' n_vars_diff <- combined_glm$n_vars_diff  # Shows difference in number of parameters
#'
#'
#' # Calculate rLR coefficients
#' results_rLR <- UScalc_raw(raw_compare, y_coef = "rLR", n_vars_diff)
#'
#' # Calculate BA coefficients
#' results_BA <- UScalc_raw(raw_compare, y_coef = "BA", n_vars_diff)
#'
#' # Calculate RB coefficients
#' results_RB <- UScalc_raw(raw_compare, y_coef = "RB", n_vars_diff)
#'
#' }
#'
#'
#' @export
#' @importFrom stats pchisq
UScalc_raw <- function(raw_data, y_coef, n_vars_diff, bootstrap_p = NULL) {
  # Validate input data structure
  if (!all(c("y", "p_ref", "p") %in% names(raw_data))) {
    stop("raw_data must contain columns: y, p_ref, p")
  }

  y <- raw_data$y
  p_ref <- raw_data$p_ref
  p <- raw_data$p

  # Calculate likelihood ratio test statistics (needed for p_Y_overall)
  logLik <- sum(y * log(p + 1e-10) + (1 - y) * log(1 - p + 1e-10))
  logLik_ref <- sum(y * log(p_ref + 1e-10) + (1 - y) * log(1 - p_ref + 1e-10))
  lrt_stat <- as.vector(2 * abs(logLik - logLik_ref))

  # Calculate degrees of freedom and p-value for overall comparison
  df <- max(n_vars_diff, 1)
  p_Y_overall <- as.vector(stats::pchisq(lrt_stat, df = df, lower.tail = FALSE))

  # Classify predictions into improvement categories
  delta <- p - p_ref
  flag <- ifelse(delta > 0, 'up',
                 ifelse(delta < 0, 'dw', 'c'))
  subclass <- ifelse(y == 0 & flag == 'dw', 'nonev_be',
                     ifelse(y == 0 & flag == 'up', 'nonev_wo',
                            ifelse(y == 1 & flag == 'dw', 'event_wo',
                                   ifelse(y == 1 & flag == 'up', 'event_be', 'unknown'))))

  probs <- data.frame(y, p_ref, p, subclass)

  # Calculate category proportions
  n0 <- sum(probs$y == 0)
  n1 <- sum(probs$y == 1)
  n <- n0 + n1

  I_nonev_be <- sum(probs$subclass == 'nonev_be') / n0
  I_nonev_wo <- sum(probs$subclass == 'nonev_wo') / n0
  I_event_wo <- sum(probs$subclass == 'event_wo') / n1
  I_event_be <- sum(probs$subclass == 'event_be') / n1

  # Calculate net improvements
  netto_I_nonev <- I_nonev_be - I_nonev_wo
  netto_I_event <- I_event_be - I_event_wo
  I_overall <- (netto_I_nonev + netto_I_event)

  if (y_coef == "rLR") {
    # Calculate relative Likelihood Ratio metrics
    res_ref <- (y * log(p_ref + 1e-10) + (1 - y) * log(1 - p_ref + 1e-10))
    res <- (y * log(p + 1e-10) + (1 - y) * log(1 - p + 1e-10))
    probs <- data.frame(y, p_ref, p, subclass, res_ref, res)

    # Calculate likelihood residuals
    SS_nonev_ref <- sum(probs$res_ref[probs$y == 0])
    SS_event_ref <- sum(probs$res_ref[probs$y == 1])
    SS_nonev <- sum(probs$res[probs$y == 0])
    SS_event <- sum(probs$res[probs$y == 1])

    # Calculate category-specific likelihoods
    SS_nonev_dw_ref <- sum(probs$res_ref[probs$subclass == 'nonev_be'])
    SS_nonev_up_ref <- sum(probs$res_ref[probs$subclass == 'nonev_wo'])
    SS_event_dw_ref <- sum(probs$res_ref[probs$subclass == 'event_wo'])
    SS_event_up_ref <- sum(probs$res_ref[probs$subclass == 'event_be'])
    SS_nonev_dw <- sum(probs$res[probs$subclass == 'nonev_be'])
    SS_nonev_up <- sum(probs$res[probs$subclass == 'nonev_wo'])
    SS_event_dw <- sum(probs$res[probs$subclass == 'event_wo'])
    SS_event_up <- sum(probs$res[probs$subclass == 'event_be'])

    # Calculate likelihood differences
    delta_SS_nonev_dw <- SS_nonev_dw_ref - SS_nonev_dw
    delta_SS_nonev_up <- SS_nonev_up_ref - SS_nonev_up
    delta_SS_event_dw <- SS_event_dw_ref - SS_event_dw
    delta_SS_event_up <- SS_event_up_ref - SS_event_up

    # Calculate likelihood ratio statistics
    nonev_be <- -2 * delta_SS_nonev_dw
    nonev_wo <- 2 * delta_SS_nonev_up
    event_wo <- 2 * delta_SS_event_dw
    event_be <- -2 * delta_SS_event_up

    netto_nonev <- nonev_be - nonev_wo
    netto_event <- event_be - event_wo
    overall <- (netto_nonev + netto_event)

    # Calculate maximum possible values for normalization
    max_netto_Y_nonev <- ifelse(nonev_be >= nonev_wo, -2 * SS_nonev_ref, -2 * SS_nonev)
    max_netto_Y_event <- ifelse(event_be >= event_wo, -2 * SS_event_ref, -2 * SS_event)
    max_overall_Y <- -2 * (SS_nonev_dw_ref + SS_nonev_up_ref + SS_event_dw_ref + SS_event_up_ref)
    max_netto_Y_ratio <- max_netto_Y_event / max_netto_Y_nonev

    # Calculate normalized metrics
    Y_nonev_be <- nonev_be / max_netto_Y_nonev
    Y_nonev_wo <- nonev_wo / max_netto_Y_nonev
    Y_event_wo <- event_wo / max_netto_Y_event
    Y_event_be <- event_be / max_netto_Y_event
    netto_Y_nonev <- netto_nonev / max_netto_Y_nonev
    netto_Y_event <- netto_event / max_netto_Y_event
    Y_overall <- overall / max_overall_Y

    # Use bootstrap p-values if provided, otherwise calculate from gamma
    if (!is.null(bootstrap_p)) {
      p_Y_nonev <- bootstrap_p$p_Y_nonev
      p_Y_event <- bootstrap_p$p_Y_event
    } else {
      # Gamma parameters
      shape <- if (!is.na(n_vars_diff)) n_vars_diff / 2 else NA
      c1 <- n1/n
      c0 <- 1-c1
      rate0 <- 1/(2*c0)
      rate1 <- 1/(2*c1)

      p_Y_event <- if (!is.na(shape)) 1 - stats::pgamma(abs(netto_event), shape = shape, rate = rate1) else NA
      p_Y_nonev <- if (!is.na(shape)) 1 - stats::pgamma(abs(netto_nonev), shape = shape, rate = rate0) else NA
    }

  } else if (y_coef == "BA" | y_coef == "RB") {
    # Calculate Brier-based metrics
    res_sq_ref <- (y - p_ref)^2
    res_sq <- (p - y)^2
    probs <- data.frame(y, p_ref, p, subclass, res_sq_ref, res_sq)

    # Calculate Brier score components
    SS_nonev_ref <- sum(probs$res_sq_ref[probs$y == 0])
    SS_event_ref <- sum(probs$res_sq_ref[probs$y == 1])
    SS_nonev <- sum(probs$res_sq[probs$y == 0])
    SS_event <- sum(probs$res_sq[probs$y == 1])

    # Calculate category-specific Brier scores
    SS_nonev_dw_ref <- sum(probs$res_sq_ref[probs$subclass == 'nonev_be'])
    SS_nonev_up_ref <- sum(probs$res_sq_ref[probs$subclass == 'nonev_wo'])
    SS_event_dw_ref <- sum(probs$res_sq_ref[probs$subclass == 'event_wo'])
    SS_event_up_ref <- sum(probs$res_sq_ref[probs$subclass == 'event_be'])
    SS_nonev_dw <- sum(probs$res_sq[probs$subclass == 'nonev_be'])
    SS_nonev_up <- sum(probs$res_sq[probs$subclass == 'nonev_wo'])
    SS_event_dw <- sum(probs$res_sq[probs$subclass == 'event_wo'])
    SS_event_up <- sum(probs$res_sq[probs$subclass == 'event_be'])

    # Calculate Brier score differences
    delta_SS_nonev_dw <- SS_nonev_dw_ref - SS_nonev_dw
    delta_SS_nonev_up <- SS_nonev_up_ref - SS_nonev_up
    delta_SS_event_dw <- SS_event_dw_ref - SS_event_dw
    delta_SS_event_up <- SS_event_up_ref - SS_event_up

    if (y_coef == "BA") {
      # Calculate Brier Advantage (absolute differences)
      Y_nonev_be <- delta_SS_nonev_dw / n0
      Y_nonev_wo <- -delta_SS_nonev_up / n0
      Y_event_wo <- -delta_SS_event_dw / n1
      Y_event_be <- delta_SS_event_up / n1
    } else {
      # Calculate Relative Brier (relative differences)
      Y_nonev_be <- delta_SS_nonev_dw / SS_nonev_ref
      Y_nonev_wo <- -delta_SS_nonev_up / SS_nonev_ref
      Y_event_wo <- -delta_SS_event_dw / SS_event_ref
      Y_event_be <- delta_SS_event_up / SS_event_ref
    }

    netto_Y_nonev <- Y_nonev_be - Y_nonev_wo
    netto_Y_event <- Y_event_be - Y_event_wo
    Y_overall <- ((n0 / n) * netto_Y_nonev + (n1 / n) * netto_Y_event)

    # Set NA for unused metrics in BA/RB mode
    max_netto_Y_nonev <- NA
    max_netto_Y_event <- NA
    max_overall_Y <- NA
    max_netto_Y_ratio <- NA
    p_Y_nonev <- NA
    p_Y_event <- NA
  } else {
    stop("Unsupported y_coef value. Use 'rLR', 'BA', or 'RB'")
  }

  # Return results - now p_Y_overall is always included
  return(list(
    data.Y = data.frame(
      subclass = c('nonev_be', 'nonev_wo', 'event_wo', 'event_be'),
      value = c(Y_nonev_be, Y_nonev_wo, Y_event_wo, Y_event_be),
      coefficient = rep('Y', 4),
      segment = c('nonev', 'nonev', 'event', 'event'),
      wo_be = c(0, 1, 1, 0)
    ),
    n = n,
    n0 = n0,
    n1 = n1,
    point_sizes = c(I_nonev_be, I_nonev_wo, I_event_wo, I_event_be),
    netto_Y_nonev = netto_Y_nonev,
    netto_Y_event = netto_Y_event,
    Y_nonev_be = Y_nonev_be,
    Y_nonev_wo = Y_nonev_wo,
    Y_event_wo = Y_event_wo,
    Y_event_be = Y_event_be,
    nonev_solid = ifelse(y_coef == "rLR", !is.na(p_Y_nonev) && p_Y_nonev < 0.05, FALSE),
    event_solid = ifelse(y_coef == "rLR", !is.na(p_Y_event) && p_Y_event < 0.05, FALSE),
    Y_overall = ifelse(y_coef == "rLR", Y_overall, NA),
    max_netto_Y_nonev = ifelse(y_coef == "rLR", max_netto_Y_nonev, NA),
    max_netto_Y_event = ifelse(y_coef == "rLR", max_netto_Y_event, NA),
    max_overall_Y = ifelse(y_coef == "rLR", max_overall_Y, NA),
    max_netto_Y_ratio = ifelse(y_coef == "rLR", max_netto_Y_ratio, NA),
    p_Y_nonev = ifelse(y_coef == "rLR", p_Y_nonev, NA),
    p_Y_event = ifelse(y_coef == "rLR", p_Y_event, NA),
    p_Y_overall = p_Y_overall,  # Now always included
    df = df,
    netto_I_nonev = netto_I_nonev,
    netto_I_event = netto_I_event,
    I_overall = I_overall
  ))
}
