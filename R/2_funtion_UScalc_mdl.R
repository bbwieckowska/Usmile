#' Calculate U-smile coefficients for model comparison (based on models)
#'
#' Computes USMILE (User-centric Statistical Measures for Interpretable Learning Explanations)
#' coefficients for comparing two predictive models.
#'
#' @param ref_model Reference model object (glm or randomForest)
#' @param new_model New model object to compare (glm or randomForest)
#' @param y_coef Type of coefficient to calculate:
#'   \itemize{
#'     \item "rLR" - Relative Likelihood Ratio
#'     \item "BA" - Brier Alteration (average absolute change)
#'     \item "RB" - Relative Brier (relative change)
#'   }
#' @param dataset #' @param dataset Dataset used to prepare data for the models.
#'   - If `testing = TRUE`, a test dataset must be provided (required).
#'   - If `testing = FALSE`, the argument can be:
#'       * `NULL` if the model stores its own training data (e.g., `glm`, `randomForest`),
#'       * the training dataset if the model does not keep training data internally
#'         (e.g., `ranger`, `e1071`, `xgboost`, `nnet`, `naive_bayes`, tidymodels).
#'   This allows the function to work consistently with both training and test data.
#' @param testing Logical indicating whether to use test data (TRUE) or training data (FALSE)
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
#' # Compare two logistic regression models
#' model_glm_ref <- glm(disease ~ age + sex + bp + chol, data = heart_disease_train, family = "binomial")
#' model_glm_new <- glm(disease ~ age + sex + bp + chol + cp, data = heart_disease_train, family = "binomial")
#'
#' # Calculate rLR coefficients on training and test data
#' train_results_rLR <- UScalc_mdl(model_glm_ref, model_glm_new, y_coef = "rLR",
#'                          dataset = NULL, testing = FALSE)
#' test_results_rLR <- UScalc_mdl(model_glm_ref, model_glm_new, y_coef = "rLR",
#'                          dataset = heart_disease_test, testing = TRUE)
#'
#' # Calculate BA coefficients on training data
#' train_results_BA <- UScalc_mdl(model_glm_ref, model_glm_new, y_coef = "BA",
#'                         dataset = NULL, testing = FALSE)

#' # Calculate RB coefficients on training data
#' train_results_RB <- UScalc_mdl(model_glm_ref, model_glm_new, y_coef = "RB",
#'                         dataset = NULL, testing = FALSE)
#'
#' # Compare Random Forest to logistic regression
#' # (both models must be built based on the same variables)
#' model_fr_ref <- randomForest::randomForest(disease ~ age + sex + bp + chol, data = heart_disease_train)
#' train_results_rf_vs_glm <- UScalc_mdl(model_glm_ref, model_rf_ref, y_coef = "rLR",
#'                                dataset = NULL, testing = FALSE)
#' }
#'
#' @export
#' @importFrom stats pchisq

UScalc_mdl <- function(ref_model, new_model, y_coef,
                       dataset = NULL, testing = FALSE) {
  # Prepare data for both models
  ref_data <- USprep_mdl(ref_model, dataset, testing)
  new_data <- USprep_mdl(new_model, dataset, testing)

  # Combine outputs using USbind_out
  combined_data <- USbind_out(ref_data, new_data)

  # Use the comparison_df from USbind_out as raw_data
  raw_data <- combined_data$comparison_df

  # Get degrees of freedom difference from USbind_out
  df <- combined_data$n_vars_diff

  # Calculate coefficients from raw data - UScalc_raw will compute all needed statistics
  plot_data <- UScalc_raw(raw_data, y_coef, df)

  # Prepare comprehensive results table using values from plot_data
  results <- data.frame(
    label = c(
      "df", "sample size", "number of non-events", "number of events",
      "non-events with better prediction", "non-events with worse prediction",
      "events with worse prediction", "events with better prediction",
      "netto non-events", "netto events", "overall",
      "max netto non-events", "max netto events", "max overall",
      "max events non-events ratio", "LR non-events p-value",
      "LR events p-value", "LR overall p-value",
      "I non-events with better prediction", "I non-events with worse prediction",
      "I events with worse prediction", "I events with better prediction",
      "I netto netto non-events", "I netto netto events", "I overall"
    ),
    coefficient = c(
      plot_data$df,
      plot_data$n,
      plot_data$n0,
      plot_data$n1,
      plot_data$Y_nonev_be,
      plot_data$Y_nonev_wo,
      plot_data$Y_event_wo,
      plot_data$Y_event_be,
      plot_data$netto_Y_nonev,
      plot_data$netto_Y_event,
      ifelse(y_coef == "rLR", plot_data$Y_overall, NA),
      ifelse(y_coef == "rLR", plot_data$max_netto_Y_nonev, NA),
      ifelse(y_coef == "rLR", plot_data$max_netto_Y_event, NA),
      ifelse(y_coef == "rLR", plot_data$max_overall_Y, NA),
      ifelse(y_coef == "rLR", plot_data$max_netto_Y_ratio, NA),
      ifelse(y_coef == "rLR", plot_data$p_Y_nonev, NA),
      ifelse(y_coef == "rLR", plot_data$p_Y_event, NA),
      plot_data$p_Y_overall,  # Now using the p-value from UScalc_raw
      plot_data$point_sizes[1],
      plot_data$point_sizes[2],
      plot_data$point_sizes[3],
      plot_data$point_sizes[4],
      plot_data$netto_I_nonev,
      plot_data$netto_I_event,
      plot_data$I_overall
    ),
    stringsAsFactors = FALSE
  )

  return(list(results = results, plot_data = plot_data))
}
