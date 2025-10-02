#' Combine Outputs from Two Models for Comparison
#'
#' This function combines the outputs from two model evaluations (from USprep_mdl)
#' to prepare them for USMILE analysis. It checks for consistency between models
#' and creates a data frame suitable for comparison.
#'
#' @param model_out_ref Output from USprep_mdl for the reference model
#' @param model_out_new Output from USprep_mdl for the new model to compare
#'
#' @return A list containing:
#' \itemize{
#'   \item comparison_df - Data frame with y, reference probabilities (p_ref), and new probabilities (p_new)
#'   \item n_vars_diff - Absolute difference in number of variables between models
#'   \item ref_vars - List of variables in the reference model
#'   \item new_vars - List of variables in the new model
#' }
#'
#' @examples
#' \dontrun{
#' # Load Heart Disease datasets
#' data(heart_disease)
#' data(heart_disease_train)
#' data(heart_disease_test)
#'
#' ### Example 1: Comparing nested logistic regression models
#' model_glm_ref <- glm(disease ~ age + sex + bp + chol, data = heart_disease_train, family = "binomial")
#' train_out_glm_ref <- USprep_mdl(model_glm_ref, dataset = NULL, testing = FALSE)
#' model_glm_new <- glm(disease ~ age + sex + bp + chol + cp, data = heart_disease_train, family = "binomial")
#' train_out_glm_new <- USprep_mdl(model_glm_new, dataset = NULL, testing = FALSE)
#'
#' # Combine outputs for comparison
#' combined_glm <- USbind_out(train_out_glm_ref, train_out_glm_new)
#' head(combined_glm$comparison_df)
#' combined_glm$n_vars_diff  # Shows difference in number of parameters
#'
#' ### Example 2: Comparing logistic regression with Random Forest
#' model_rf_ref <- randomForest::randomForest(disease ~ age + sex + bp + chol, data = heart_disease_train)
#' train_out_rf_ref <- USprep_mdl(model_rf_ref, dataset = NULL, testing = FALSE)
#'
#' # Combine with logistic regression output
#' combined_rf_vs_glm <- USbind_out(train_out_glm_ref, train_out_rf_ref)
#' head(combined_rf_vs_glm$comparison_df)
#' }
#'
#' @export
USbind_out <- function(model_out_ref, model_out_new) {
  # Check if data lengths are the same
  if (length(model_out_ref$y) != length(model_out_new$y) ||
      length(model_out_ref$p) != length(model_out_new$p)) {
    stop("Error: Data size differs between models. Both model versions must have the same number of observations.")
  }

  # Check if y values are identical in both models
  if (!all(model_out_ref$y == model_out_new$y)) {
    warning("Warning: y values differ between models. Please check the input data.")
  }

  # Create a results data frame
  dataframe_compare <- data.frame(
    y = model_out_ref$y,
    p_ref = model_out_ref$p,
    p = model_out_new$p
  )

  # Calculate the difference in number of variables
  n_vars_diff <- abs(model_out_new$n_vars - model_out_ref$n_vars)

  # Get variable lists
  ref_vars <- model_out_ref$var_names
  new_vars <- model_out_new$var_names

  # Return the results list
  list(
    comparison_df = dataframe_compare,
    n_vars_diff = n_vars_diff,
    ref_vars = ref_vars,
    new_vars = new_vars
  )
}
