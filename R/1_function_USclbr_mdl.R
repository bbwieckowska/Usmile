#' Calibration of Random Forest predictive probabilities
#'
#' This function calibrates predictive probabilities using either isotonic regression
#' or logistic regression with cross-validation.
#'
#' @param y Vector of observed binary values (0 or 1)
#' @param p Vector of predicted probabilities (values between 0 and 1)
#' @param method Calibration method ("isotonic" - default or "logistic")
#' @param n_folds Number of folds for cross-validation (default: 10)
#' @param random_seed Random seed for reproducibility (default: 123)
#'
#' @return Vector of calibrated probabilities with the same length as the input vector p
#'
#' @examples
#' \dontrun{
#' # Load Heart Disease datasets
#' data(heart_disease)
#' data(heart_disease_train)
#' data(heart_disease_test)
#'
#' # Train Random Forest model
#' model_rf <- randomForest::randomForest(disease ~ age + sex, data = train_data)
#'
#' # Get raw predictions
#' preds <- predict(model_rf, type = "prob")[, 2]
#'
#' # Calibrate predictions using isotonic regression
#' calibrated_preds <- USclbr_mdl(
#'   y = as.numeric(train_data$disease) - 1,
#'   p = preds,
#'   method = "isotonic"
#' )
#'
#' # Calibrate predictions using logistic regression with 5-fold CV
#' calibrated_preds_log <- USclbr_mdl(
#'   y = as.numeric(train_data$disease) - 1,
#'   p = preds,
#'   method = "logistic",
#'   n_folds = 5
#' )
#' }
#'
#' @export
#' @importFrom caret createFolds
#' @importFrom earth earth
#' @importFrom stats predict glm binomial
USclbr_mdl <- function(y, p, method = "isotonic", n_folds = 10, random_seed = 123) {
  # Set the random seed for all stochastic operations
  set.seed(random_seed)

  if (!method %in% c("isotonic", "logistic")) {
    stop("Calibration method must be either 'isotonic' or 'logistic'")
  }

  # Checking input conditions
  if (length(unique(y)) != 2) stop("Calibration requires binary outcome")
  if (any(p < 0 | p > 1)) stop("Probabilities must be between 0 and 1")

  # Randomness control in createFolds
  folds <- caret::createFolds(y, k = n_folds, list = TRUE, returnTrain = FALSE)

  calibrated_p <- rep(NA, length(y))

  for (i in seq_along(folds)) {
    test_idx <- folds[[i]]
    train_idx <- setdiff(seq_along(y), test_idx)

    if (method == "isotonic") {
      calibr_model <- earth::earth(
        x = p[train_idx],
        y = y[train_idx],
        degree = 1,
        pmethod = "none"
      )
      calibrated_p[test_idx] <- stats::predict(calibr_model, newdata = data.frame(p = p[test_idx]))
    } else {
      calibr_model <- stats::glm(
        y[train_idx] ~ p[train_idx],
        family = stats::binomial()
      )
      calibrated_p[test_idx] <- stats::predict(calibr_model,
                                               newdata = data.frame(p = p[test_idx]),
                                               type = "response")
    }
  }

  calibrated_p <- pmin(pmax(calibrated_p, 0), 1)
  return(calibrated_p)
}
