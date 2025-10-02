#' Prepare data for model evaluation for various binary classifiers
#'
#' This function prepares data for evaluating various binary classification models
#' including Logistic Regression, Random Forest, SVM, XGBoost, Neural Networks, and Naive Bayes.
#' Supports both training and test data with optional probability calibration.
#'
#' @param model Model object (supported classes: 'glm', 'randomForest', 'ranger', 'svm', 'xgb.Booster',
#'                            'nnet', 'naive_bayes', 'model_fit' from tidymodels)
#' @param dataset Dataset used to prepare data for the models.
#'   - If `testing = TRUE`, a test dataset must be provided (required).
#'   - If `testing = FALSE`, the argument can be:
#'       * `NULL` if the model stores its own training data (e.g., `glm`, `randomForest`),
#'       * the training dataset if the model does not keep training data internally
#'         (e.g., `ranger`, `e1071`, `xgboost`, `nnet`, `naive_bayes`, tidymodels).
#'   This allows the function to work consistently with both training and test data.
#' @param testing Whether to prepare test data (TRUE) or training data (FALSE)
#' @param calibrate Whether to calibrate probabilities (default: FALSE)
#' @param calibration_method Calibration method ("isotonic" or "logistic")
#'
#' @return A list containing:
#' \itemize{
#'   \item y - vector of observed values (0/1)
#'   \item p - vector of predicted probabilities for positive class
#'   \item n_vars - number of predictor variables in the model
#'   \item var_names - names of predictor variables in the model
#' }
#'
#' @note This function requires the following packages to be installed for specific model types:
#' \itemize{
#'   \item \code{randomForest} - for randomForest models
#'   \item \code{ranger} - for ranger models
#'   \item \code{e1071} - for SVM models
#'   \item \code{xgboost} - for XGBoost models
#'   \item \code{nnet} - for Neural Network models
#'   \item \code{parsnip}, \code{workflows} - for tidymodels models
#'   \item \code{naivebayes} - for Naive Bayes models
#' }
#' The function will load these packages automatically when needed.
#'
#' @examples
#' \dontrun{
#' # Load required packages
#' library(tidyverse)
#' library(randomForest)
#' library(ranger)
#' library(e1071)
#' library(xgboost)
#' library(nnet)
#' library(parsnip)
#' library(workflows)
#' library(naivebayes)
#'
#' # Load Heart Disease datasets
#' data(heart_disease)
#' data(heart_disease_train)
#' data(heart_disease_test)
#'
#' # For Logistic Regression model (glm)
#'
#' # For logistic regression model (reference model with age, sex, bp, chol)
#' model_glm_ref <- glm(disease ~ age + sex + bp + chol, data = heart_disease_train, family = "binomial")
#' train_out_glm_ref <- USprep_mdl(model_glm_ref, dataset = NULL, testing = FALSE)
#' test_out_glm_ref <- USprep_mdl(model_glm_ref, dataset = heart_disease_test, testing = TRUE)
#'
#' # For Random Forest model (randomForest package)
#' model_rf_ref <- randomForest::randomForest(disease ~ age + sex + bp + chol, data = heart_disease_train)
#' train_out_rf_ref <- USprep_mdl(model_rf_ref, dataset = NULL, testing = FALSE)
#' test_out_rf_ref <- USprep_mdl(model_rf_ref, dataset = heart_disease_test, testing = TRUE)
#'
#' # For Random Forest model (ranger package)
#' model_ranger <- ranger(disease ~ age + sex + bp + chol, data = heart_disease_train,
#'                       probability = TRUE, keep.inbag = TRUE)
#' train_result_ranger <- USprep_mdl(model_ranger, dataset = heart_disease_train, testing = FALSE)
#'
#' # For SVM model (e1071 package)
#' model_svm <- svm(disease ~ age + sex + bp + chol, data = heart_disease_train,
#'                 probability = TRUE, kernel = "radial")
#' train_result_svm <- USprep_mdl(model_svm, dataset = heart_disease_train, testing = FALSE)
#'
#' # For Neural Network model (nnet package)
#' train_data_scaled <- heart_disease_train
#' numeric_vars <- c("age", "bp", "chol")
#' train_data_scaled[numeric_vars] <- scale(heart_disease_train[numeric_vars])
#' model_formula <- disease ~ age + sex + bp + chol
#' model_matrix_train <- model.matrix(model_formula, data = train_data_scaled)[,-1]
#' model_nnet <- nnet(x = model_matrix_train,
#'                   y = as.numeric(train_data_scaled$disease) - 1,
#'                   size = 10, maxit = 1000, linout = FALSE, entropy = TRUE, trace = TRUE)
#' model_matrix_with_y <- as.data.frame(model_matrix_pred)
#' model_matrix_with_y$disease <- as.numeric(train_data_scaled$disease) - 1
#' train_result_nnet <- USprep_mdl(model_nnet, dataset = model_matrix_with_y, testing = FALSE)
#'
#' # For Naive Bayes model (naivebayes package)
#' model_nb <- naive_bayes(disease ~ age + sex + bp + chol, data = heart_disease_train)
#' train_result_nb <- USprep_mdl(model_nb, dataset = heart_disease_train, testing = FALSE)
#'
#' # For tidymodels/parsnip models
#' model_spec <- logistic_reg() %>% set_engine("glm") %>% set_mode("classification")
#' model_tidymodels <- model_spec %>% fit(disease ~ age + sex + bp + chol, data = heart_disease_train)
#' train_result_tidy <- USprep_mdl(model_tidymodels, dataset = NULL, testing = FALSE)
#'
#' # With calibration
#' model_for_calib <- glm(disease ~ age + sex + bp + chol, data = heart_disease_train, family = binomial)
#' train_result_calibrated <- USprep_mdl(model_for_calib, dataset = NULL, testing = FALSE,
#'   calibrate = TRUE, calibration_method = "isotonic")
#' }
#' @export
#' @importFrom stats predict coef model.frame terms formula
#' @importFrom randomForest randomForest

USprep_mdl <- function(model, dataset = NULL, testing = FALSE, calibrate = FALSE,
                       calibration_method = "isotonic") {

  # Validate inputs
  if (testing && is.null(dataset)) {
    stop("Dataset must be provided when testing = TRUE")
  }

  # Initialize variables
  y <- NULL
  p <- NULL
  var_names <- NULL
  n_vars <- NULL

  # Get target variable name from model formula
  get_target_name <- function(model) {
    if (!is.null(model$terms)) {
      return(as.character(model$terms[[2]]))
    } else if (!is.null(model$call)) {
      form <- as.character(model$call)[2]
      return(strsplit(form, "~")[[1]][1] %>% trimws())
    } else if (inherits(model, "model_fit")) {
      return(as.character(model$preproc$y_var))
    } else if (inherits(model, "xgb.Booster") && !is.null(model$params)) {
      # Try to extract from xgboost call
      if (!is.null(model$call)) {
        form <- as.character(model$call)[2]
        if (grepl("~", form)) {
          return(strsplit(form, "~")[[1]][1] %>% trimws())
        }
      }
    }
    return(NULL)
  }

  # Get predictor names from model
  get_predictor_names <- function(model) {
    if (!is.null(model$terms)) {
      return(attr(model$terms, "term.labels"))
    } else if (!is.null(model$call) && inherits(model, "randomForest")) {
      form <- as.character(model$call)[2]
      rhs <- strsplit(form, "~")[[1]][2]
      return(trimws(strsplit(rhs, "\\+")[[1]]))
    } else if (inherits(model, "model_fit")) {
      return(model$preproc$x_var)
    } else if (inherits(model, "ranger")) {
      return(model$forest$independent.variable.names)
    } else if (!is.null(model$feature_names)) {
      return(model$feature_names)
    } else if (inherits(model, "xgb.Booster")) {
      return(model$feature_names)
    }
    return(NULL)
  }

  # Get training data from model if available
  get_training_data <- function(model) {
    if (!is.null(model$model)) {
      return(model$model)
    } else if (!is.null(model$data)) {
      return(model$data)
    } else if (inherits(model, "model_fit") && !is.null(model$fit$data)) {
      return(model$fit$data)
    }
    return(NULL)
  }

  # 1. TIDYMODELS (parsnip) models
  if (inherits(model, "model_fit")) {
    if (testing) {
      target_name <- get_target_name(model)
      y <- dataset[[target_name]]
      p <- predict(model, new_data = dataset, type = "prob")$.pred_1
    } else {
      # Try to get training data from model
      training_data <- get_training_data(model)
      if (is.null(training_data) && !is.null(dataset)) {
        training_data <- dataset
      }
      if (is.null(training_data)) {
        stop("Training data not available in model and dataset not provided")
      }
      target_name <- get_target_name(model)
      y <- training_data[[target_name]]
      p <- predict(model, new_data = training_data, type = "prob")$.pred_1
    }
    var_names <- get_predictor_names(model)
    n_vars <- length(var_names)

    # 2. LOGISTIC REGRESSION (glm)
  } else if (inherits(model, "glm")) {
    if (testing) {
      target_name <- names(model$model)[1]
      y <- dataset[[target_name]]
      p <- predict(model, newdata = dataset, type = "response")
    } else {
      # Use model's stored data or provided dataset
      if (!is.null(model$model)) {
        y <- model$y
        p <- model$fitted.values
      } else if (!is.null(dataset)) {
        target_name <- get_target_name(model)
        y <- dataset[[target_name]]
        p <- predict(model, newdata = dataset, type = "response")
      } else {
        stop("Training data not available in glm model and dataset not provided")
      }
    }
    coef_names <- names(coef(model))
    var_names <- setdiff(coef_names, "(Intercept)")
    n_vars <- length(var_names)

    # 3. RANDOM FOREST (randomForest package)
  } else if (inherits(model, "randomForest")) {
    target_name <- get_target_name(model)

    if (testing) {
      y <- dataset[[target_name]]
      p <- predict(model, newdata = dataset, type = "prob")
      if (is.matrix(p) && ncol(p) == 2) p <- p[, 2]
    } else {
      # Try to use model's stored data or provided dataset
      if (!is.null(model$y)) {
        y <- model$y
        p <- predict(model, type = "prob")
        if (is.matrix(p) && ncol(p) == 2) p <- p[, 2]
      } else if (!is.null(dataset)) {
        y <- dataset[[target_name]]
        p <- predict(model, newdata = dataset, type = "prob")
        if (is.matrix(p) && ncol(p) == 2) p <- p[, 2]
      } else {
        stop("Training data not available in randomForest model and dataset not provided")
      }
    }
    var_names <- get_predictor_names(model)
    n_vars <- length(var_names)

    # 4. RANDOM FOREST (ranger package)
  } else if (inherits(model, "ranger")) {
    target_name <- get_target_name(model)

    if (testing) {
      y <- dataset[[target_name]]
      pred <- predict(model, data = dataset)
      p <- pred$predictions
      if (is.matrix(p) && ncol(p) == 2) p <- p[, 2]
    } else {
      # For ranger, try to use provided dataset or model's data
      if (!is.null(dataset)) {
        y <- dataset[[target_name]]
        pred <- predict(model, data = dataset)
        p <- pred$predictions
        if (is.matrix(p) && ncol(p) == 2) p <- p[, 2]
      } else if (!is.null(model$data)) {
        y <- model$data[[target_name]]
        p <- model$predictions
        if (is.matrix(p) && ncol(p) == 2) p <- p[, 2]
      } else {
        stop("For ranger models, training data must be provided via dataset parameter")
      }
    }
    var_names <- get_predictor_names(model)
    n_vars <- length(var_names)

    # 5. SVM (e1071 package)
  } else if (inherits(model, "svm")) {
    target_name <- get_target_name(model)

    if (testing) {
      y <- dataset[[target_name]]
      svm_pred <- predict(model, newdata = dataset, probability = TRUE)
      p <- attr(svm_pred, "probabilities")[, 2]
    } else {
      # For training data, try different approaches
      if (!is.null(model$y)) {
        y <- model$y
        svm_pred <- predict(model, probability = TRUE)
        p <- attr(svm_pred, "probabilities")[, 2]
      } else if (!is.null(dataset)) {
        y <- dataset[[target_name]]
        svm_pred <- predict(model, newdata = dataset, probability = TRUE)
        p <- attr(svm_pred, "probabilities")[, 2]
      } else {
        stop("Training data not available in svm model and dataset not provided")
      }
    }
    var_names <- get_predictor_names(model)
    n_vars <- length(var_names)

    # 6. XGBOOST
  } else if (inherits(model, "xgb.Booster")) {
    target_name <- get_target_name(model)

    if (is.null(target_name) && testing) {
      stop("Target variable name could not be extracted from xgboost model. Please ensure the model was trained with a formula.")
    }

    if (testing) {
      y <- dataset[[target_name]]
      # Convert data to matrix format for xgboost
      features <- dataset[setdiff(names(dataset), target_name)]
      features_matrix <- as.matrix(features)
      p <- predict(model, newdata = features_matrix)
    } else {
      # For training data, xgboost doesn't store the original data
      if (!is.null(dataset)) {
        y <- dataset[[target_name]]
        features <- dataset[setdiff(names(dataset), target_name)]
        features_matrix <- as.matrix(features)
        p <- predict(model, newdata = features_matrix)
      } else {
        stop("For xgboost training data, dataset must be provided")
      }
    }
    var_names <- get_predictor_names(model)
    n_vars <- length(var_names)

    # 7. NEURAL NETWORK (nnet)
  } else if (inherits(model, "nnet")) {
    # For nnet models, we need special handling as they don't store formula well
    if (testing) {
      # Check if dataset has the target variable
      if ("disease" %in% names(dataset)) {
        y <- dataset[["disease"]]
        p <- predict(model, newdata = dataset, type = "raw")
      } else {
        stop("Target variable 'disease' not found in dataset for nnet model")
      }
    } else {
      # For training data with nnet
      if (!is.null(dataset)) {
        # Check if dataset has the target variable
        if ("disease" %in% names(dataset)) {
          y <- dataset[["disease"]]
          p <- predict(model, newdata = dataset, type = "raw")
        } else {
          stop("Target variable 'disease' not found in dataset for nnet model")
        }
      } else {
        stop("Training data not available in nnet model and dataset not provided")
      }
    }

    # For nnet, extract variable names from the model matrix if possible
    if (!is.null(model$coefnames)) {
      var_names <- model$coefnames
    } else if (!is.null(colnames(dataset)) && "disease" %in% colnames(dataset)) {
      var_names <- setdiff(colnames(dataset), "disease")
    } else {
      var_names <- NULL
    }
    n_vars <- length(var_names)

    # 8. NAIVE BAYES
  } else if (inherits(model, "naive_bayes")) {
    target_name <- get_target_name(model)

    if (is.null(target_name)) {
      stop("Could not extract target variable name from naive_bayes model")
    }

    if (testing) {
      y <- dataset[[target_name]]
      nb_pred <- predict(model, newdata = dataset, type = "prob")
      # Handle different output formats from naive_bayes
      if (is.matrix(nb_pred) && ncol(nb_pred) >= 2) {
        # Assuming the second column is the positive class
        p <- nb_pred[, 2]
      } else if (is.list(nb_pred)) {
        # Some versions return a list
        p <- nb_pred[[2]]
      } else {
        stop("Unexpected output format from naive_bayes predict function")
      }
    } else {
      # For training data, naive_bayes doesn't store y directly
      # We must use the provided dataset
      if (!is.null(dataset)) {
        y <- dataset[[target_name]]
        nb_pred <- predict(model, newdata = dataset, type = "prob")
        if (is.matrix(nb_pred) && ncol(nb_pred) >= 2) {
          p <- nb_pred[, 2]
        } else if (is.list(nb_pred)) {
          p <- nb_pred[[2]]
        } else {
          stop("Unexpected output format from naive_bayes predict function")
        }
      } else {
        stop("For naive_bayes training data, dataset must be provided")
      }
    }

    # Extract variable names from naive_bayes model
    if (!is.null(model$tables)) {
      var_names <- names(model$tables)
      n_vars <- length(var_names)
    } else if (!is.null(model$call)) {
      # Fallback: extract from formula
      var_names <- get_predictor_names(model)
      n_vars <- length(var_names)
    } else {
      var_names <- NULL
      n_vars <- 0
    }

  } else {
    stop("Unsupported model type. Supported classes: glm, randomForest, ranger, svm, xgb.Booster, nnet, naive_bayes, model_fit")
  }

  # Ensure y is numeric 0/1
  if (is.factor(y)) {
    y_levels <- levels(y)
    if (length(y_levels) == 2) {
      y <- as.numeric(y) - 1
    } else {
      # For multi-class, use the second level as positive class (common convention)
      y <- as.numeric(y == y_levels[2])
    }
  }

  # Ensure probabilities are between 0 and 1
  p <- pmin(pmax(p, 0), 1)

  # Apply calibration if requested
  if (calibrate) {
    if (!exists("USclbr_mdl")) {
      warning("Calibration requested but USclbr_mdl function not found. Skipping calibration.")
    } else {
      p <- USclbr_mdl(y, p, method = calibration_method)
    }
  }

  return(list(y = y, p = p, n_vars = n_vars, var_names = var_names))
}
