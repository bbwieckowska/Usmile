#' Receiver Operating Characteristic (ROC) Curve Plot with AUC Comparison
#'
#' Creates an ROC plot comparing two models and computes area under the curve (AUC)
#' with statistical significance testing using DeLong's method. Includes reference
#' line for random classifier performance.
#'
#' @param data_ref List containing reference model output from \code{USprep_mdl} with elements:
#'   \itemize{
#'     \item y - vector of observed values (0/1)
#'     \item p - vector of predicted probabilities
#'     \item n_vars - number of predictor variables in the model
#'   }
#' @param data_new List containing new model output from \code{USprep_mdl} (same structure as data_ref)
#' @param title Plot title (default: "ROC plot")
#' @param alternative Alternative hypothesis for DeLong's test ("two.sided", "greater", or "less")
#'                   (default: "two.sided")
#'
#' @return A list containing:
#' \itemize{
#'   \item plot - ggplot object showing ROC curves with diagonal reference line
#'   \item results_table - data frame with performance metrics:
#'     \itemize{
#'       \item AUC_ref - AUC for reference model
#'       \item AUC_p-value_ref - p-value for reference model AUC vs random
#'       \item AUC_new - AUC for new model
#'       \item AUC_p-value_new - p-value for new model AUC vs random
#'       \item AUC_diff_p_value - p-value for AUC difference (DeLong's test)
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # Load Heart Disease datasets
#' data(heart_disease)
#' data(heart_disease_train)
#' data(heart_disease_test)
#'
#' # Prepare model outputs
#' model_glm_ref <- glm(disease ~ age + sex + bp + chol, data = train_data, family = "binomial")
#' model_glm_new <- glm(disease ~ age + sex + bp + chol + cp, data = train_data, family = "binomial")
#'
#' train_out_ref <- USprep_mdl(model_glm_ref, dataset = NULL, testing = FALSE)
#' train_out_new <- USprep_mdl(model_glm_new, dataset = NULL, testing = FALSE)
#'
#' # Create ROC plot with default two-sided test
#' roc_results <- ROCplot(
#'   data_ref = train_out_ref,
#'   data_new = train_out_new,
#'   title = "ROC: Basic vs Extended Cardiac Model"
#' )
#'
#' # Display plot and metrics
#' roc_results$plot
#' roc_results$results_table
#'
#' # Example with one-sided test (testing if new model is better)
#' ROCplot(
#'   data_ref = train_out_ref,
#'   data_new = train_out_new,
#'   alternative = "greater",
#'   title = "ROC Curve (New > Reference)"
#' )
#'
#' # Example with test data
#' test_out_ref <- USprep_mdl(model_glm_ref, dataset = test_data, testing = TRUE)
#' test_out_new <- USprep_mdl(model_glm_new, dataset = test_data, testing = TRUE)
#'
#' ROCplot(
#'   data_ref = test_out_ref,
#'   data_new = test_out_new,
#'   title = "Test Data ROC Curves"
#' )
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_abline geom_line scale_color_manual
#' @importFrom ggplot2 scale_linetype_manual labs coord_equal theme_minimal
#' @importFrom ggplot2 theme element_text annotate theme_void
#' @importFrom pROC roc roc.test var
#' @importFrom stats pnorm
ROCplot <- function(data_ref, data_new, title = "ROC plot",
                                alternative = "two.sided") {
  # Validate input data
  if (length(data_ref$y) != length(data_new$y)) {
    stop("Reference and new data have different number of observations")
  }

  if (!all(data_ref$y == data_new$y)) {
    stop("Mismatch in y values between reference and new data")
  }

  # Prepare data
  y <- data_ref$y
  p_ref <- data_ref$p
  p_new <- data_new$p

  # Plot aesthetics
  line_colors <- c("#2F4F4F", "#2F4F4F")  # Both curves same color
  line_types <- c("dotted", "solid")      # Different line styles

  # Calculate ROC curves and AUC
  roc_list <- list()
  results_list <- list()

  # Reference model
  if (data_ref$n_vars > 0) {
    roc_ref <- pROC::roc(response = y, predictor = p_ref)
    roc_list[["Reference"]] <- roc_ref

    # Test AUC vs 0.5
    se <- sqrt(pROC::var(roc_ref))  # Standard error of AUC
    z_score <- (roc_ref$auc - 0.5)/se
    p_value <- 2 * stats::pnorm(-abs(z_score))  # Two-tailed p-value

    results_list[["AUC_ref"]] <- sprintf("%.3f", roc_ref$auc)
    results_list[["AUC_p-value_ref"]] <- sprintf("%.3f", p_value)
  }

  # New model
  if (data_new$n_vars > 0) {
    roc_new <- pROC::roc(response = y, predictor = p_new)
    roc_list[["New"]] <- roc_new

    # Test AUC vs 0.5
    se <- sqrt(pROC::var(roc_new))
    z_score <- (roc_new$auc - 0.5)/se
    p_value <- 2 * stats::pnorm(-abs(z_score))

    results_list[["AUC_new"]] <- sprintf("%.3f", roc_new$auc)
    results_list[["AUC_p-value_new"]] <- sprintf("%.3f", p_value)
  }

  # ROC comparison test
  comparison_p_value <- NA
  if (length(roc_list) == 2) {
    # Compare two models using DeLong's test
    comparison_test <- pROC::roc.test(
      roc1 = roc_list[[1]],
      roc2 = roc_list[[2]],
      paired = TRUE,
      alternative = alternative
    )
    comparison_p_value <- comparison_test$p.value
    results_list[["AUC_diff_p_value"]] <- sprintf("%.3f", comparison_p_value)
  } else if (length(roc_list) == 1) {
    # Single model - compare AUC to 0.5
    single_model <- roc_list[[1]]
    se <- sqrt(pROC::var(single_model))
    z_score <- (single_model$auc - 0.5)/se
    comparison_p_value <- 2 * stats::pnorm(-abs(z_score))
    results_list[["AUC_vs_random_p_value"]] <- sprintf("%.3f", comparison_p_value)
  }

  # Prepare plot data
  roc_data <- data.frame()
  for (model_name in names(roc_list)) {
    roc_df <- data.frame(
      Sensitivity = roc_list[[model_name]]$sensitivities,
      Specificity = roc_list[[model_name]]$specificities,
      Model = model_name
    )
    roc_data <- rbind(roc_data, roc_df)
  }

  # Handle empty models case
  if (nrow(roc_data) == 0) {
    return(list(
      plot = ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "Both models are empty (no independent variables)") +
        ggplot2::theme_void(),
      results_table = NULL
    ))
  }

  # Dynamic plot title
  plot_title <- if (!is.na(comparison_p_value)) {
    paste0(title, " (p=", sprintf("%.3f", comparison_p_value), ")")
  } else {
    title
  }

  # Create ROC plot
  roc_plot <- ggplot2::ggplot(
    roc_data,
    ggplot2::aes(x = 1 - Specificity, y = Sensitivity, color = Model, linetype = Model)
  ) +
    ggplot2::geom_abline(
      intercept = 0, slope = 1,
      color = "grey", linetype = "dashed", linewidth = 0.6
    ) +
    ggplot2::geom_line(linewidth = 0.6) +

    ggplot2::scale_color_manual(
      values = line_colors,
      breaks = c("Reference", "New")
    ) +
    ggplot2::scale_linetype_manual(
      values = line_types,
      breaks = c("Reference", "New")
    ) +

    ggplot2::labs(
      title = plot_title,
      x = "1 - Specificity (False Positive Rate)",
      y = "Sensitivity (True Positive Rate)",
      color = "Model",
      linetype = "Model",
      caption = "Diagonal line: performance of random classifier"
    ) +

    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = TRUE) +

    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      aspect.ratio = 1,
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.caption = ggplot2::element_text(hjust = 0.5, color = "grey40")
    )

  # Prepare results table
  results_table <- data.frame(
    Metric = names(results_list),
    Value = unlist(results_list),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  return(list(plot = roc_plot, results_table = results_table))
}
