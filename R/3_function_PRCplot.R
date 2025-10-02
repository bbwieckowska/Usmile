#' Precision-Recall Curve Plot with AUPRC Comparison
#'
#' Creates a precision-recall plot comparing two models and computes area under the PR curve (AUPRC)
#' with statistical significance testing via bootstrap. Includes baseline prevalence reference.
#'
#' @param data_ref List containing reference model output from \code{USprep_mdl} with elements:
#'   \itemize{
#'     \item y - vector of observed values (0/1)
#'     \item p - vector of predicted probabilities
#'     \item n_vars - number of predictor variables in the model
#'   }
#' @param data_new List containing new model output from \code{USprep_mdl} (same structure as data_ref)
#' @param title Plot title (default: "Precision-Recall plot")
#' @param n_boot Number of bootstrap samples for significance testing (default: 1000)
#' @param seed Random seed for reproducibility (default: 123)
#'
#' @return A list containing:
#' \itemize{
#'   \item plot - ggplot object showing PR curves for both models with baseline prevalence
#'   \item results_table - data frame with performance metrics:
#'     \itemize{
#'       \item AUPRC_ref - Area Under PR Curve for reference model
#'       \item AUPRC_new - Area Under PR Curve for new model
#'       \item AUPRC_diff_p_value - Bootstrap p-value for AUPRC difference
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
#' model_glm_ref <- glm(disease ~ age + sex + bp + chol, data = heart_disease_train, family = "binomial")
#' model_glm_new <- glm(disease ~ age + sex + bp + chol + cp, data = heart_disease_train, family = "binomial")
#'
#' train_out_ref <- USprep_mdl(model_glm_ref, dataset = NULL, testing = FALSE)
#' train_out_new <- USprep_mdl(model_glm_new, dataset = NULL, testing = FALSE)
#'
#' # Create PR curve plot with default settings
#' pr_results <- PRCplot(
#'   data_ref = train_out_ref,
#'   data_new = train_out_new,
#'   title = "PR Curve: Basic vs Extended Cardiac Model"
#' )
#'
#' # Display plot and metrics
#' pr_results$plot
#' pr_results$results_table
#'
#' # Example with test data and custom bootstrap samples
#' test_out_ref <- USprep_mdl(model_glm_ref, dataset = heart_disease_test, testing = TRUE)
#' test_out_new <- USprep_mdl(model_glm_new, dataset = heart_disease_test, testing = TRUE)
#'
#' PRCplot(
#'   data_ref = test_out_ref,
#'   data_new = test_out_new,
#'   n_boot = 500,
#'   seed = 456,
#'   title = "Test Data PR Curves (500 bootstraps)"
#' )
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_hline geom_line scale_color_manual
#' @importFrom ggplot2 scale_linetype_manual labs coord_equal theme_minimal
#' @importFrom ggplot2 theme element_text annotate theme_void
#' @importFrom PRROC pr.curve

  PRCplot <- function(data_ref, data_new, title = "Precision-Recall plot",
                      n_boot = 1000, seed = 123) {
    # Handle case where both models are empty
    if ((data_ref$n_vars == 0) && (data_new$n_vars == 0)) {
      return(list(
        plot = ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "Both models are empty (no independent variables)") +
          ggplot2::theme_void(),
        results_table = NULL
      ))
    }
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

  # Calculate PR curves and AUPRC
  pr_list <- list()
  results_list <- list()

  # Reference model
  if (data_ref$n_vars > 0) {
    pr_ref <- PRROC::pr.curve(scores.class0 = p_ref, weights.class0 = y, curve = TRUE)
    pr_list[["Reference"]] <- pr_ref
    results_list[["AUPRC_ref"]] <- sprintf("%.3f", pr_ref$auc.integral)
  }

  # New model
  if (data_new$n_vars > 0) {
    pr_new <- PRROC::pr.curve(scores.class0 = p_new, weights.class0 = y, curve = TRUE)
    pr_list[["New"]] <- pr_new
    results_list[["AUPRC_new"]] <- sprintf("%.3f", pr_new$auc.integral)
  }

  # AUPRC comparison test
  comparison_p_value <- NA
  if (length(pr_list) == 2) {
    set.seed(seed) # for reproducibility
    boot_diff <- numeric(n_boot)

    for (i in 1:n_boot) {
      idx <- sample(1:length(y), replace = TRUE)
      y_boot <- y[idx]
      p_ref_boot <- p_ref[idx]
      p_new_boot <- p_new[idx]

      pr_ref_boot <- PRROC::pr.curve(scores.class0 = p_ref_boot, weights.class0 = y_boot)
      pr_new_boot <- PRROC::pr.curve(scores.class0 = p_new_boot, weights.class0 = y_boot)

      boot_diff[i] <- pr_new_boot$auc.integral - pr_ref_boot$auc.integral
    }

    observed_diff <- pr_new$auc.integral - pr_ref$auc.integral
    comparison_p_value <- mean(abs(boot_diff) >= abs(observed_diff))
    results_list[["AUPRC_diff_p_value"]] <- sprintf("%.3f", comparison_p_value)
  }

  # Prepare plot data
  pr_data <- data.frame()
  for (model_name in names(pr_list)) {
    pr_df <- data.frame(
      Recall = pr_list[[model_name]]$curve[, 1],
      Precision = pr_list[[model_name]]$curve[, 2],
      Model = model_name
    )
    pr_data <- rbind(pr_data, pr_df)
  }

  # Handle empty models case
  if (nrow(pr_data) == 0) {
    return(list(
      plot = ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "Both models are empty (no independent variables)") +
        ggplot2::theme_void(),
      results_table = NULL
    ))
  }

  # Calculate baseline (class prevalence)
  baseline <- mean(y)

  # Dynamic plot title
  plot_title <- if (!is.na(comparison_p_value)) {
    paste0(title, " (p=", sprintf("%.3f", comparison_p_value), ")")
  } else {
    title
  }

  # Create PR curve plot
  pr_plot <- ggplot2::ggplot(pr_data, ggplot2::aes(x = Recall, y = Precision,
                                                   color = Model, linetype = Model)) +
    ggplot2::geom_hline(yintercept = baseline, color = "grey",
                        linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_line(linewidth = 0.6) +

    ggplot2::scale_color_manual(values = line_colors, breaks = c("Reference", "New")) +
    ggplot2::scale_linetype_manual(values = line_types, breaks = c("Reference", "New")) +

    ggplot2::labs(
      title = plot_title,
      x = "Recall (Sensitivity)",
      y = "Precision (Positive Predictive Value)",
      color = "Model",
      linetype = "Model",
      caption = "Horizontal line: baseline precision corresponding to prevalence"
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

  return(list(plot = pr_plot, results_table = results_table))
}
