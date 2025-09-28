#' Calibration Plot with Brier Score Comparison
#'
#' Creates a calibration plot comparing predicted vs. empirical probabilities for two models,
#' along with a table of Brier score metrics. The plot shows how well predicted probabilities
#' match observed event rates across probability bins.
#'
#' @param data_ref List containing reference model output from \code{USprep_mdl} with elements:
#'   \itemize{
#'     \item y - vector of observed values (0/1)
#'     \item p - vector of predicted probabilities
#'     \item n_vars - number of predictor variables in the model
#'   }
#' @param data_new List containing new model output from \code{USprep_mdl} (same structure as data_ref)
#' @param title Plot title (default: "Calibration Plot")
#' @param n_bins Number of bins for calibration calculation (default: 10)
#'
#' @return A list containing:
#' \itemize{
#'   \item plot - ggplot object showing calibration curves for both models
#'   \item results_table - data frame with Brier score metrics:
#'     \itemize{
#'       \item Brier_ref - Brier score for reference model
#'       \item Brier_new - Brier score for new model
#'       \item Delta_Brier - Improvement in Brier score (reference - new)
#'       \item BSS - Brier Skill Score (relative improvement)
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
#' # Create calibration plot for training data
#' calibration_results <- CLBplot(
#'   data_ref = train_out_ref,
#'   data_new = train_out_new,
#'   title = "Calibration: Basic vs Extended Cardiac Risk Model"
#' )
#'
#' # Display plot and metrics
#' calibration_results$plot
#' calibration_results$results_table
#'
#' # Example with test data and custom bins
#' test_out_ref <- USprep_mdl(model_glm_ref, dataset = test_data, testing = TRUE)
#' test_out_new <- USprep_mdl(model_glm_new, dataset = test_data, testing = TRUE)
#'
#' CLBplot(
#'   data_ref = test_out_ref,
#'   data_new = test_out_new,
#'   n_bins = 5,
#'   title = "Test Data Calibration (5 bins)"
#' )
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_abline geom_line geom_point scale_color_manual
#' @importFrom ggplot2 scale_linetype_manual scale_shape_manual labs coord_equal
#' @importFrom ggplot2 theme_minimal theme element_text annotate theme_void
CLBplot <- function(data_ref, data_new, title = "Calibration Plot", n_bins = 10) {
  # Validate input data
  if (length(data_ref$y) != length(data_new$y)) {
    stop("Reference and new data have different number of observations")
  }

  if (!all(data_ref$y == data_new$y)) {
    stop("Mismatch in y values between reference and new data")
  }

  y <- data_ref$y
  p_ref <- data_ref$p
  p_new <- data_new$p

  # Set plot aesthetics
  line_colors <- c("#2F4F4F", "#2F4F4F")  # Both curves same color
  line_types <- c("dotted", "solid")      # Different line styles

  # Helper function for calibration calculations
  calculate_calibration <- function(p, y, model_name, n_bins) {
    bins <- cut(p, breaks = seq(0, 1, length.out = n_bins + 1),
                       include.lowest = TRUE)
    bin_centers <- (seq(0, 1, length.out = n_bins + 1)[-1] +
                      seq(0, 1, length.out = n_bins + 1)[-(n_bins + 1)]) / 2
    empirical_probs <- tapply(y, bins, mean)
    predicted_probs <- tapply(p, bins, mean)

    data.frame(
      Predicted = predicted_probs,
      Empirical = empirical_probs,
      BinCenter = bin_centers[1:length(predicted_probs)],
      Model = model_name
    )
  }

  calibration_data <- data.frame()

  # Calculate calibration for reference model
  if (data_ref$n_vars > 0) {
    calibration_ref <- calculate_calibration(p_ref, y, "Reference", n_bins)
    calibration_data <- rbind(calibration_data, calibration_ref)
  }

  # Calculate calibration for new model
  if (data_new$n_vars > 0) {
    calibration_new <- calculate_calibration(p_new, y, "New", n_bins)
    calibration_data <- rbind(calibration_data, calibration_new)
  }

  # Handle empty models case
  if (nrow(calibration_data) == 0) {
    return(list(
      plot = ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "Both models are empty (no independent variables)") +
        ggplot2::theme_void(),
      results_table = NULL
    ))
  }

  # Calculate Brier scores
  brier_ref <- if (data_ref$n_vars > 0) mean((p_ref - y)^2) else NA
  brier_new <- if (data_new$n_vars > 0) mean((p_new - y)^2) else NA
  delta_brier <- if (!is.na(brier_ref) && !is.na(brier_new)) brier_ref - brier_new else NA
  bss <- if (!is.na(delta_brier) && !is.na(brier_ref) && brier_ref > 0) delta_brier / brier_ref else NA

  # Prepare results table
  results_list <- list(
    "Brier_ref" = if (!is.na(brier_ref)) sprintf("%.3f", brier_ref) else "NA",
    "Brier_new" = if (!is.na(brier_new)) sprintf("%.3f", brier_new) else "NA",
    "Delta_Brier" = if (!is.na(delta_brier)) sprintf("%.3f", delta_brier) else "NA",
    "BSS" = if (!is.na(bss)) sprintf("%.3f", bss) else "NA"
  )

  # Create calibration plot
  calibration_plot <- ggplot2::ggplot(
    calibration_data,
    ggplot2::aes(x = Predicted, y = Empirical, color = Model, linetype = Model)
  ) +
    ggplot2::geom_abline(
      intercept = 0, slope = 1, color = "grey",
      linetype = "dashed", linewidth = 0.6
    ) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 2, ggplot2::aes(shape = Model)) +

    ggplot2::scale_color_manual(values = line_colors, breaks = c("Reference", "New")) +
    ggplot2::scale_linetype_manual(values = line_types, breaks = c("Reference", "New")) +
    ggplot2::scale_shape_manual(values = c(16, 17), breaks = c("Reference", "New")) +

    ggplot2::labs(
      title = title,
      x = "Predicted Probability",
      y = "Empirical Probability",
      color = "Model",
      linetype = "Model",
      shape = "Model",
      caption = "Line y = x represents perfect agreement between predicted probability and reality"
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

  return(list(plot = calibration_plot, results_table = results_table))
}
