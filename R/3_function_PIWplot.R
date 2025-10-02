#' Prediction Improvement/Worsening (PIW) Plot
#'
#' Creates a scatter plot comparing predicted probabilities between a reference model and a new model,
#' highlighting where predictions improved or worsened for events and non-events.
#'
#' @param data_ref List containing reference model output from \code{USprep_mdl} with elements:
#'   \itemize{
#'     \item y - vector of observed values (0/1)
#'     \item p - vector of predicted probabilities
#'     \item n_vars - number of predictor variables in the model
#'   }
#' @param data_new List containing new model output from \code{USprep_mdl} (same structure as data_ref)
#' @param title Plot title (default: "PIW Plot")
#'
#' @return A ggplot object showing:
#' \itemize{
#'   \item Points colored by prediction change direction (improvement/worsening) and event status
#'   \item Diagonal reference line (y = x) representing no change between models
#'   \item Consistent color scheme with USplot (blue for non-events, red for events)
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
#' # Create PIW plot for training data
#' PIWplot(data_ref = train_out_ref, data_new = train_out_new,
#'         title = "Prediction Changes: Adding CP to Cardiac Risk Model")
#'
#' # Example with test data
#' test_out_ref <- USprep_mdl(model_glm_ref, dataset = heart_disease_test, testing = TRUE)
#' test_out_new <- USprep_mdl(model_glm_new, dataset = heart_disease_test, testing = TRUE)
#'
#' PIWplot(data_ref = test_out_ref, data_new = test_out_new,
#'         title = "Test Data Prediction Changes")
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_abline geom_point scale_color_manual
#' @importFrom ggplot2 scale_fill_manual labs coord_equal theme_minimal theme
#' @importFrom ggplot2 element_text unit guide_legend guides annotate theme_void
PIWplot <- function(data_ref, data_new, title = "PIW Plot") {
  # Check if both models are empty (no independent variables)
  if ((data_ref$n_vars == 0) && (data_new$n_vars == 0)) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = "Both models are empty (no independent variables)") +
             ggplot2::theme_void())
  }

  # Validate input data
  if (length(data_ref$y) != length(data_new$y)) {
    stop("Reference and new data have different number of observations")
  }

  if (!all(data_ref$y == data_new$y)) {
    stop("Mismatch in y values between reference and new data")
  }

  # Prepare plot data
  plot_data <- data.frame(
    y = data_ref$y,
    p_ref = data_ref$p,
    p_new = data_new$p,
    delta = data_new$p - data_ref$p
  )

  # Define subclasses with full labels
  plot_data$subclass <- factor(
    ifelse(plot_data$y == 0 & plot_data$delta < 0, 'nonev_be',
           ifelse(plot_data$y == 0 & plot_data$delta > 0, 'nonev_wo',
                  ifelse(plot_data$y == 1 & plot_data$delta < 0, 'event_wo',
                         ifelse(plot_data$y == 1 & plot_data$delta > 0, 'event_be', 'unknown')))),
    levels = c('nonev_be', 'nonev_wo', 'event_wo', 'event_be'),
    labels = c('non-events with better prediction',
               'non-events with worse prediction',
               'events with worse prediction',
               'events with better prediction')
  )

  # Color scheme consistent with USplot
  usmile_colors <- c(
    'non-events with better prediction' = '#0F3C78',
    'non-events with worse prediction' = '#0F3C78',
    'events with worse prediction' = '#D51424',
    'events with better prediction' = '#D51424'
  )

  usmile_fills <- c(
    'non-events with better prediction' = '#0F3C78',
    'non-events with worse prediction' = '#BED2FA',
    'events with worse prediction' = '#FBCDB9',
    'events with better prediction' = '#D51424'
  )

  # Create scatter plot
  piw_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = p_ref, y = p_new)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = 'grey45',
                         linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(color = subclass, fill = subclass),
                        shape = 21, size = 2.5, alpha = 0.7, stroke = 0.8) +

    # Color scales with full labels
    ggplot2::scale_color_manual(
      name = "subclass",
      values = usmile_colors,
      drop = FALSE
    ) +
    ggplot2::scale_fill_manual(
      name = "subclass",
      values = usmile_fills,
      drop = FALSE
    ) +

    # Labels and title
    ggplot2::labs(
      title = title,
      x = "Reference model probability (p_ref)",
      y = "New model probability (p_new)",
      caption = "Line y = x represents perfect aggregates of models"
    ) +

    # Axis limits
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = TRUE) +

    # Plot style
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold"),
      legend.spacing.y = ggplot2::unit(0.8, "cm"),
      legend.text = ggplot2::element_text(size = 10),
      aspect.ratio = 1,
      plot.caption = ggplot2::element_text(hjust = 0.5, color = "grey40")
    ) +

    # Legend customization
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        override.aes = list(size = 4, alpha = 1)
      ),
      fill = ggplot2::guide_legend(
        title.position = "top",
        title.hjust = 0.5
      )
    )

  return(piw_plot)
}
