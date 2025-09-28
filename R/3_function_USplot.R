#' U-Smile plot visualization
#'
#' Creates a U-Smile plot showing the performance comparison of models. Models can be of different types (e.g., glm vs randomForest).
#' The function offers multiple ways to generate the plot:
#' 1. From pre-calculated plot data (output from UScalc_mdl or UScalc_raw)
#' 2. From raw data (y, p_ref, p columns)
#' 3. Directly from model specifications (formulas and model types)
#'
#' For model specifications, you can choose to evaluate on training or test data.
#'
#' @param plot_data List containing pre-calculated plot data (output from UScalc_mdl or UScalc_raw)
#' @param y_coef Type of coefficient being plotted ("rLR", "BA", or "RB")
#' @param raw_data Optional raw data frame containing columns: y, p_ref, p (alternative to plot_data)
#' @param ref_formula Optional formula for reference model
#' @param new_formula Optional formula for new model
#' @param ref_model_type Type of reference model ("glm" or "randomForest")
#' @param new_model_type Type of new model ("glm" or "randomForest")
#' @param train_data Training dataset for model building
#' @param test_data Optional test dataset for evaluation
#' @param testing Logical indicating whether to evaluate on test data (TRUE) or training data (FALSE)
#' @param ref_calibrate Whether to calibrate probabilities for reference model (randomForest only)
#' @param new_calibrate Whether to calibrate probabilities for new model (randomForest only)
#' @param calibration_method Calibration method ("isotonic" or "logistic")
#' @param n_vars_diff Difference in number of variables (degrees of freedom) - required if raw_data is provided
#' @param circle_sizes Logical indicating whether to scale point sizes by proportion (default: TRUE)
#' @param y_lim Optional vector specifying y-axis limits (default: NULL for automatic)
#' @param net Logical indicating whether to show net coefficients (default: FALSE)
#' @param crit Criteria for line types (0: solid gray, 2: conditional dotted/solid for rLR)
#'
#' @return A ggplot object showing the U-Smile plot with performance metrics
#'
#' @examples
#' \dontrun{
#' # Load Heart Disease datasets
#' data(heart_disease)
#' data(heart_disease_train)
#' data(heart_disease_test)
#'
#' # Building models
#' model_glm_ref <- glm(disease ~ age + sex + bp + chol, data = train_data, family = "binomial")
#' model_glm_new <- glm(disease ~ age + sex + bp + chol + cp, data = train_data, family = "binomial")
#'
#' # Example 1: Using pre-calculated plot data
#' train_results <- UScalc_mdl(model_glm_ref, model_glm_new, y_coef = "rLR")
#' USplot(plot_data = train_results$plot_data, y_coef = "rLR")
#'
#' # Example 2a: Using raw data
#' train_out_glm_ref <- USprep_mdl(model_glm_ref, dataset = NULL, testing = FALSE)
#' train_out_glm_new <- USprep_mdl(model_glm_new, dataset = NULL, testing = FALSE)
#'
#' # Combine raw data for comparison
#' combined_glm <- USbind_out(train_out_glm_ref, train_out_glm_new)
#' raw_compare <- combined_glm$comparison_df
#' n_vars_diff <- combined_glm$n_vars_diff  # Shows difference in number of parameters
#'
#'#' # Calculate rLR coefficients
#' results_rLR <- UScalc_raw(raw_compare, y_coef = "rLR", n_vars_diff)
#'
#' USplot(raw_data = raw_data, y_coef = "rLR", n_vars_diff = n_vars_diff)
#'
#' #' # Example 2b: Using raw data
#' train_out_glm_ref <- USprep_mdl(model_glm_ref, dataset = NULL, testing = FALSE)
#' train_out_glm_new <- USprep_mdl(model_glm_new, dataset = NULL, testing = FALSE)
#' combined_glm <- USbind_out(train_out_glm_ref, train_out_glm_new)
#' USplot(raw_data=combined_glm$comparison_df, y_coef = "rLR", n_vars_diff = combined_glm$n_vars_diff)
#'
#' # Example 3: Directly from models and data
#' # Ex 1: Evaluate on training data
#' USplot(ref_formula = disease ~ age + sex + bp + chol,
#'        new_formula = disease ~ age + sex + bp + chol + cp,
#'        ref_model_type = "glm",
#'        new_model_type = "glm",
#'        train_data = train_data,
#'        testing = FALSE,
#'        y_coef = "rLR")
#'
#' # Ex 2: Evaluate on test data
#' USplot(ref_formula = disease ~ age + sex + bp + chol,
#'        new_formula = disease ~ age + sex + bp + chol + cp,
#'        ref_model_type = "glm",
#'        new_model_type = "glm",
#'        train_data = train_data,
#'        test_data = test_data,
#'        testing = TRUE,
#'        y_coef = "rLR")
#'
#' # Ex 3: Evaluate on training data (net coefficients and statistical significance testing)
#' USplot(ref_formula = disease ~ age + sex + bp + chol,
#'        new_formula = disease ~ age + sex + bp + chol + cp,
#'        ref_model_type = "glm",
#'        new_model_type = "glm",
#'        train_data = train_data,
#'        testing = FALSE,
#'        y_coef = "rLR",
#'        net = TRUE,
#'        crit=2)
#'
#' # Ex 3: With randomForest and calibration on test data
#' USplot(ref_formula = disease ~ age + sex + bp + chol,
#'        new_formula = disease ~ age + sex + bp + chol + cp,
#'        ref_model_type = "randomForest",
#'        new_model_type = "randomForest",
#'        train_data = train_data,
#'        test_data = test_data,
#'        testing = TRUE,
#'        y_coef = "rLR",
#'        calibrate = TRUE)
#'
#' # Ex 4: Compare GLM with randomForest
#' USplot(ref_formula = disease ~ age + sex,
#'        new_formula = disease ~ age + sex,
#'        ref_model_type = "glm",
#'        new_model_type = "randomForest",
#'        train_data = train_data,
#'        y_coef = "rLR")
#'
#' # Ex 5: Compare randomForest with calibrated randomForest on test data
#' USplot(ref_formula = disease ~ age + sex,
#'        new_formula = disease ~ age + sex,
#'        ref_model_type = "randomForest",
#'        new_model_type = "randomForest",
#'        train_data = train_data,
#'        test_data = test_data,
#'        testing = TRUE,
#'        ref_calibrate = FALSE,
#'        new_calibrate = TRUE,
#'        y_coef = "rLR")
#'
#'
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_size_continuous
#' @importFrom ggplot2 scale_x_discrete scale_color_manual scale_fill_manual
#' @importFrom ggplot2 theme element_blank element_text ggtitle coord_cartesian
#' @importFrom ggplot2 guides guide_legend scale_y_continuous geom_segment
#' @importFrom ggplot2 geom_label
#' @importFrom stats glm binomial predict
#' @importFrom randomForest randomForest
USplot <- function(plot_data = NULL, y_coef, raw_data = NULL,
                   ref_formula = NULL, new_formula = NULL,
                   ref_model_type = "glm", new_model_type = "glm",
                   train_data = NULL, test_data = NULL, testing = FALSE,
                   ref_calibrate = TRUE, new_calibrate = TRUE,
                   calibration_method = "isotonic",
                   n_vars_diff = NULL, circle_sizes = TRUE, y_lim = NULL,
                   net = FALSE, crit = 0) {

  # Validate input parameters and prepare plot_data if not provided directly
  if (is.null(plot_data)) {
    if (!is.null(ref_formula) && !is.null(new_formula) && !is.null(train_data)) {
      # Case 1: Model specifications provided - build models and calculate plot_data

      # Validate model types
      if (!ref_model_type %in% c("glm", "randomForest")) {
        stop("ref_model_type must be either 'glm' or 'randomForest'")
      }
      if (!new_model_type %in% c("glm", "randomForest")) {
        stop("new_model_type must be either 'glm' or 'randomForest'")
      }

      # Determine evaluation dataset
      eval_data <- if (testing) {
        if (is.null(test_data)) {
          warning("No test_data provided, using train_data for evaluation")
          train_data
        } else {
          test_data
        }
      } else {
        train_data
      }

      # Build reference model on training data
      if (ref_model_type == "glm") {
        ref_model <- glm(ref_formula, data = train_data, family = binomial())
      } else {
        if (!requireNamespace("randomForest", quietly = TRUE)) {
          stop("randomForest package is required for randomForest models")
        }
        ref_model <- randomForest::randomForest(ref_formula, data = train_data)
      }

      # Build new model on training data
      if (new_model_type == "glm") {
        new_model <- glm(new_formula, data = train_data, family = binomial())
      } else {
        new_model <- randomForest::randomForest(new_formula, data = train_data)
      }

      # Prepare model outputs with appropriate calibration settings
      ref_out <- USprep_mdl(ref_model,
                            dataset = if (testing) eval_data else NULL,
                            testing = testing,
                            calibrate = if (ref_model_type == "randomForest") ref_calibrate else FALSE,
                            calibration_method = calibration_method)

      new_out <- USprep_mdl(new_model,
                            dataset = if (testing) eval_data else NULL,
                            testing = testing,
                            calibrate = if (new_model_type == "randomForest") new_calibrate else FALSE,
                            calibration_method = calibration_method)

      # Combine outputs
      combined <- USbind_out(ref_out, new_out)

      # Calculate plot data
      plot_data <- UScalc_raw(combined$comparison_df, y_coef, combined$n_vars_diff)

    } else if (!is.null(raw_data)) {
      # Case 2: Raw data provided - calculate plot_data using UScalc_raw
      if (is.null(n_vars_diff)) {
        stop("When providing raw_data, n_vars_diff must be specified")
      }
      plot_data <- UScalc_raw(raw_data, y_coef, n_vars_diff)
    } else {
      stop("Either plot_data, or raw_data + n_vars_diff, or model specifications must be provided")
    }
  }

  # Extract data from plot_data
  data.Y <- plot_data$data.Y
  point_sizes <- plot_data$point_sizes
  p_value <- plot_data$p_Y_overall
  netto_Y_nonev <- plot_data$netto_Y_nonev
  netto_Y_event <- plot_data$netto_Y_event
  Y_nonev_be <- plot_data$Y_nonev_be
  Y_nonev_wo <- plot_data$Y_nonev_wo
  Y_event_wo <- plot_data$Y_event_wo
  Y_event_be <- plot_data$Y_event_be
  nonev_solid <- plot_data$nonev_solid
  event_solid <- plot_data$event_solid

  # Define plot aesthetics
  subclass_order <- c('nonev_be', 'nonev_wo', 'event_wo', 'event_be')
  usmile_colors <- c('nonev_be' = '#0F3C78',
                     'nonev_wo' = '#0F3C78',
                     'event_wo' = '#D51424',
                     'event_be' = '#D51424')
  usmile_fills  <- c('nonev_be' = '#0F3C78',
                     'nonev_wo' = '#BED2FA',
                     'event_wo' = '#FBCDB9',
                     'event_be' = '#D51424')
  usmile_labels <- c('non-events with better prediction',
                     'non-events with worse prediction',
                     'events with worse prediction',
                     'events with better prediction')
  fixed_sizes <- c(0, 0.25, 0.5, 0.75, 1)

  # Create base plot
  single_usmile <- ggplot2::ggplot(data.Y, ggplot2::aes(x = subclass, y = value, group = coefficient))

  # Add connecting lines based on criteria
  if (crit == 2 && y_coef == "rLR") {
    # For rLR with crit=2, use conditional line types
    nonev_data <- subset(data.Y, segment == 'nonev')
    single_usmile <- single_usmile +
      ggplot2::geom_line(
        data = nonev_data,
        linetype = ifelse(nonev_solid, "solid", "dotted"),
        linewidth = 0.5,
        show.legend = FALSE
      )

    event_data <- subset(data.Y, segment == 'event')
    single_usmile <- single_usmile +
      ggplot2::geom_line(
        data = event_data,
        linetype = ifelse(event_solid, "solid", "dotted"),
        linewidth = 0.5,
        show.legend = FALSE
      )

    wo_be_data <- subset(data.Y, wo_be == 1)
    single_usmile <- single_usmile +
      ggplot2::geom_line(
        data = wo_be_data,
        linetype = "solid",
        color = "gray",
        linewidth = 0.5,
        show.legend = FALSE
      )
  } else {
    # Default solid gray connecting lines
    single_usmile <- single_usmile +
      ggplot2::geom_line(
        data = data.Y,
        linetype = "solid",
        color = "gray",
        linewidth = 0.5,
        show.legend = FALSE
      )
  }

  # Add points with size scaling
  single_usmile <- single_usmile +
    ggplot2::geom_point(
      ggplot2::aes(color = subclass, fill = subclass, size = point_sizes),
      shape = 21,
      stroke = 1,
      show.legend = TRUE
    ) +
    ggplot2::scale_size_continuous(
      breaks = if (circle_sizes) fixed_sizes else NULL,
      range = if (circle_sizes) c(0.1, 10) else c(1, 1),
      labels = if (circle_sizes) fixed_sizes else NULL,
      limits = if (circle_sizes) c(min(fixed_sizes), max(fixed_sizes)) else NULL,
      guide = if (circle_sizes) ggplot2::guide_legend(direction = "horizontal") else "none"
    ) +
    ggplot2::scale_x_discrete(
      limits = subclass_order,
      labels = usmile_labels
    ) +
    ggplot2::scale_color_manual(
      values = usmile_colors,
      breaks = subclass_order,
      labels = usmile_labels
    ) +
    ggplot2::scale_fill_manual(
      values = usmile_fills,
      breaks = subclass_order,
      labels = usmile_labels
    ) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::ggtitle(paste(y_coef, sprintf(" (p = %.4f)", p_value))) +
    ggplot2::coord_cartesian(clip = "off")

  # Add point size legend if needed
  if (circle_sizes) {
    single_usmile <- single_usmile +
      ggplot2::guides(
        size = ggplot2::guide_legend(
          title = "Point size",
          override.aes = list(shape = 21, color = "black", fill = "gray")
        )
      )
  }

  # Set Y-axis range
  if (!is.null(y_lim)) {
    y_range <- y_lim
  } else if (y_coef == "rLR") {
    y_range <- c(0, 1)
  } else {
    y_min <- min(Y_nonev_be, Y_nonev_wo, Y_event_wo, Y_event_be)
    y_max <- max(Y_nonev_be, Y_nonev_wo, Y_event_wo, Y_event_be)
    y_range <- c(y_min, y_max)
  }

  # Add margin to y-range
  y_margin <- (y_range[2] - y_range[1]) * 0.02
  y_range[2] <- y_range[2] + y_margin

  # Format y-axis (percentage for rLR)
  if (y_coef == "rLR") {
    single_usmile <- single_usmile +
      ggplot2::scale_y_continuous(
        limits = y_range,
        labels = function(x) paste0(x * 100, "%")
      )
  } else {
    single_usmile <- single_usmile +
      ggplot2::scale_y_continuous(limits = y_range)
  }

  # Add net coefficients if requested
  if (net) {
    y_offset <- (y_range[2] - y_range[1]) * 0.05

    single_usmile <- single_usmile +
      # Net coefficient for non-events
      ggplot2::geom_segment(
        ggplot2::aes(
          x = 0.2, xend = 0.2,
          y = ifelse(netto_Y_nonev < 0, Y_nonev_wo, Y_nonev_be),
          yend = ifelse(netto_Y_nonev < 0, Y_nonev_be, Y_nonev_wo)
        ),
        color = ifelse(netto_Y_nonev < 0, '#BED2FA', '#0F3C78'),
        linewidth = 2
      ) +
      ggplot2::geom_label(
        ggplot2::aes(
          x = 0.5,
          y = max(Y_nonev_be, Y_nonev_wo) + y_offset,
          label = ifelse(y_coef == "rLR",
                         sprintf("%.1f%%", netto_Y_nonev * 100),
                         sprintf("%.6f", netto_Y_nonev))
        ),
        color = '#0F3C78',
        label.size = 0,
        alpha = 0,
        size = 4
      ) +
      # Net coefficient for events
      ggplot2::geom_segment(
        ggplot2::aes(
          x = 4.8, xend = 4.8,
          y = ifelse(netto_Y_event < 0, Y_event_wo, Y_event_be),
          yend = ifelse(netto_Y_event < 0, Y_event_be, Y_event_wo)
        ),
        color = ifelse(netto_Y_event < 0, '#FBCDB9', '#D51424'),
        linewidth = 2
      ) +
      ggplot2::geom_label(
        ggplot2::aes(
          x = 4.5,
          y = max(Y_event_be, Y_event_wo) + y_offset,
          label = ifelse(y_coef == "rLR",
                         sprintf("%.1f%%", netto_Y_event * 100),
                         sprintf("%.6f", netto_Y_event))
        ),
        color = '#D51424',
        label.size = 0,
        alpha = 0,
        size = 4
      )
  }

  return(single_usmile)
}
