# Load packages
library("tidyr") # Data wrangling
library("glmnet") # LASSO
library("ggrepel") # Prevent labels from overlapping

plot_lasso_paths <- function(group, lasso_results) {

  # Extract model and selected lambda
  lasso_model <- lasso_results[[group]]$model
  lambda_1se  <- lasso_results[[group]]$lambda_1se

  # Convert coefficient paths to a data frame
  coef_matrix <- as.matrix(lasso_model$beta)

  coef_df <- data.frame(
    variable = rep(rownames(coef_matrix), times = ncol(coef_matrix)),
    lambda = rep(lasso_model$lambda, each = nrow(coef_matrix)),
    coefficient = as.vector(coef_matrix)
  ) |>
    mutate(log_lambda = log(lambda))

  # Identify the leftmost endpoint of each coefficient path
  left_labels <- coef_df |>
    group_by(variable) |>
    slice_min(log_lambda, n = 1, with_ties = FALSE) |>
    ungroup()

  # Determine spacing for labels in the original x-axis units
  x_range <- range(coef_df$log_lambda, na.rm = TRUE)
  x_gap <- diff(x_range) * 0.12

  # Create the plot
  p <- ggplot(
    coef_df,
    aes(
      x = log_lambda,
      y = coefficient,
      colour = variable,
      group = variable
    )
  ) +
    geom_line(linewidth = 0.8) +

    # Selected lambda
    geom_vline(
      xintercept = log(lambda_1se),
      linetype = "dashed",
      colour = "grey40",
      linewidth = 0.6
    ) +

    # Labels at the left endpoints, with white backgrounds
    geom_label_repel(
      data = left_labels,
      aes(
        x = log_lambda,
        y = coefficient,
        label = variable
      ),
      direction = "y",
      nudge_x = -x_gap,
      hjust = 1,
      force = 2,
      force_pull = 0.2,
      box.padding = 0.45,
      point.padding = 0.25,
      label.padding = unit(0.12, "lines"),
      label.size = 0,
      fill = "white",
      min.segment.length = 0,
      segment.colour = "grey60",
      segment.size = 0.3,
      max.overlaps = Inf,
      seed = 123,
      show.legend = FALSE
    ) +

    # Preserve the original, normal x-axis direction
    scale_x_continuous(
      expand = expansion(mult = c(0.25, 0.05))
    ) +

    # Distinguish coefficient paths by variable
    scale_colour_discrete() +

    coord_cartesian(clip = "off") +

    labs(
      title = paste("LASSO coefficient paths:", group),
      x = expression(log(lambda)),
      y = "Coefficient",
      colour = "Variable"
    ) +

    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.margin = margin(
        t = 10, r = 20, b = 10, l = 40
      ),
      legend.position = "none"
    )

  return(p)
}


# Save all groups as PNG files
for (group in impairment_groups) {

  p <- plot_lasso_paths(group, lasso_results)

  ggsave(
    filename = file.path(
      "output",
      paste0("lasso_paths_", group, ".png")
    ),
    plot = p,
    width = 12,
    height = 7,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}