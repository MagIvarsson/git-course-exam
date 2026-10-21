# Load packages needed to run Lasso
library("glmnet")
library("dplyr")

# Load the cleaned data
df_barnulf_mhp_factor <- readRDS("data/data_with_mhp_factor.rds")

# Set seed
set.seed(123456)

# Define groups and their corresponding variable names
impairment_groups <- c(
  "td",
  "asthma_allergy",
  "adhd_asd",
  "dyslexia_dyscalculia",
  "other_broader",
  "sensory"
)

# List to store LASSO results for all groups
lasso_results <- list()

# Define a sequence of penalty strengths to evaluate
lambda_seq <- 10^seq(5, -5, length = 500)

# Repeat the LASSO analysis for each impairment group
for (group in impairment_groups) {

  # Select participants according to the current group's inclusion criterion
  # Remove ID, year, and impairment variables before constructing the predictor matrix
  # Select participants without the relevant impairment
if (group == "td") {
  df_lasso <- df_barnulf_mhp_factor %>%
    filter(child_impairment_any == 0)
} else {
  df_lasso <- df_barnulf_mhp_factor %>%
    filter(.data[[paste0("child_impairment_", group)]] == 0)
}

# Exclude ID, year, and impairment variables from the predictors
df_lasso <- df_lasso %>%
  dplyr::select(
    -c(ID, year, "...1"),
    -starts_with("child_impairment")
  )

  # Construct predictor matrix (X) and outcome vector (y)
  x_mental_health_problems <- model.matrix(
    mental_health_problems ~ .,
    df_lasso
  )

  y_mental_health_problems <- df_lasso %>%
    dplyr::select(mental_health_problems) %>%
    unlist()

  # Fit LASSO regression across the specified lambda sequence
  lasso_mental_health_problems_model <- glmnet::glmnet(
    x_mental_health_problems,
    y_mental_health_problems,
    alpha = 1,
    lambda = lambda_seq,
    standardize = TRUE
  )

  # Use cross-validation to select the penalty strength
  lasso_mental_health_problems_cv_output <- glmnet::cv.glmnet(
    x_mental_health_problems,
    y_mental_health_problems,
    alpha = 1,
    standardize = TRUE
  )

  # Select lambda.1se to favour a simpler model
  lasso_mental_health_problems_1se_lambda <-
    lasso_mental_health_problems_cv_output$lambda.1se

  # Generate predictions at the selected lambda
  lasso_mental_health_problems_predicted_y <- predict(
    lasso_mental_health_problems_model,
    s = lasso_mental_health_problems_1se_lambda,
    newx = x_mental_health_problems
  )

  # Calculate mean squared error (MSE)
  lasso_mental_health_problems_MSE <- mean(
    (lasso_mental_health_problems_predicted_y -
       y_mental_health_problems)^2
  )

  # Extract regression coefficients at the selected lambda
  lasso_mental_health_problems_predicted_coefficients <- predict(
    lasso_mental_health_problems_model,
    type = "coefficients",
    s = lasso_mental_health_problems_1se_lambda,
    newx = x_mental_health_problems
  )

  # Calculate squared correlation between observed and predicted values
  lasso_mental_health_problems_r2 <-
    cor(y_mental_health_problems,
        lasso_mental_health_problems_predicted_y)^2

  # Store results for the current group
  lasso_results[[group]] <- list(
    model = lasso_mental_health_problems_model,
    cv_output = lasso_mental_health_problems_cv_output,
    lambda_1se = lasso_mental_health_problems_1se_lambda,
    MSE = lasso_mental_health_problems_MSE,
    r2 = lasso_mental_health_problems_r2,
    predicted_y = lasso_mental_health_problems_predicted_y,
    coefficients = lasso_mental_health_problems_predicted_coefficients
  )
  
  # Save results using object names specific to the impairment group
  assign(paste0("df_lasso_", group), df_lasso)

  assign(
    paste0("x_mental_health_problems_", group),
    x_mental_health_problems
  )

  assign(
    paste0("y_mental_health_problems_", group),
    y_mental_health_problems
  )

  assign(
    paste0("lasso_mental_health_problems_", group, "_model"),
    lasso_mental_health_problems_model
  )

  assign(
    paste0("lasso_mental_health_problems_", group, "_cv_output"),
    lasso_mental_health_problems_cv_output
  )

  assign(
    paste0("lasso_mental_health_problems_", group, "_1se_lambda"),
    lasso_mental_health_problems_1se_lambda
  )

  assign(
    paste0("lasso_mental_health_problems_", group, "_predicted_y"),
    lasso_mental_health_problems_predicted_y
  )

  assign(
    paste0("lasso_mental_health_problems_", group, "_MSE"),
    lasso_mental_health_problems_MSE
  )

  assign(
    paste0("lasso_mental_health_problems_", group, "_predicted_coefficients"),
    lasso_mental_health_problems_predicted_coefficients
  )

  assign(
    paste0("lasso_mental_health_problems_", group, "_r2"),
    lasso_mental_health_problems_r2
  )
}

# Save the results of the LASSO
saveRDS(lasso_results, "data/lasso_results.rds")