# Load packages
library("lavaan")

# Set seed
set.seed(666)

# Loading the cleaned up data
df_barnulf_synth_indices_5 <- readRDS("data/barnulf_synthetic_clean.rds")

# Define the mental health problems model
mental_health_problems_model <- 'mental_health_problems =~ mental_health_problem_sad + mental_health_problem_concentrate + mental_health_problem_nervous + mental_health_problem_stress + mental_health_problem_bad_sleep + mental_health_problem_fall_asleep + mental_health_problem_tired + mental_health_problem_headache + mental_health_problem_stomache'

# Running the CFA model with the barnULF data
mhp_lav_output <- cfa(data = df_barnulf_synth_indices_5, model = mental_health_problems_model, ordered = TRUE, estimator = "WLSMV")

# Display results
summary(mhp_lav_output, fit.measures = TRUE)

# Checking possible modifications
mhp_mi_1 <- modindices(mhp_lav_output)
print(mhp_mi_1)

# Defining the 2nd CFA model (adding the residual correlation with most effect importance to fit added)
mental_health_problems_model_2 <- '
#latent variable and indicators
mental_health_problems =~ mental_health_problem_sad + mental_health_problem_concentrate + mental_health_problem_nervous + mental_health_problem_stress + mental_health_problem_bad_sleep + mental_health_problem_fall_asleep + mental_health_problem_tired +mental_health_problem_headache + mental_health_problem_stomache

#residual correlations
mental_health_problem_bad_sleep ~~ mental_health_problem_fall_asleep
'
# Checking possible modifications
mhp_mi_2 <- modindices(mhp_lav_output)
print(mhp_mi_2)

# Defining the 3rd CFA model (adding the residual correlation with most effect importance to fit added)
mental_health_problems_model_3 <- '
#latent variable and indicators
mental_health_problems =~ mental_health_problem_sad + mental_health_problem_concentrate + mental_health_problem_nervous + mental_health_problem_stress + mental_health_problem_bad_sleep + mental_health_problem_fall_asleep + mental_health_problem_tired +mental_health_problem_headache + mental_health_problem_stomache

#residual correlations
mental_health_problem_bad_sleep ~~ mental_health_problem_fall_asleep
mental_health_problem_sad ~~ mental_health_problem_nervous
'

# Running model 3
mhp_lav_output_3 <- cfa(data = df_barnulf_synth_indices_5, model = mental_health_problems_model_3, ordered = TRUE, estimator = "WLSMV")

# Display results
summary(mhp_lav_output_3, fit.measures = TRUE)

# Checking possible modifications
mhp_mi_3 <- modindices(mhp_lav_output_3)
print(mhp_mi_3)

# Defining the 4th CFA model (adding the residual correlation with most effect importance to fit added)
mental_health_problems_model_4 <- '
#latent variable and indicators
mental_health_problems =~ mental_health_problem_sad + mental_health_problem_concentrate + mental_health_problem_nervous + mental_health_problem_stress + mental_health_problem_bad_sleep + mental_health_problem_fall_asleep + mental_health_problem_tired +mental_health_problem_headache + mental_health_problem_stomache

#residual correlations
mental_health_problem_bad_sleep ~~ mental_health_problem_fall_asleep
mental_health_problem_sad ~~ mental_health_problem_nervous
mental_health_problem_nervous ~~ mental_health_problem_stress
'

# Running model 4
mhp_lav_output_4 <- cfa(data = df_barnulf_synth_indices_5, model = mental_health_problems_model_4, ordered = TRUE, estimator = "WLSMV")

# Display results
summary(mhp_lav_output_4, fit.measures = TRUE)

# Checking possible modifications
mhp_mi_4 <- modindices(mhp_lav_output_4)
print(mhp_mi_4)

# Defining the 5th CFA model (adding the residual correlation with most effect importance to fit added)
mental_health_problems_model_5 <- '
#latent variable and indicators
mental_health_problems =~ mental_health_problem_sad + mental_health_problem_concentrate + mental_health_problem_nervous + mental_health_problem_stress + mental_health_problem_bad_sleep + mental_health_problem_fall_asleep + mental_health_problem_tired +mental_health_problem_headache + mental_health_problem_stomache

#residual correlations
mental_health_problem_bad_sleep ~~ mental_health_problem_fall_asleep
mental_health_problem_sad ~~ mental_health_problem_nervous
mental_health_problem_nervous ~~ mental_health_problem_stress
mental_health_problem_headache ~~ mental_health_problem_stomache
'

# Running model 45
mhp_lav_output_5 <- cfa(data = df_barnulf_synth_indices_5, model = mental_health_problems_model_5, ordered = TRUE, estimator = "WLSMV")

# Display results
summary(mhp_lav_output_5, fit.measures = TRUE)

# Checking possible modifications
mhp_mi_5 <- modindices(mhp_lav_output_5)
print(mhp_mi_5)

# Models can be further optimized (and can be according to preregistration) but I am stopping here for the purpose of this mini-assignment

# Create factor scores based on final model
df_mental_health_problem_factor <- lavPredict(mhp_lav_output_5, type = "lv")

# Combine with the rest of data
df_barnulf_synth_indices_6 <- df_barnulf_synth_indices_5 %>%
  dplyr::bind_cols(., df_mental_health_problem_factor) %>%

# Remove the indicators df
  dplyr::select(-starts_with("mental_health_problem_"))

# Save as new data file
saveRDS(df_barnulf_synth_indices_6, "data/data_with_mhp_factor.rds")
