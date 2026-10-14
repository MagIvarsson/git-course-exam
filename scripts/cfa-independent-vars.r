# Libraries
library("lavaan")

# Loading the cleaned up data
df_barnulf_synth_indices_5 <- readRDS("data/barnulf_synthetic_clean.rds")

# Define the mental health problems model
mental_health_problems_model <- 'mental_health_problems =~ mental_health_problem_sad + mental_health_problem_concentrate + mental_health_problem_nervous + mental_health_problem_stress + mental_health_problem_bad_sleep + mental_health_problem_fall_asleep + mental_health_problem_tired + mental_health_problem_headache + mental_health_problem_stomache'

# Running the CFA model with the barnULF data
mhp_lav_output <- cfa(data = df_barnulf_synth_indices_5, model = mental_health_problems_model, ordered = TRUE, estimator = "WLSMV")

# Display results
summary(mhp_lav_output, fit.measures = TRUE)