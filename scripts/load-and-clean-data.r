# Libraries needed for script
library("dplyr")
library("tidyverse")
library("psych")

# Load data
df_barnulf_synth <- readr::read_csv("data/barnulf_synthetic.csv")

# Quick overview of data
skimr::skim(df_barnulf_synth)

# Visualize with scatter plots
# Define function that creates 12 bar plots at a time
overview_barplot_barnulf <- function(df, vars = names(df), start = 1, n = 12) {

  vars <- intersect(vars, names(df))
  vars <- vars[start:min(start + n - 1, length(vars))]

  df %>%
    dplyr::select(all_of(vars)) %>%
    pivot_longer(
      everything(),
      names_to = "variable",
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    ggplot(aes(x = value)) +
    geom_bar() +
    facet_wrap(~variable, ncol = 3, scales = "free_x") +
    labs(x = "Response", y = "Frequency") +
    theme_minimal()
}

# Plot variables 12 at a time (except ID)
overview_barplot_barnulf(df_barnulf_synth, start = 2)
overview_barplot_barnulf(df_barnulf_synth, start = 14)
overview_barplot_barnulf(df_barnulf_synth, start = 26)
overview_barplot_barnulf(df_barnulf_synth, start = 38)
overview_barplot_barnulf(df_barnulf_synth, start = 50)
overview_barplot_barnulf(df_barnulf_synth, start = 62)
overview_barplot_barnulf(df_barnulf_synth, start = 74)
overview_barplot_barnulf(df_barnulf_synth, start = 86)

# Setting up indices of variables according to preregistration
df_barnulf_synth_indices_1 <- df_barnulf_synth %>%
  rowwise() %>%
  mutate(
    
# Participation (attendance) in cultural activities
# Preregistration: "If answer is yes it is coded as 1, no as 0. Items will be summed and the total sum used"
    participation_culture = sum(across(starts_with("attend"))),

# Participation (attendance) in sport and similar activities
# Preregistration: "Items are answered as yes=1 or no=0. Items will be summed and the total sum used."
    participation_sport = sum(across(c(sport, train, other_activity))), 
    
# Participation (attendance) in household activities 
# Preregistration: "If answer is yes=1, if no=0. Items will be summed and the total sum used."
    participation_household = sum(across(c(clean_room, cook, wash, outdoor_work))),
    
# Participation (attendance) in social activities
# Preregistration: "All items listed under social participation will be summed, and the total sum used."
    participation_friends = sum(across(starts_with("friends_"))), 
    
# Person to speak with 
# Preregistration: "Each possible person to speak to is answered as yes/no. [...] Items will be summed, and the total sum used."
    family_speak = sum(across(c(family_speak_mother, family_speak_father, family_speak_sibling))),
    .keep = "unused")

# Family relationships 
# Preregistration: "As these items are not expected to load equally, they will not be summed but instead an exploratory factor analysis using a one factor solution will be performed."
# Run EFA
fit_family <- df_barnulf_synth_indices_1 %>%
  dplyr::select(starts_with("family_")) %>% 
  psych::fa(., 1)

# Extract factor scores
family_fs_obj <- psych::factor.scores(df_barnulf_synth_indices_1 %>% dplyr::select(starts_with("family_")), fit_family)

# Add factor score to data frame
df_barnulf_synth_indices_1$family_relationships <- family_fs_obj$scores

# Remove the variables that are not needed anymore
df_barnulf_synth_indices_2 <- df_barnulf_synth_indices_1 %>%
  dplyr::select(
    -c(
        family_speak, 
        family_mother_get_on, 
        family_father_get_on, 
        family_mother_time, 
        family_father_time, 
        family_decide
    )
)

# Create factor scores for school social relationships 
# Preregistration: "As these items are not expected to load equally, they will not be summed but instead an exploratory factor analysis using a one factor solution will be performed."
df_barnulf_synth_indices_3 <- df_barnulf_synth_indices_2 %>%
  rowwise() %>%
  mutate(school_speak_to = sum(across(starts_with("school_speak_"))),
    .keep = "unused")

# Run EFA
fit_school <- df_barnulf_synth_indices_3 %>%
  dplyr::select(starts_with("school")) %>%
  psych::fa(., 1)

# Extract factor scores
school_fs_obj <- psych::factor.scores(df_barnulf_synth_indices_3 %>% dplyr::select(c(school_speak_to, school_social_treat_bad_teacher, school_social_close_friend)), fit_school)

# Add new factor score to dataframe
df_barnulf_synth_indices_3$school_social_relationships <- school_fs_obj$scores

# Remove the varibles that were used to build factor score
df_barnulf_synth_indices_4 <- df_barnulf_synth_indices_3 %>%
  dplyr::select(
    -c(
        school_speak_to, 
        school_social_treat_bad_teacher, 
        school_social_close_friend
    )
)

# Create variable with sum score of items relating to sense of safety 
df_barnulf_synth_indices_5 <- df_barnulf_synth_indices_4 %>%
  rowwise() %>%
  mutate(
    sense_safety = sum(across(starts_with("safe_"))),
    .keep="unused"
    ) %>%
   
# Create variable with sum score of items relating to parental physcial health
# Preregistration: "Each possible type of pain to is answered as yes/no. Items are answered as yes=1 or no=0. Items will be summed, and the total sum used."
  mutate(parent_pain = sum(across(c(parent_pain_arms_legs, parent_pain_back, parent_pain_shoulders_neck))),
         .keep = "unused"
  ) %>%
   
# Create sum score of SES related variables
# Preregistration: "An index will be created for SES with a sum score where a higher score indicates a lower SES status."
  dplyr::mutate(ses = sum(across(c(ses_cash_reserve,ses_financial_problems,ses_further_education_parent,ses_overcrowding))),
         .keep = "unused"
  ) %>%
  
# Remove SES household group from analysis at this stage (difficult to interpret)
dplyr::select(-ses_household_group)

# Save cleaned up data frame to file
saveRDS(df_barnulf_synth_indices_5, "mydata.rds")