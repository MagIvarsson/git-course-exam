# Libraries needed for script
library("tidyverse")
library("psych")

# Load data
df_barnulf_synth <- readr::read_csv("data/barnulf_synthetic.csv")

# Remove variables not needed for the current project
df_barnulf_synth_1 <- df_barnulf_synth %>%
  dplyr::select(
    -c(
        starts_with("wellbeing"),
        ses_household_group,
        mental_health_problems_old
    )
  )

# Setting up indices of variables according to preregistration
df_barnulf_synth_indices_1 <- df_barnulf_synth_1 %>%
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
         .keep = "unused")

# Transform disability-variable in line with what is stipulated in the preregistration
# Preregistration: "If one or more groups do not contain enough participants (n<100), will be merged with category 7 (other disability) (except for: if both hearing and visual impairment groups are low in numbers they will be merged to a perceptual disability group)."

# Create empty broader impairment category from the original "other" impairment variable. This ensures the variable exists even if none of the subsequent conditional recoding steps are triggered.
df_barnulf_synth_indices_5 <- df_barnulf_synth_indices_5 %>%
  dplyr::mutate(
    child_impairment_other_broader = child_impairment_other
  )

# Create a combined sensory impairment indicator if fewer than 100 participants report vision impairment OR fewer than 100 report having a hearing impairment
if(
  sum(df_barnulf_synth_indices_5$child_impairment_vision == 1)<100 | sum(df_barnulf_synth_indices_5$child_impairment_hearing == 1)<100) {
  df_barnulf_synth_indices_5 <- df_barnulf_synth_indices_5 %>% 
    dplyr::mutate(
      child_impairment_sensory = if_else(
        child_impairment_vision == 1,
        1, 
        if_else(
          child_impairment_hearing == 1,
          1,
          0)
        ),
        .keep = "unused"
      )
  }

# If fewer than 100 participants reported any sensory impairment, combine it with other impairments in the broader "other impairment" category
if(
  "child_impairment_sensory" %in% colnames(df_barnulf_synth_indices_5) & sum(df_barnulf_synth_indices_5$child_impairment_sensory == 1)<100) {
  df_barnulf_synth_indices_5 <- df_barnulf_synth_indices_5 %>%
    dplyr::mutate(child_impairment_other_broader = if_else(child_impairment_other == 1 | child_impairment_sensory == 1, 1, child_impairment_other),
           .keep = "unused")
}

# If fewer than 100 participants reported asthma or allergy, combine this category with the broader "other impairment" category.
if(sum(df_barnulf_synth_indices_5$child_impairment_asthma_allergy == 1)<100) {
  df_barnulf_synth_indices_5 <- df_barnulf_synth_indices_5 %>%
    mutate(child_impairment_other_broader = if_else(child_impairment_other_broader == 1 | child_impairment_asthma_allergy == 1, 1, child_impairment_other_broader),
           .keep = "unused")
}

# If fewer than 100 participants reported ADHD or autism, combine this category with the broader "other impairment" category.
if(sum(df_barnulf_synth_indices_5$child_impairment_adhd_asd == 1)<100) {
  df_barnulf_synth_indices_5 <- df_barnulf_synth_indices_5 %>%
    mutate(child_impairment_other_broader = if_else(child_impairment_other_broader == 1 | child_impairment_adhd_asd == 1, 1, child_impairment_other_broader),
           .keep = "unused")
}

# If fewer than 100 participants report having dyslexia or dyscalculia, combine this category with the broader "other impairment" category.
if(sum(df_barnulf_synth_indices_5$child_impairment_dyslexia_dyscalculia == 1)<100) {
  df_barnulf_synth_indices_5 <- df_barnulf_synth_indices_5 %>%
    mutate(child_impairment_other_broader = if_else(child_impairment_other_broader == 1 | child_impairment_dyslexia_dyscalculia == 1, 1, child_impairment_other_broader),
           .keep = "unused")
}

# If fewer than 100 participants report having a mobility impairment, combine this category with the broader "other impairment" category.
if(sum(df_barnulf_synth_indices_5$child_impairment_mobility == 1)<100) {
  df_barnulf_synth_indices_5 <- df_barnulf_synth_indices_5 %>%
    mutate(child_impairment_other_broader = if_else(child_impairment_other_broader == 1 | child_impairment_mobility == 1, 1, child_impairment_other_broader),
           .keep = "unused")
}

# Create variable indicating that the participant has at least 1 impairment
df_barnulf_synth_indices_5 <- df_barnulf_synth_indices_5 %>%
  dplyr::mutate(
    child_impairment_any = if_else(
      if_any(starts_with("child_impairment"), ~ .x == 1),
      1L,
      0L
    ),
    .keep = "all"
  ) %>%
  ungroup()

# Save cleaned up data frame to file
saveRDS(df_barnulf_synth_indices_5, "data/barnulf_synthetic_clean.rds")
