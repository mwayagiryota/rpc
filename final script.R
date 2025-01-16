library(readr)
library(dplyr)
library(tidyr)
library(janitor)

# Read & initial clean
actualsurvey <- read_csv("/Users/macuser/Documents/Dissertation/Survey Data/surveyv.csv", 
                         show_col_types = FALSE)

# Clean the data
clean_survey <- actualsurvey %>%
  # Remove the first two metadata rows
  slice(3:n()) %>%
  
  # Clean column names
  clean_names() %>%
  
  # Filter out incomplete responses first
  filter(progress == "100") %>%
  
  # Remove unnecessary columns
  select(-c(
    status, start_date, end_date, progress, duration_in_seconds,
    finished, recorded_date, recipient_last_name, recipient_first_name,
    recipient_email, external_reference, location_latitude, location_longitude,
    distribution_channel, user_language, q_recaptcha_score,
    id, name, size, type
  )) %>%
  
  # Convert numeric columns
  mutate(across(starts_with(c("q14", "q15")), as.numeric)) %>%
  
  # Rename important columns for clarity
  rename(
    age_group = q1,
    residence_duration = q2,
    participation_frequency = q13
  )

# From previous day saved - Further transformations
clean_survey <- clean_survey %>%
  # Remove rows where all key demographics are NA
  filter(!is.na(age_group) & !is.na(residence_duration) & !is.na(participation_frequency)) %>%
  
  # Convert ranking questions to numeric
  mutate(across(starts_with(c("q14", "q15")), as.numeric)) %>%
  
  # Add labels for categorical variables
  mutate(
    age_group = case_when(
      age_group == "1" ~ "18-24",
      age_group == "2" ~ "25-34",
      age_group == "3" ~ "35-44",
      age_group == "4" ~ "45-54",
      age_group == "5" ~ "55-64",
      age_group == "6" ~ "65+",
      TRUE ~ NA_character_
    ),
    
    residence_duration = case_when(
      residence_duration == "1" ~ "Less than 1 year",
      residence_duration == "2" ~ "1-5 years",
      residence_duration == "3" ~ "6-10 years",
      residence_duration == "4" ~ "More than 10 years",
      residence_duration == "8" ~ "I don't live in Chesterfield",
      TRUE ~ NA_character_
    ),
    
    participation_frequency = case_when(
      participation_frequency == "1" ~ "Very often",
      participation_frequency == "2" ~ "Often",
      participation_frequency == "3" ~ "Sometimes",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # Rename the ranking questions for clarity
  rename(
    rank_viewing_projects = q14_1,
    rank_search = q14_2,
    rank_events_calendar = q14_3,
    rank_filter_areas = q14_4,
    rank_feedback_account = q14_5,
    rank_email_updates = q14_6,
    rank_comment_developments = q14_7,
    
    rank_project_descriptions = q15_1,
    rank_key_dates = q15_2,
    rank_location_details = q15_3,
    rank_project_timelines = q15_4,
    rank_faqs = q15_5,
    rank_location_maps = q15_6,
    rank_commenting = q15_7,
    rank_council_meetings = q15_8
  ) %>%
  
  # Create accessibility features as binary columns
  mutate(
    accessibility_alt_text = ifelse(!is.na(q16_1), 1, 0),
    accessibility_contrast = ifelse(!is.na(q16_2), 1, 0),
    accessibility_text_size = ifelse(!is.na(q16_3), 1, 0),
    accessibility_none = ifelse(!is.na(q16_4), 1, 0),
    accessibility_other = ifelse(!is.na(q16_5), 1, 0),
    accessibility_other_text = q16_5_text
  ) %>%
  
  # Remove original accessibility columns
  select(-starts_with("q16")) %>%
  
  # Rename remaining columns
  rename(
    design_preference = q17,
    homepage_preference = q18,
    prototype_interest = q19,
    additional_comments = q20
  )

# View the cleaned data structure
print("Structure of cleaned data:")
str(clean_survey)

# Show summary statistics for ranking questions
print("Mean rankings for engagement features:")
clean_survey %>%
  select(starts_with("rank_viewing"):rank_comment_developments) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

print("Mean rankings for content types:")
clean_survey %>%
  select(rank_project_descriptions:rank_council_meetings) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# View column names and structure
colnames(clean_survey)

# Quick summary of the data
summary(clean_survey)

# Final version! Analyse it tomo for building
write_csv(clean_survey, "final_cleaned_survey.csv")
