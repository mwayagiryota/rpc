library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Feature preferences by age group
age_preferences <- final_survey %>%
  group_by(age_group) %>%
  summarise(
    across(starts_with("rank_viewing"):rank_comment_developments, 
           ~mean(., na.rm = TRUE))
  ) %>%
  gather(feature, avg_rank, -age_group) %>%
  mutate(
    feature = case_when(
      feature == "rank_viewing_projects" ~ "Viewing projects",
      feature == "rank_search" ~ "Search functionality",
      feature == "rank_events_calendar" ~ "Events calendar",
      feature == "rank_filter_areas" ~ "Filter by areas",
      feature == "rank_feedback_account" ~ "Feedback account",
      feature == "rank_email_updates" ~ "Email updates",
      feature == "rank_comment_developments" ~ "Comment on developments",
      TRUE ~ feature
    )
  )

# 2. Content preferences by residence duration
residence_preferences <- final_survey %>%
  group_by(residence_duration) %>%
  summarise(
    across(starts_with("rank_project"):rank_council_meetings, 
           ~mean(., na.rm = TRUE))
  ) %>%
  gather(feature, avg_rank, -residence_duration)

# 3. Accessibility needs by age group
accessibility_by_age <- final_survey %>%
  group_by(age_group) %>%
  summarise(
    across(starts_with("accessibility_") & !ends_with("text"), 
           ~sum(., na.rm = TRUE))
  )

# 4. Feature preferences by participation frequency
participation_preferences <- final_survey %>%
  group_by(participation_frequency) %>%
  summarise(
    across(starts_with("rank_viewing"):rank_comment_developments, 
           ~mean(., na.rm = TRUE))
  ) %>%
  gather(feature, avg_rank, -participation_frequency)

# Print key findings
print("Top Feature for Each Age Group:")
age_preferences %>%
  group_by(age_group) %>%
  slice_min(avg_rank, n = 1)

print("Top Feature for Each Residence Duration:")
residence_preferences %>%
  group_by(residence_duration) %>%
  slice_min(avg_rank, n = 1)

print("Accessibility Needs by Age Group:")
print(accessibility_by_age)

print("Top Feature by Participation Frequency:")
participation_preferences %>%
  group_by(participation_frequency) %>%
  slice_min(avg_rank, n = 1)