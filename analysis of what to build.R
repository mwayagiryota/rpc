#Analysis of final cleaned one
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Demographics Analysis
demographics <- final_survey %>%
  summarise(
    # Age distribution
    age_dist = list(table(age_group)),
    # Residence patterns
    residence_dist = list(table(residence_duration)),
    # Participation frequency
    participation_dist = list(table(participation_frequency))
  )

# 2. Ranking Analysis for Engagement Features (q14)
engagement_features <- final_survey %>%
  summarise(
    across(starts_with("rank_viewing"):rank_comment_developments, 
           ~mean(., na.rm = TRUE))
  ) %>%
  gather(feature, avg_rank) %>%
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
  ) %>%
  arrange(avg_rank)

# 3. Website Content Rankings (q15)
content_features <- final_survey %>%
  summarise(
    across(starts_with("rank_project"):rank_council_meetings, 
           ~mean(., na.rm = TRUE))
  ) %>%
  gather(feature, avg_rank) %>%
  mutate(
    feature = case_when(
      feature == "rank_project_descriptions" ~ "Project descriptions",
      feature == "rank_key_dates" ~ "Key dates",
      feature == "rank_location_details" ~ "Location details",
      feature == "rank_project_timelines" ~ "Project timelines",
      feature == "rank_faqs" ~ "FAQs",
      feature == "rank_location_maps" ~ "Location maps",
      feature == "rank_commenting" ~ "Commenting feature",
      feature == "rank_council_meetings" ~ "Council meeting dates",
      TRUE ~ feature
    )
  ) %>%
  arrange(avg_rank)

# 4. Accessibility Analysis
accessibility_summary <- final_survey %>%
  summarise(
    across(starts_with("accessibility_") & !ends_with("text"), 
           ~sum(., na.rm = TRUE))
  ) %>%
  gather(feature, count) %>%
  mutate(
    feature = case_when(
      feature == "accessibility_alt_text" ~ "Alt text for images",
      feature == "accessibility_contrast" ~ "High contrast mode",
      feature == "accessibility_text_size" ~ "Text size adjustment",
      feature == "accessibility_none" ~ "No accessibility needs",
      feature == "accessibility_other" ~ "Other features",
      TRUE ~ feature
    )
  ) %>%
  arrange(desc(count))

# Print results
print("Top 3 Most Important Engagement Features:")
head(engagement_features, 3)

print("Top 3 Most Valued Website Content Types:")
head(content_features, 3)

print("Most Requested Accessibility Features:")
print(accessibility_summary)

# 5. Design Preferences Analysis
design_pref_summary <- final_survey %>%
  group_by(design_preference) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

print("Design Preferences Distribution:")
print(design_pref_summary)