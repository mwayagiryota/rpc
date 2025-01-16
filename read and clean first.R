library(readr)
library(dplyr)
library(tidyr)
library(janitor)

# Read the data first
actualsurvey <- read_csv("/Users/macuser/Documents/Dissertation/Survey Data/surveyv.csv", 
                         show_col_types = FALSE)

# A further look at the data
print("Initial data dimensions:")
dim(actualsurvey)

# Unnessecary metadata rows and clean names
clean_survey <- actualsurvey %>%
  slice(3:n()) %>%
  clean_names()

# Results
print("Data after removing metadata:")
dim(clean_survey)
print("Column names:")
names(clean_survey)

# Responses that were finished should be kept only
complete_survey <- clean_survey %>%
  filter(progress == "100")

print("Number of complete responses:")
nrow(complete_survey)

# Renaming important columns
focused_survey <- complete_survey %>%
  select(
    ip_address,
    q1, q2, q13,  # Basic demographics
    starts_with("q14"),  # Engagement features
    starts_with("q15"),  # Website content
    starts_with("q16"),  # Accessibility
    q17, q18, q19, q20  # Design and additional info
  ) %>%
  rename(
    age_group = q1,
    residence_duration = q2,
    participation_frequency = q13
  )

# See the sructure
print("\nStructure of focused data:")
str(focused_survey)

#summary of responses for key questions
print("\nAge group distribution:")
table(focused_survey$age_group, useNA = "ifany")

print("\nResidence duration distribution:")
table(focused_survey$residence_duration, useNA = "ifany")

print("\nParticipation frequency distribution:")
table(focused_survey$participation_frequency, useNA = "ifany")

# Save and continue it later
write_csv(focused_survey, "focused_survey.csv")

