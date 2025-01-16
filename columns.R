# Rename the question columns
actualsurvey <- actualsurvey %>%
  rename(
    "Question 2" = "QID4",
    "Question 3" = "QID6",
    "Question 4" = "QID7",
    "Question 5" = "QID8",
    "Question 6" = "QID5", 
    "Question 7" = "Q1",
    "Question 8" = "Q2",
    "Question 9" = "Q13",
    "Question 13" = "Q17",
    "Question 14" = "Q18",
    "Question 15" = "Q20"
  )

# Define the features for Question 10 (previously Q14)
features <- c(
  "viewing development projects", 
  "search", 
  "event calendar", 
  "filter projects by area", 
  "create an account", 
  "registering", 
  "commenting"
)

# Define the content for Question 11 (previously Q15)
content_q15 <- c(
  "Project development descriptions", 
  "Key dates", 
  "Location details", 
  "Project timelines", 
  "FAQs", 
  "Project location maps (static images)", 
  "Being able to comment",
  "Council meeting development talks"
)

# Transform both ranking questions with proper question numbers
actualsurvey <- actualsurvey %>%
  # First, reshape Question 10 (previously Q14)
  pivot_longer(
    cols = starts_with("Q14_"),
    names_to = "Question_10_Rank",
    values_to = "Feature_Value"
  ) %>%
  mutate(
    Feature_Number = as.numeric(gsub("Q14_", "", Question_10_Rank)),
    Feature = features[Feature_Number]
  ) %>%
  # Then, reshape Question 11 (previously Q15)
  pivot_longer(
    cols = starts_with("Q15_"),
    names_to = "Question_11_Rank",
    values_to = "Content_Value"
  ) %>%
  mutate(
    Content_Number = as.numeric(gsub("Q15_", "", Question_11_Rank)),
    Content = content_q15[Content_Number]
  )

# Check column names
colnames(actualsurvey)

# One-step transformation for Question 16
actualsurvey <- actualsurvey %>%
  mutate(
    Q16_Alt_Text = ifelse(Q16_1 == "1", 1, 0),
    Q16_Contrast = ifelse(Q16_2 == "1", 1, 0),
    Q16_Text_Size = ifelse(Q16_3 == "1", 1, 0),
    Q16_None = ifelse(Q16_4 == "1", 1, 0),
    Q16_Other = ifelse(Q16_5 == "1", 1, 0),
    Q16_Other_Text = ifelse(Q16_5 == "1", Q16_5_TEXT, NA)
  ) %>%
  select(-c(Q16_1, Q16_2, Q16_3, Q16_4, Q16_5, Q16_5_TEXT))

# Check the results
head(actualsurvey[, c("Q16_Alt_Text", "Q16_Contrast", "Q16_Text_Size", "Q16_None", "Q16_Other", "Q16_Other_Text")])
