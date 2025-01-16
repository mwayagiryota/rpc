library(readr)
library(dplyr)
library(tidyr)

# Read the survey data
actualsurvey <- read_csv("/Users/macuser/Documents/Dissertation/Survey Data/surveyv.csv")







#Question 12 is on accesibility multiple choice and has been put into 2 columns - 
#^Q16 and Q16_5_TEXT

# Combine Q16 (multiple choice options) and Q16_5_TEXT (open-ended "other" text) into a new column 'Question 12'
actualsurvey <- actualsurvey %>%
  mutate(`Question 12` = case_when(
    # If "Other" is selected and there's text in Q16_5_TEXT, combine both
    grepl("Other", Q16, ignore.case = TRUE) & !is.na(Q16_5_TEXT) ~ paste(Q16, ": ", Q16_5_TEXT),
    # If "Other" is selected but no additional text, just return Q16
    grepl("Other", Q16, ignore.case = TRUE) & is.na(Q16_5_TEXT) ~ Q16,
    # Otherwise, just keep the selected options in Q16
    TRUE ~ Q16
  ))

# Check the result
head(actualsurvey)
#Check column Q12 only
head(actualsurvey$`Question 12`)
# View the data in Q16 (accessibility features)
head(actualsurvey$Q16)

# View the 'Other' responses in Q16_5_TEXT
head(actualsurvey$Q16_5_TEXT)
head(actualsurvey$Q16, 20)
colnames(actualsurvey)
actualsurvey$Q16_split <- strsplit(as.character(actualsurvey$Q16), ", ")
head(actualsurvey$Q16_split)






















# Reshape Q15_1 to Q15_7 into one column called "Rank", keeping other columns intact
actualsurvey <- actualsurvey %>%
  pivot_longer(cols = Q15_1:Q15_7, 
               names_to = "Question", 
               values_to = "Content Rank") %>%
  mutate(Question = "Question 11") %>%  # Question column to "Question 11"
  select(IPAddress, ResponseId, Question, Rank, everything())  # Keeping all other columns intact








#Question 13 is one choice for choosing what element of design is important for building a site - Q17
#Question 14 is open text on a website homepage what info do you like to see - Q18
#Question 17 anything to let me know - Q20