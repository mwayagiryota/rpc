library(readr)
library(dplyr)
library(tidyr)

actualsurvey <- read_csv("/Users/macuser/Documents/Dissertation/Survey Data/surveyv.csv")

# Clean column names by removing JSON-like metadata
actualsurvey <- actualsurvey[3:nrow(actualsurvey), ]

# Delete columns that are unneccesary
actualsurvey <- subset(actualsurvey, select = -c(
  Status, StartDate, EndDate, Progress, Q10, Q11, Q19_1_TEXT, `Duration (in seconds)`, Finished, RecordedDate, 
  RecipientLastName, RecipientEmail, ExternalReference, LocationLatitude, LocationLongitude, 
  DistributionChannel, UserLanguage, Q_RecaptchaScore, RecipientFirstName, `_Id`, `_Name`, `_Size`, `_Type` 
))
# See if the columns have been deleted 
head(actualsurvey)

# Delete question columns that are unnecessary - name ,, signature and testing the prototype
actualsurvey <- subset(actualsurvey, select = -c(
  Q10,Q11,Q19,Q19_1_TEXT
))

# See if the columns have been deleted 
colnames(actualsurvey)
library(readr)
library(dplyr)
library(tidyr)

# Read the data
actualsurvey <- read_csv("/Users/macuser/Documents/Dissertation/Survey Data/surveyv.csv", 
                         show_col_types = FALSE)

# Remove the first two rows (metadata) and clean up columns
actualsurvey <- actualsurvey %>%
  slice(3:n()) %>%
  # Remove unnecessary columns
  select(-c(Status, StartDate, EndDate, Progress, Q10, Q11, Q19_1_TEXT, 
            `Duration (in seconds)`, Finished, RecordedDate, 
            RecipientLastName, RecipientEmail, ExternalReference, 
            LocationLatitude, LocationLongitude, DistributionChannel, 
            UserLanguage, Q_RecaptchaScore, RecipientFirstName,
            `_Id`, `_Name`, `_Size`, `_Type`))

# Convert all columns to appropriate types
actualsurvey <- actualsurvey %>%
  mutate(across(everything(), ~type.convert(.x, as.is = TRUE)))

# Display summary information
glimpse(actualsurvey)

# Check for missing values
colSums(is.na(actualsurvey)) %>% 
  sort(decreasing = TRUE)

# Display first few rows with better formatting
print(actualsurvey, n = 6, width = Inf)
