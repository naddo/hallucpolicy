library(tidyverse)
# ... other libraries ...

# Load raw data
temp <- readRDS("Temp020525.RDA")

# Data cleaning and variable creation
# ... your data wrangling code here ...

# Save cleaned data for use in Rmds
saveRDS(temp, "cleaned_data.rds") 