library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(stringr)
library(tibble)

# Create connection, read table
con = dbConnect(RSQLite::SQLite(), "data.sqlite")
df = as_tibble(dbReadTable(con, "jobs"))


# Data Manipulation
df = df %>% 
    mutate(days = today() - as_date(df$date)) %>%
    mutate(category = NA)


## -----------------------------
## Helpers
# Detect function
# @param {string} x, 
# @param {array<str>} keywords
# @return {bool} found
detect = function(x, keywords) {
    found = FALSE
    for (i in 1:length(keywords)) {
        if (str_detect(x, regex(keywords[i], ignore_case = TRUE))) {
            found = TRUE
        }
    }
    return(found)
}


# Category Function
# @param {string} x 
# @return {string} - category
categorize = function(x) {
    # Keywords
    controllingKeywords = c("controlling", "controller")
    riskKeywords = c("risk", "risiko")

    # Find Category
    category = vector(mode="character")
    if(detect(x, controllingKeywords)) { category = "controlling" }
    if(detect(x, riskKeywords)) { category = "risk" }
    
    return(paste(category, collapse = ", "))
}

## -----------------------------


# Find category
for (i in 1:nrow(df)) {
    df$category[i] = categorize(df$title[i])
}


# Save back to DB
dbWriteTable(con, "JobAnalytics", df, overwrite = TRUE)


# Analytics 
df %>% group_by(category) %>% summarise(count = n())        # by Category
