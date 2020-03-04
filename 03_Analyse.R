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
    keywords = list()
    keywords$controlling = c("controlling", "controller", "financial analyst", "cost account", "finance")
    keywords$risk = c("risk", "risiko")
    keywords$treasury = c("treasury", "treasurer", "cash")
    keywords$data = c("data", "analytics", "DWH", "BI", "Business Intelligence", "Business Analyst", 
                      "Datenbank", "SQL", "digital", "information", "IT ", "Entwickler", "Developer")
    keywords$intern = c("intern", "internship", "praktikant", "praktikum")
    
    
    # Find Category
    category = vector(mode="character")
    for (i in 1:length(keywords)) {
        if(detect(x, keywords[[i]])) { category = keywords[[i]][1] }
    }
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
df %>%   
    group_by(category) %>% 
    summarise(count = n(), age = round(mean(days), 2))  

df %>% 
    group_by(company) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    top_n(10)

df %>% 
    select(company, category) %>%
    mutate(
        IT = if_else(category == "data", 1, 0),
        FI = if_else(category %in% c("controlling", "risk"), 1, 0)
        ) %>%
    group_by(company) %>%
    summarise(
        IT = sum(IT),
        FI = sum(FI),
        Total = IT + FI
        ) %>%
    arrange(desc(Total)) %>% 
    top_n(10)
    
