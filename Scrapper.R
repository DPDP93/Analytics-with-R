library(stringr)
library(rvest)
library(dplyr)
library(tibble)

# Scrap Page
# Input: URL
# Output: data.frame
parseDoc = function(url) {
    page = read_html(url)
    numberOfJobs = page %>% 
        html_nodes(".k-c-headline-2xl") %>%
        html_text(TRUE) %>%
        str_extract_all("[0-9]") %>%
        unlist() %>%
        paste(collapse = "") %>%
        as.integer()
    
    dataContainer = page %>% 
        html_nodes(".m-jobsListItem__dataContainer")
    
    jobs = data.frame(
        title = rep(NA, length(dataContainer)),
        company = rep(NA, length(dataContainer))
    )
    
    jobs$title = dataContainer %>% 
        html_nodes(".m-jobsListItem__title") %>%
        html_text(TRUE)
    
    jobs$company = dataContainer %>% 
        html_nodes(".m-jobsListItem__company") %>%
        html_text(TRUE)
    
    return(jobs)
}


# Main
words = c("Risk", "Controlling", "Data-Science", "DWH")
jobs = data.frame(
    title = c(""),
    company = c("")
)

for (i in 1:length(words)) {
    url = str_c("https://www.karriere.at/jobs/", words[i], "/wien/?page=1")
    pg = read_html(url)
    
    pageNumber = pg %>% html_nodes(".m-pagination__meta") %>% html_text() %>% str_trim()
    pageNumber = as.integer((str_sub(pageNumber, 7)))
    
    for (j in 1:pageNumber) {
        url =  str_c("https://www.karriere.at/jobs/", words[i], "/wien/?page=", j)
        jobs = bind_rows(jobs, parseDoc(url))
    }
}

jobs = jobs[-1,]
jobs = jobs %>% distinct()
jobs = jobs %>% mutate(date = as.character(lubridate::today()))
jobs = as_tibble(jobs)
print(jobs)

rm(i, j, pageNumber, url, parseDoc, pg)
