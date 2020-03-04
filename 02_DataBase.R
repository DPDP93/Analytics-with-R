library(DBI)
library(RSQLite)
library(dplyr)
library(tibble)

# Create connection, read table
con = dbConnect(RSQLite::SQLite(), "data.sqlite")
dbJobs = as_tibble(dbReadTable(con, "jobs"))
dbListObjects(con)

# Differentiate Dataset in old, existing, new jobs
oldJobs = anti_join(dbJobs, jobs, by = c("title", "company"))
existingJobs = inner_join(dbJobs, jobs, by = c("title", "company")) %>%
    mutate(date = date.x) %>% 
    select(-date.y, -date.x)
newJobs = anti_join(jobs, existingJobs, by = c("title", "company"))


# Write to database
jobs = bind_rows(oldJobs, existingJobs, newJobs)
dbWriteTable(con, "jobs", jobs, overwrite = TRUE)
dbWriteTable(con, "_jobs", dbJobs, overwrite = TRUE)


# Log, cleanup
tibble(
    tables = c("old", "existing", "new"),
    rows = c(nrow(oldJobs), nrow(existingJobs), nrow(newJobs))
)
rm(oldJobs, existingJobs, newJobs, dbJobs, jobs)
