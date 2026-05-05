# This script downloads view and download data from Zenodo OER

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

# Download one resource or module from Zenodo ----

# Record ID
record_id <- 18661315

# API URL
url <- paste0("https://zenodo.org/api/records/", record_id)

# Request data
res <- GET(url)
data <- fromJSON(content(res, "text", encoding = "UTF-8"))

# Extract stats
views <- data$stats$unique_views
downloads <- data$stats$unique_downloads

# Print
cat("Views:", views, "\n")
cat("Downloads:", downloads, "\n")

# Save to CSV
df <- data.frame(
  record_id = record_id,
  views = views,
  downloads = downloads
)

write.csv(df, "zenodo_stats.csv", row.names = FALSE)

# get names of records given id

record_id <- 18661315

url <- paste0("https://zenodo.org/api/records/", record_id)

res <- GET(url)
data <- fromJSON(content(res, "text", encoding = "UTF-8"))

title <- data$metadata$title

cat("Title:", title, "\n")


# Download multiple resources from Zenodo ----

zenodo_oer <- read.csv("data/zenodo/zenodo_oer.csv")

records_id <- zenodo_oer$resource_id

get_stats <- function(id) {
  url <- paste0("https://zenodo.org/api/records/", id)
  res <- GET(url)
  data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  
  data.frame(
    record_id = id,
    title = data$metadata$title,
    views = data$stats$unique_views,
    downloads = data$stats$unique_downloads
  )
}

df <- do.call(rbind, lapply(records_id, get_stats))

write.csv(df, "data/zenodo/zenodo_records.csv", row.names = FALSE)
