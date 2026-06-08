# This script downloads view and download data from Zenodo OER

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

# Download multiple resources from Zenodo ----
zenodo_oer <- read.csv("data/zenodo/zenodo_oer.csv")

zenodo_oer <- zenodo_oer |>
  filter(status == "accepted") |>
  mutate(resource_id = str_extract(doi, "\\d+$"),
         resource_id = as.integer(resource_id),
         submission_date = as_date(submission_date, origin = "1899-12-30"))

resource_id <- zenodo_oer$resource_id

get_zenodo_metrics <- function(id) {
  url <- paste0("https://zenodo.org/api/records/", id)
  res <- GET(url)
  data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  
  data.frame(
    resource_id = id,
    title = data$metadata$title,
    views = data$stats$unique_views,
    downloads = data$stats$unique_downloads
  )
}

df <- do.call(rbind, lapply(resource_id, get_zenodo_metrics))

df_zenodo <- df |>
  left_join(zenodo_oer) |>
  select(-c(resource_title, doi, submission_date, status))

write.csv(
  df_zenodo,
  paste0("data/zenodo/zenodo_records_", format(Sys.Date(), "%d-%m-%Y"), ".csv"),
  row.names = FALSE
)
