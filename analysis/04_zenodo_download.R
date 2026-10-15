# This script downloads view and download data from Zenodo OER

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

# Download multiple resources from Zenodo ----
# ATTENTION: The file zenodo_oer needs to be updated each time to reflect the list of resources recorded here: 
# https://ethz.sharepoint.com/:x:/r/sites/PRO-INFO-00002177/_layouts/15/Doc2.aspx?action=edit&sourcedoc=%7B76640ce7-164f-4a6e-be58-b379f7cdfd62%7D&wdOrigin=TEAMS-MAGLEV.teamsSdk_ns.rwc&wdExp=TEAMS-TREATMENT&wdhostclicktime=1772714038905&web=1

zenodo_oer <- read.csv("data/zenodo/zenodo_oer_15-10-2026.csv")

zenodo_oer <- zenodo_oer |>
  filter(status == "accepted") |>
  mutate(resource_id = str_extract(doi, "\\d+$"),
         resource_id = as.integer(resource_id))

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
  select(-c(resource_title, doi, authors, status))

write.csv(
  df_zenodo,
  paste0("data/zenodo/zenodo_downloads_", format(Sys.Date(), "%d-%m-%Y"), ".csv"),
  row.names = FALSE
)
