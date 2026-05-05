# This script downloads enrollment data from OpenLearnity

library(httr)
library(readr)
library(dplyr)


## Authentication ----
# This script requires authentication to access the data portal.
# A valid COOKIE and CSRF_TOKEN must be provided.
# These credentials should be set as environment variables in `.Renviron`:
# The script reads them using `Sys.getenv()`.

cookie <- Sys.getenv("COOKIE") # COOKIE (session cookie from a valid login)
csrf_token <- Sys.getenv("CSRF_TOKEN") # CSRF_TOKEN (security token linked to the session)

# Define modules ----
courses <- c(
  "course-v1:data+management+plan",
  "course-v1:ETH+sensitive+data",
  "course-v1:open+fair+data",
  "course-v1:open+formats+code",
  "course-v1:storage+backup+versioning",
  "course-v1:ETH+RDM-01+2024",
  "course-v1:ETH+data+licences",
  "course-v1:ETH+code+reproducibility",
  "course-v1:ETH+documentation+metadata",
  "course-v1:open+data+organisation"
)

# Map course IDs → filenames
course_names <- c(
  "course-v1:data+management+plan" = "dmp",
  "course-v1:ETH+sensitive+data" = "sensitive_data",
  "course-v1:open+fair+data" = "fair_data",
  "course-v1:open+formats+code" = "openformats_code",
  "course-v1:storage+backup+versioning" = "storage_backup_versioning",
  "course-v1:ETH+RDM-01+2024" = "publishing",
  "course-v1:ETH+data+licences" = "licences",
  "course-v1:ETH+code+reproducibility" = "reproducibility",
  "course-v1:ETH+documentation+metadata" = "documentation_metadata",
  "course-v1:open+data+organisation" = "organisation"
)

# Read function  ----
get_data <- function(course_id, cookie, csrf_token, filename_custom = NULL) {
  
  base_url <- paste0(
    "https://courses.openlearnity.org/courses/",
    course_id,
    "/instructor/api/"
  )
  
  headers <- add_headers(
    Cookie = cookie,
    `x-csrftoken` = csrf_token,
    Referer = paste0(
      "https://courses.openlearnity.org/courses/",
      course_id,
      "/instructor/dashboard"
    )
  )
  
  # Ensure directory exists
  dir.create("data/openlearnity", recursive = TRUE, showWarnings = FALSE)
  
  # 1 Trigger new report
  POST(paste0(base_url, "get_students_features/csv"), headers)
  
  Sys.sleep(10)  # wait for report generation
  
  # 2 Get downloads list
  res <- POST(
    paste0(base_url, "list_report_downloads"),
    headers,
    encode = "json"
  )
  
  data <- content(res, "parsed")
  
  # Safety check
  if (is.null(data$downloads) || length(data$downloads) == 0) {
    stop("❌ No downloads found or authentication failed")
  }
  
  # 3 Get latest CSV URL
  csv_url <- data$downloads[[1]]$url
  df <- read_csv(csv_url)
  
  # 4 Determine filename (always in data/openlearnity)
  if (is.null(filename_custom)) {
    clean_name <- course_names[course_id]
    filename <- file.path(
      "data/openlearnity",
      paste0(
        clean_name,
        "_",
        format(Sys.time(), "%Y-%m-%d-%H%M"),
        ".csv"
      )
    )
  } else {
    filename <- file.path("data/openlearnity", filename_custom)
  }
  
  # 5 Save CSV
  readr::write_csv(df, filename)
  cat("✅ Saved:", filename, "\n")
  
  return(df)
}

# Run function  ----
results <- lapply(courses, function(course) {
  get_data(course, cookie, csrf_token)
})

# Name list elements for clarity
names(results) <- course_names
