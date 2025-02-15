# Load required libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(shiny)
library(bslib)
library(markdown)
library(shinythemes)
library(shinyalert)

# Define variables for data pull
api_url <- "https://catsched.davidson.edu/course"
bearer_token <- "5ed40de719a4635dd26f9b8c_gybr7ez~iac8t.d7iwa8ea.65grj55o2b~z5a58jobzlcv76"

# Get course data from API
get_course_data <- function() {
  # Make the GET request
  response <- GET(
    url = api_url,
    add_headers(Authorization = paste("Bearer", bearer_token))
  )
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON content
    response_content <- content(response, as = "text", encoding = "UTF-8")
    response_json <- fromJSON(response_content, simplifyVector  = TRUE, flatten = FALSE)#, flatten = TRUE)
    # response_flat <- flatten(response_content)
    
  } else {
    # Print the error message
    message("Failed to retrieve data. Status code: ", status_code(response))
    print(content(response, as = "text", encoding = "UTF-8"))
  }
}

activeTerm <- "202501"

df <- get_course_data()

dummy_times <- as.list(
  tribble(
    ~days,   ~startTime, ~endTime,
    "M,W,F", "9:30am",   "10:20am"
  )
)

df_dummy <- tribble(
  ~ courseNumber,
  ~ sectionNumber,
  ~ sectionTitle,
  ~ semester,
  ~ department,
  ~ firstAssignedRoom,
  ~ firstMeetingTime,
  ~ secondAssignedRoom,
  ~ secondMeetingTime,
  ~ crossPosts,
  ~ firstNonStandardMeetingTime,
  ~ secondNonStandardMeetingTime,
  "000",
  "0",
  "Dummy",
  activeTerm,
  "ZZZ",
  "",
  "",
  "",
  "",
  "",
  dummy_times,
  dummy_times
) %>%
  unnest_wider(col = firstNonStandardMeetingTime, names_sep = "$")
  

df_clean <- df %>%
  # select only the columns we need
  select(
    courseNumber,
    sectionNumber,
    sectionTitle,
    semester,
    department,
    firstAssignedRoom,
    firstMeetingTime,
    secondAssignedRoom,
    secondMeetingTime,
    crossPosts,
    firstNonStandardMeetingTime,
    secondNonStandardMeetingTime
  ) %>%
  unnest_wider(col = firstNonStandardMeetingTime, names_sep = "$") %>% 
  union(df_dummy)

df_depts <- get_course_data() %>%
  distinct(department) %>%
  filter(department != "UNS") %>%
  mutate(
    deptGroup = case_when(
      department %in% c("MAT", "CSC", "DAT") ~ "Mathematics + Computer Science + Data Science",
      department %in% c("PHY", "PBH") ~ "Public Health + Physics",
      department %in% c("HUM", "ANT") ~ "Humanities + Anthropology",
      department %in% c("DAN", "PHI") ~ "Dance + Philosophy",
      department %in% c("CHI", "CIS") ~ "Chinese Studies + Center for Interdisciplinary Studies",
      department %in% c("CLA", "AFR", "GRE", "LAT") ~ "Classics + Africana Studies",
      department %in% c("COM", "GSS") ~ "Communication Studies + Gender and Sexuality Studies",
      department %in% c("EDU", "RUS") ~ "Educational Studies + Russian Studies",
      department %in% c("FMS", "DIG", "ARB") ~ "Film, Media, and Digital Studies + Arab Studies",
      department %in% c("FRE", "LAS", "ARB") ~ "French and Francophone Studies + Latin American Studies",
      department %in% c("GER", "THE") ~ "German Studies + Theatre",
      department %in% c("MUS", "LIT") ~ "Music + Global Literary Theory",
      .default = department
    )
  ) %>%
  arrange(deptGroup)
