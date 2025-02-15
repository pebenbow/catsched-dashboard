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

########################################################
# PRODUCT BACKLOG
# Cross-listed courses?
# Add multi-select?
# Add filter by building?
# Show visual in modal dialog?
########################################################

# Define variables for data pull
conn_args <- config::get("dataconnection")

api_url <- conn_args$url
bearer_token <- conn_args$token

#activeTerm <- "202402"

########################################################
# Helper functions
########################################################

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
    response_json <- fromJSON(response_content)
    
    # Convert JSON to a data frame
    df <- as.data.frame(response_json)
    
  } else {
    # Print the error message
    message("Failed to retrieve data. Status code: ", status_code(response))
    print(content(response, as = "text", encoding = "UTF-8"))
  }
}

# Function to calculate stack positions for a single day
calculate_stack_positions <- function(data) {
  n <- nrow(data)
  stack_positions <- integer(n)  # Initialize vector for stack positions
  active_end_times <- numeric()  # Tracks end times of active stacks
  
  for (i in seq_len(n)) {
    start_time <- data$startMinsSince8[i]
    end_time <- data$endMinsSince8[i]
    
    # Find the first available stack position
    available_stack <- which(!active_end_times | active_end_times <= start_time)
    if (length(available_stack) > 0) {
      stack_positions[i] <- available_stack[1]
      active_end_times[available_stack[1]] <- end_time
    } else {
      # Create a new stack position
      stack_positions[i] <- length(active_end_times) + 1
      active_end_times <- c(active_end_times, end_time)
    }
  }
  stack_positions
}

# Define maximum stack height and centering logic
stack_height <- 0.8  # Maximum height allocated per day (adjustable)
text_height_threshold <- 0.05  # Define a height threshold for text

# Centering function for stacks
center_stack_y <- function(day_numeric, stack_pos, total_stacks) {
  day_numeric + (stack_pos - (total_stacks + 1) / 2) * (stack_height / total_stacks)
}

# Tile height function
tile_height <- function(total_stacks) {
  stack_height / total_stacks
}

# Additional gray tile data
gray_tiles <- data.frame(
  dayOfWeek = c("Tue", "Thu"),
  startMinsSince8 = c(185, 185),  # 11:05 AM in minutes since 8:00 AM
  endMinsSince8 = c(245, 245),    # 12:05 PM in minutes since 8:00 AM
  fill = "Break"
)

# Convert gray_tiles Day_of_Week to a factor
gray_tiles$dayOfWeek <- factor(gray_tiles$dayOfWeek, levels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri")))

########################################################
# Shiny app: UI layout
########################################################

# define the UI layout
ui <- fluidPage(  
  #shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  theme = bs_theme(version = 5, bootswatch = "materia"),
  # Custom CSS for full-height layout
  tags$style(HTML("
    #schedViz_container {
      height: calc(80vh); /* Adjust based on header and navbar height */
    }
    #schedViz {
      width: 100%;
      height: 100%; /* Fill the container */
    }
  ")),
  # Filter controls at the top
  fluidRow(
    column(width = 3, titlePanel("CatSched Dashboard")),
    tags$script('
        $(document).on("click", "#refresh", function() {
          location.reload();
        })
    '),
    column(
      width = 3,
      selectInput(
        "term_filter",
        label = "Academic Term",
        choices = list(
          #`Fall 2024`   = "202401",
          #`Spring 2025` = "202402",
          `Fall 2025`   = "202501",
          `Spring 2026` = "202502"
        )
      )
    ),
    column(
      width = 4,
      selectizeInput(
        "department_filter", 
        label = "Department Grouping", 
        choices = NULL, #c("All", df_depts$deptGroup), 
        options = list(placeholder = 'Search/Select Department(s)'),
        width="500px"
      )
    ),
    column(width = 2,actionButton("refresh", "Refresh Departments"))
  ),
  
  navset_card_underline(
    id = "main_tabs",
    nav_panel(
      "Class Schedules Visualization",
      div(
        id = "schedViz_container",  # Outer div for height control
        plotOutput("schedViz")     # Plot output inside the container
      )
    ),
    nav_panel(
      "User Guide",
      includeMarkdown("dashboardUserGuide.md")
    )
  )
)

########################################################
# Shiny app: server functionality
########################################################

server <- function(input, output, session) {
  
  df_depts <- reactive({
    
    get_course_data() %>%
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
  })
  
  # Initialize a reactive value to trigger updates
  # data_update_trigger <- reactiveVal(FALSE)
  
  # Reactive expression to fetch and process data
  refreshed_data <- reactive({
    
    # Trigger reactive dependency
    # data_update_trigger()
    
    req(input$department_filter)  # Ensure a department is selected
    
    activeTerm <- input$term_filter

    df <- get_course_data()
    
    # For testing
    # activeTerm <- "202501"
    
    # Generate dummy class schedule to aid in processing non-standard times
    dummy_times <- as.list(
      tribble(
        ~days,   ~startTime, ~endTime,
        "M,T,W,R,F", "9:30am",   "10:20am"
      )
    )
    
    # Generate dummy data frame
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
    unnest_wider(col = firstNonStandardMeetingTime, names_sep = "_")
    
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
      # remove UNS "unspecified" department code and filter on specific semester
      filter(
        #department != "UNS",
        semester   == activeTerm
      ) %>%
      # split firstNonStandardMeetingTime into separate columns
      unnest_wider(col = firstNonStandardMeetingTime, names_sep = "_") %>% 
      # insert the dummy data frame
      union(df_dummy) %>%
      # generate new column to uniquely identify each section
      unite(
        keyTermDeptCrseSect,
        c("semester","department","courseNumber","sectionNumber"),
        sep    = "-",
        remove = FALSE
      )%>%
      # generate new column to uniquely identify each course
      unite(
        keyDeptCrse,
        c("department","courseNumber"),
        sep    = "-",
        remove = FALSE
      )
    
    # Process non-standard meeting times (group 1)
    df_nonstandard1 <- df_clean %>%
      select(
        keyDeptCrse,
        firstNonStandardMeetingTime_days,
        firstNonStandardMeetingTime_startTime,
        firstNonStandardMeetingTime_endTime
      ) %>%  
      filter(
        firstNonStandardMeetingTime_days != ""
      ) %>%
      # split days into separate columns
      mutate(meetDays = str_split(firstNonStandardMeetingTime_days, ",")) %>%
      unnest_wider(meetDays, names_sep = "")%>%
      select(
        keyDeptCrse,
        startTime = firstNonStandardMeetingTime_startTime,
        endTime = firstNonStandardMeetingTime_endTime,
        meetDays1:meetDays5,
      ) %>%
      pivot_longer(
        !keyDeptCrse:endTime,
        names_to       ="meetDayNum",
        values_to      = "dayOfWeek",
        values_drop_na = TRUE
      )
    
    
    # Process non-standard meeting times
    df_nonstandard2 <- df_clean %>%
      select(
        keyDeptCrse,
        secondNonStandardMeetingTime
      ) %>%  
      mutate(
        secondNonStandardMeetingTime = map(
          secondNonStandardMeetingTime,
          ~ ifelse(. == "", NA, .)
        )) %>%
      filter(
        !str_detect(secondNonStandardMeetingTime,"NA")
      ) %>%
      unnest_wider(secondNonStandardMeetingTime, names_sep = "_") %>%
      # split days into separate columns
      mutate(meetDays = str_split(secondNonStandardMeetingTime_days, ",")) %>%
      unnest_wider(meetDays, names_sep = "") %>%
      select(
        keyDeptCrse,
        startTime = secondNonStandardMeetingTime_startTime,
        endTime = secondNonStandardMeetingTime_endTime,
        meetDays1:meetDays5,
      ) %>%
      pivot_longer(
        !keyDeptCrse:endTime,
        names_to       ="meetDayNum",
        values_to      = "dayOfWeek",
        values_drop_na = TRUE
      )
    
    df_times <- df_clean %>%
      # get only the columns we need
      select(
        keyDeptCrse,
        firstMeetingTime,
        secondMeetingTime
      ) %>%
      # filter out TBD and blank meeting times
      filter(
        firstMeetingTime != "TBD",
        firstMeetingTime != ""
      ) %>%
      # unpivot values
      pivot_longer(
        !keyDeptCrse,
        names_to  =  "firstOrSecond",
        values_to = "meetingTime"
      ) %>%
      # remove blank meeting times
      filter(
        meetingTime != ""
      ) %>%
      # split meeting times into separate columns for days and times
      separate_wider_delim(
        col      = meetingTime,
        delim    = " ",
        names    = c("meetDays", "meetTimes"),
        too_many = "merge"
      ) %>%
      # split times into separate columns for start times and end times
      separate_wider_delim(
        col   = meetTimes,
        delim = " - ",
        names = c("startTime", "endTime")
      ) %>%
      # split days into separate columns
      mutate(meetDays = str_split(meetDays, "")) %>%
      unnest_wider(meetDays, names_sep = "") %>%
      # unpivot again based on days
      pivot_longer(
        meetDays1:meetDays3,
        names_to       =  "meetDayNum",
        values_to      = "dayOfWeek",
        values_drop_na = TRUE
      ) %>% 
      bind_rows(df_nonstandard1,df_nonstandard2) %>%
      mutate(
        dayOfWeek = case_when(
          dayOfWeek == "M" ~ "Mon",
          dayOfWeek == "T" ~ "Tue",
          dayOfWeek == "W" ~ "Wed",
          dayOfWeek == "R" ~ "Thu",
          dayOfWeek == "F" ~ "Fri"
        )
      ) %>%
      mutate(
        # Convert times to proper datetime objects (using parse_date_time with updated format)
        startTime_parsed = parse_date_time(startTime, "HMp"),
        endTime_parsed   = parse_date_time(endTime, "HMp"),
        # Calculate minutes since 8:00 AM
        startMinsSince8 = as.numeric(difftime(startTime_parsed, parse_date_time("8:00am", "HMp"), units = "mins")),
        endMinsSince8   = as.numeric(difftime(endTime_parsed, parse_date_time("8:00am", "HMp"), units = "mins")),
        duration        = endMinsSince8 - startMinsSince8
      ) %>%
      # Drop intermediate columns
      select(-startTime_parsed, -endTime_parsed) %>%
      # get distinct records
      distinct(.keep_all = TRUE) %>%
      arrange(keyDeptCrse) %>%
      mutate(
        department = str_sub(keyDeptCrse,0,3)
      ) %>%
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
      filter(
        department != "UNS",
        department != "ZZZ"
      ) %>%
      select(
        keyDeptCrse,
        deptGroup,
        department,
        dayOfWeek,
        startTime,
        endTime,
        duration,
        startMinsSince8,
        endMinsSince8
      )
    
    # Clean out intermediate data frames
    rm(df_nonstandard1)
    rm(df_nonstandard2)
    
    # If user chooses all departments, show all; otherwise, filter them
    if (input$department_filter != "All") {
      df_times <- df_times %>% filter(deptGroup == input$department_filter)
    }
    
    # Process dataset to include duration and calculate stack positions
    classes <- df_times %>%
      group_by(dayOfWeek) %>%
      arrange(startMinsSince8, endMinsSince8) %>%
      mutate(Stack_Pos = calculate_stack_positions(cur_data())) %>%  # Apply custom function
      ungroup() %>%
      group_by(dayOfWeek) %>%
      mutate(Total_Stacks = max(Stack_Pos)) %>%
      ungroup() %>%
      # Calculate whether text should be shown based on tile height
      mutate(
        Tile_Height = stack_height / Total_Stacks,  # Calculate tile height
        Show_Text = Tile_Height > text_height_threshold  # Determine if text fits
      )
    
    # Convert Day_of_Week to a factor to ensure correct order on the Y-axis
    classes$dayOfWeek <- factor(classes$dayOfWeek, levels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri")))
    classes
  })
  
  # Refresh button
  # observeEvent(input$refresh, {
  #   # Invalidate the reactive value to trigger an update
  #   data_update_trigger(!data_update_trigger())
  # })
  
  # Observe changes from other inputs or refresh button
  observeEvent(c(input$refresh), {
    # Update selectizeInput choices
    updateSelectizeInput(session, "department_filter",
                         choices = c("All", df_depts()$deptGroup))
  }, ignoreNULL = FALSE)
  
  # Define output
  output$schedViz <- renderPlot({
    # Create the ggplot with coord_flip for horizontal orientation
    ggplot(refreshed_data()) +
      # Use geom_segment to draw lines from Start_Time to End_Time
      geom_tile(
        aes(
          #y = as.numeric(Day_of_Week) + (Stack_Pos - (Total_Sections + 1) / 2) * (box_height / Total_Sections),
          # y = as.numeric(Day_of_Week) + (Stack_Pos - 1) * (max_stack_height / max(Stack_Pos)),  # Offset for overlaps
          y = center_stack_y(as.numeric(dayOfWeek), Stack_Pos, Total_Stacks),
          x = (startMinsSince8 + endMinsSince8) / 2,
          width = endMinsSince8 - startMinsSince8,  # Each tile spans the full class duration
          #height = 0.25,
          #height = box_height / max(Stack_Pos),
          height = tile_height(Total_Stacks)
        ),
        color = "black", fill = "#99e3dd",
        show.legend = FALSE
      ) +
      # Add gray tiles for break times
      geom_tile(
        data = gray_tiles,
        aes(
          x = (startMinsSince8 + endMinsSince8) / 2,
          y = as.numeric(dayOfWeek),
          width = endMinsSince8 - startMinsSince8,
          height = 0.8
        ),
        fill = "gray", alpha = 0.5  # Set gray color with transparency
      ) +
      # Conditionally add text to tiles where it fits
      geom_text(
        aes(
          x = (startMinsSince8 + endMinsSince8) / 2,
          y = center_stack_y(as.numeric(dayOfWeek), Stack_Pos, Total_Stacks),
          label = ifelse(Show_Text, keyDeptCrse, NA)  # Only show text if it fits
        ),
        hjust = 0.5,  # Center align text horizontally
        size = 5, 
        color = "black",
        na.rm = TRUE  # Skip missing labels
      ) +
      # Customize the X-axis to show times from 8 AM to 5 PM in ascending order
      scale_x_continuous(
        breaks = seq(0, 510, by = 60),  # 540 is the number of minutes from 8:00 AM to 5:00 PM
        labels = function(x) {
          hours <- (x + 480) %/% 60
          minutes <- (x + 480) %% 60
          sprintf("%02d:%02d", hours, minutes)
        },
        limits = c(0, 510)
      ) +
      # Add text labels for gray tiles
      geom_text(
        data = gray_tiles,
        aes(
          x = (startMinsSince8 + endMinsSince8) / 2,
          y = as.numeric(dayOfWeek),
          label = "Common\nhour"
        ),
        size = 5, color = "black", fontface = "bold"
      ) +
      # Customize the Y-axis to label days of the week correctly
      scale_y_continuous(
        breaks = 1:5,
        labels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri")),
        expand = c(0, 0)
        #expand = expansion(add = 0.5)  # Adds padding around labels to improve alignment
      ) +
      # Adjust the scale for segment size (width) for better visualization
      scale_size_continuous(range = c(5, 30)) +  # Adjust range as needed for visibility
      labs(
        x = "Time",
        y = "Day of the Week"
      ) +
      # Flip coordinates to make days on the Y-axis and time on the X-axis
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          size = 14,
          face = "bold",
          color = "black"
        ), # Change X-axis text size
        axis.text.y = element_text(
          angle = 0, 
          hjust = 1, 
          size  = 14,
          face  = "bold",
          color = "black"
        ),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),  # Remove default horizontal grid lines
        plot.margin = unit(c(1, 1, 1, 1), "lines")  # Adjust plot margins as needed
      ) +
      # Add custom horizontal lines to mark day boundaries
      geom_hline(yintercept = seq(0.5, 5.5, by = 1), color = "grey70")
  },height = 900)

}

########################################################
# Shiny app: run the app!
########################################################

shinyApp(ui = ui, server = server)

########################################################
# end
########################################################