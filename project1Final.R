library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggpattern)
library(tidyr)
library(plotly)
library(scales)

# Load and preprocess the dataset
# Data as of 06/28/2023, start date of 01/01/2020, end date of 06/24/2023
covid_data <- read.csv("covid.csv")
covid_data <- covid_data[, -(1:3)] # Remove metadata columns
colnames(covid_data) <- c("sex", "age", "total_deaths", "covid_deaths") # Rename columns
covid_data <- covid_data[-(1:4), ] # Drop metadata rows
covid_data$sex <- factor(covid_data$sex)
covid_data$age <- c(rep(1:84, each = 2), "85+", "85+")
covid_data$age <- factor(covid_data$age,
                         levels = c(seq(1, 84), "85+"),
                         ordered = TRUE) # Treat age as an ordered factor
count_data <- covid_data %>% 
  mutate(prop_covid_death = covid_deaths / total_deaths)

ui <- dashboardPage(
  dashboardHeader(title = "U.S. COVID-19 Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info")),
      menuItem("Data Visualizations", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Proportions Table", tabName = "props_table", icon = icon("table")),
      menuItem("Counts Table", tabName = "counts_table", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .dataTables_wrapper .dataTables_length, 
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info, 
        .dataTables_wrapper .dataTables_paginate {
          margin-bottom: 20px;
        }
        .box {
          width: 100% !important;
        }
        table.dataTable {
          width: 100% !important;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "overview",
              h2("U.S. COVID-19 2020-2023 Deaths Data Dashboard"),
              p("This dashboard provides interactive visualization and tables of COVID-19 data in the United States."),
              p("Data Source:", a("U.S. Government COVID Deaths Data", 
                                  href = "https://catalog.data.gov/dataset/provisional-covid-19-deaths-counts-by-age-in-years")),
              h3("Description of Variables:"),
              tags$ul(
                tags$li("sex: The sex of the group (male or female)."),
                tags$li("age: The age group of the population."),
                tags$li("total_deaths: Total deaths in the group."),
                tags$li("covid_deaths: Deaths attributed to COVID-19 in the group.")
              ),
              p("Instructions: Use the sidebar menu to explore the dashboard. Filters allow customization of plots and tables. Use the 'Download Source Code' button below to access the source code."),
              downloadButton("downloadSource", "Download Source Code")
      ),
      tabItem(tabName = "visualization",
              fluidRow(
                box(title = "Select Sex", width = 3,
                    checkboxGroupInput("sex_select", "Sex:", 
                                       choices = levels(covid_data$sex),
                                       selected = levels(covid_data$sex)) # Default to all levels to avoid empty data
                ),
                box(title = "Select Death Types", width = 3,
                    checkboxGroupInput("death_type_select", "Death Types:",
                                       choices = c("COVID-19 Deaths" = "covid_deaths", 
                                                   "Total Deaths" = "total_deaths"),
                                       selected = c("covid_deaths", "total_deaths")) # Default to all death types
                ),
                box(title = "Select Age Range", width = 6,
                    uiOutput("age_range_ui") # Dynamic slider for age range
                )
              ),
              fluidRow(
                box(title = "COVID-19 Death Data Plot", width = 12,
                    plotlyOutput("linePlot")
                ),
                fluidRow(
                  column(width = 12, HTML("&nbsp;"))
                ),
                box(title = "Deaths by Sex and Type", width = 12,
                    plotlyOutput("barPlot")
                ),
                fluidRow(
                  column(width = 12, HTML("&nbsp;"))
                ),
                box(title = "Proportion of Deaths Due to COVID by Age and Sex", width = 12,
                    plotlyOutput("propPlot")
                )
              )
      ),
      tabItem(tabName = "props_table",
              fluidRow(
                box(title = "Select Sex", width = 3,
                    checkboxGroupInput("sex_select_props", "Sex:", 
                                       choices = levels(covid_data$sex),
                                       selected = levels(covid_data$sex)) # Default to all levels to avoid empty data
                ),
                box(title = "Select Age Range for Table", width = 6,
                    uiOutput("age_range_ui_props") # Dynamic slider for age range in the table
                ),
                box(title = "Select Proportion Range", width = 3,
                    sliderInput("prop_range", "Proportion Range:",
                                min = 0, max = 0.12,
                                value = c(0, 0.12),
                                step = 0.01)
                ),
                box(title = "COVID-19 Death Proportions Table", width = 12,
                    dataTableOutput("propsTable")
                )
              )
      ),
      tabItem(tabName = "counts_table",
              fluidRow(
                box(title = "Select Sex", width = 3,
                    checkboxGroupInput("sex_select_counts", "Sex:", 
                                       choices = levels(covid_data$sex),
                                       selected = levels(covid_data$sex)) # Default to all levels to avoid empty data
                ),
                box(title = "Select Death Types", width = 3,
                    checkboxGroupInput("death_type_select_counts", "Death Types:",
                                       choices = c("COVID-19 Deaths" = "covid_deaths", 
                                                   "Total Deaths" = "total_deaths"),
                                       selected = c("covid_deaths", "total_deaths")) # Default to all death types
                ),
                box(title = "Select Age Range for Table", width = 6,
                    uiOutput("age_range_ui_counts") # Dynamic slider for age range in the table
                ),
                box(title = "COVID-19 Death Counts Table", width = 12,
                    dataTableOutput("countsTable")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  # Dynamic UI for age range slider
  output$age_range_ui <- renderUI({
    sliderInput("age_range", "Age Range:",
                min = 1, max = length(levels(covid_data$age)),
                value = c(18, length(levels(covid_data$age)) - 5),
                step = 1,
                ticks = FALSE,
                animate = TRUE)
  })
  
  output$age_range_ui_counts <- renderUI({
    sliderInput("age_range_counts", "Age Range for Counts Table:",
                min = 1, max = length(levels(covid_data$age)),
                value = c(1, length(levels(covid_data$age))),
                step = 1,
                ticks = FALSE)
  })
  
  output$age_range_ui_props <- renderUI({
    sliderInput("age_range_props", "Age Range for Proportions Table:",
                min = 1, max = length(levels(covid_data$age)),
                value = c(1, length(levels(covid_data$age))),
                step = 1,
                ticks = FALSE)
  })
  
  # Filtered data based on user inputs for visualization tab
  filtered_data <- reactive({
    req(input$sex_select, input$age_range, input$death_type_select)
    
    age_levels <- levels(covid_data$age)
    selected_ages <- age_levels[input$age_range[1]:input$age_range[2]]
    covid_data %>%
      mutate(age = as.character(age)) %>%
      filter(sex %in% input$sex_select, age %in% selected_ages) %>%
      pivot_longer(cols = c(total_deaths, covid_deaths), 
                   names_to = "death_type", values_to = "death_count") %>%
      filter(death_type %in% input$death_type_select)
  })
  
  # Filtered data for counts table based on user inputs
  filtered_counts <- reactive({
    req(input$sex_select_counts, input$age_range_counts, input$death_type_select_counts)
    
    age_levels <- levels(covid_data$age)
    selected_ages <- age_levels[input$age_range_counts[1]:input$age_range_counts[2]]
    
    covid_data %>%
      mutate(age = as.character(age)) %>%
      filter(sex %in% input$sex_select_counts, age %in% selected_ages) %>%
      pivot_longer(cols = c(total_deaths, covid_deaths), 
                   names_to = "death_type", values_to = "death_count") %>%
      filter(death_type %in% input$death_type_select_counts) %>%
      mutate(age = factor(age, levels = c(as.character(seq(1, 84)), "85+"), ordered = TRUE),
             death_type = case_when(
               death_type == "covid_deaths" ~ "COVID Deaths",
               death_type == "total_deaths" ~ "Total Deaths"
             )) %>% # Ensure age is treated as an ordered factor for correct sorting
      arrange(age) %>%
      select(sex, age, death_type, death_count)
  })
  
  # Filtered data for proportions table based on user inputs
  filtered_props <- reactive({
    req(input$sex_select_props, input$age_range_props, input$prop_range)
    
    age_levels <- levels(count_data$age)
    selected_ages <- age_levels[input$age_range_props[1]:input$age_range_props[2]]
    count_data %>%
      filter(sex %in% input$sex_select_props, age %in% selected_ages,
             prop_covid_death >= input$prop_range[1], prop_covid_death <= input$prop_range[2]) %>%
      select(sex, age, covid_deaths, total_deaths, prop_covid_death)
  })
  
  # Line plot
  output$linePlot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0) # Make sure data is available
    
    # Add more user-friendly labels for death types
    data <- data %>%
      mutate(
        death_type_label = case_when(
          death_type == "covid_deaths" ~ "COVID Deaths",
          death_type == "total_deaths" ~ "Total Deaths",
          TRUE ~ death_type
        )
      )
    
    x_breaks <- levels(covid_data$age)
    x_labels <- ifelse(as.integer(x_breaks) %% 5 == 0 | x_breaks == "85+", x_breaks, "")
    
    # Update ggplot to use `death_type_label`
    gg <- ggplot(data, aes(x = age, y = death_count, 
                           color = sex, linetype = death_type_label, 
                           group = interaction(sex, death_type_label),
                           text = paste("Age:", age,
                                        "<br>Gender:", sex,
                                        "<br>Death Type:", death_type_label,  
                                        "<br>Death Count:", comma(death_count)))) +
      geom_line() +
      scale_x_discrete(labels = x_labels, limits = levels(covid_data$age)) +
      labs(title = "Death Counts by Age and Death Type",
           x = "Age Group", y = "Death Count") +
      scale_color_manual(values = c("Male" = "blue", "Female" = "red"), 
                         labels = c("Male" = "Men", "Female" = "Women")) +
      scale_linetype_manual(values = c("COVID Deaths" = "dashed", "Total Deaths" = "solid"),
                            labels = c("COVID Deaths" = "COVID Deaths", "Total Deaths" = "Total Deaths (All Causes)")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
    
    ggplotly(gg, tooltip = "text")
  })
  
  
  output$barPlot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0) # Ensure data is available
    
    # Summarize data to get the total death count per sex and death type
    summarized_data <- data %>%
      group_by(sex, death_type) %>%
      summarise(death_count = sum(death_count, na.rm = TRUE)) %>%
      ungroup() %>%
      # Rename `death_type` values for display purposes
      mutate(
        death_type_label = case_when(
          death_type == "covid_deaths" ~ "COVID Deaths",
          death_type == "total_deaths" ~ "Total Deaths",
          TRUE ~ death_type
        ),
        sex_death_type = paste(sex, death_type, sep = "_") # For custom colors
      )
    
    # Define custom colors for each combination
    custom_colors <- c(
      "Male_covid_deaths" = "lightblue",
      "Male_total_deaths" = "blue",
      "Female_covid_deaths" = "pink",
      "Female_total_deaths" = "red"
    )
    
    # Create the ggplot object
    gg <- ggplot(summarized_data, aes(
      x = sex,
      y = death_count,
      fill = sex_death_type, # Use the new column for fill
      text = paste("Sex:", sex,
                   "<br>Death Type:", death_type_label, # Use renamed labels
                   "<br>Death Count:", scales::comma(death_count))
    )) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Deaths by Gender and Type",
        x = "Gender",
        y = "Death Count"
      ) +
      theme_minimal() +
      scale_fill_manual(values = custom_colors) # Apply the custom colors
    
    # Convert ggplot object to Plotly for interactivity
    ggplotly(gg, tooltip = "text")
  })
  
  # Proportions graph for Visualization tab
  output$propPlot <- renderPlotly({
    # Using filters from the Data Visualization tab, not the Proportions Table tab
    req(input$sex_select, input$age_range)
    
    age_levels <- levels(covid_data$age)
    selected_ages <- age_levels[input$age_range[1]:input$age_range[2]]
    filtered_data_for_prop <- count_data %>%
      filter(sex %in% input$sex_select, age %in% selected_ages)
    
    req(nrow(filtered_data_for_prop) > 0) # Make sure data is available
    
    gg <- ggplot(data = filtered_data_for_prop, aes(x = age, y = prop_covid_death, group = sex, color = sex,
                                                    text = paste("Age:", age,
                                                                 "<br>Gender:", sex,
                                                                 "<br>COVID Death Proportion:", round(prop_covid_death, 4), 
                                                                 "<br>COVID Death Percentage:", round(prop_covid_death*100, 2), "%"))) +
      geom_line(size = 1) +
      labs(title = "Proportion of Deaths Due to COVID-19 by Age and Sex",
           x = "Age Group", y = "Proportion of COVID-19 Deaths") +
      scale_color_manual(values = c("Male" = "blue", "Female" = "red"),
                         labels = c("Male" = "Men", "Female" = "Women")) +
      scale_x_discrete(labels = ifelse(levels(covid_data$age) %in% c(seq(5, 85, by = 5), "85+"), levels(covid_data$age), ""),
                       limits = levels(covid_data$age)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
    
    ggplotly(gg, tooltip = "text")
  })
  
  # Data table for Counts Table tab
  output$countsTable <- renderDataTable({
    filtered_counts() %>%
      mutate(death_count = comma(death_count)) %>%
      rename(
        "Gender" = sex,
        "Age Group" = age,
        "Death Type" = death_type,
        "Death Count" = death_count
      )
  }, options = list(pageLength = 15, autoWidth = TRUE))
  
  # Data table for Proportions Table tab
  output$propsTable <- renderDataTable({
    filtered_props() %>%
      mutate(prop_covid_death = round(prop_covid_death, 4),
             covid_deaths = comma(covid_deaths), 
             total_deaths = comma(total_deaths)) %>%
      rename(
        "Gender" = sex,
        "Age Group" = age,
        "Death Type" = covid_deaths,
        "Death Count" = total_deaths,
        "COVID Death Proportion" = prop_covid_death
      )
  }, options = list(pageLength = 15, autoWidth = TRUE))
  
  # Downloadable source code
  output$downloadSource <- downloadHandler(
    filename = function() { "dashboard_source_code.txt" },
    content = function(file) {
      writeLines(capture.output(dump("shinyApp", ""), sep = "\n"), file)
    }
  )
}

shinyApp(ui = ui, server = server)

