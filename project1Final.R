library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

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

ui <- dashboardPage(
  dashboardHeader(title = "U.S. COVID-19 Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info")),
      menuItem("Data Visualization", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Data Table", tabName = "table", icon = icon("table"))
    )
  ),
  dashboardBody(
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
                                       selected = levels(covid_data$sex))
                ),
                box(title = "Select Death Types", width = 3,
                    checkboxGroupInput("death_type_select", "Death Types:",
                                       choices = c("COVID-19 Deaths" = "covid_deaths", 
                                                   "Total Deaths" = "total_deaths"),
                                       selected = c("covid_deaths", "total_deaths"))
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
                )
              )
      ),
      tabItem(tabName = "table",
              fluidRow(
                box(title = "Select Sex", width = 3,
                    checkboxGroupInput("sex_select_table", "Sex:", 
                                       choices = levels(covid_data$sex),
                                       selected = levels(covid_data$sex))
                ),
                box(title = "Select Death Types", width = 3,
                    checkboxGroupInput("death_type_select_table", "Death Types:",
                                       choices = c("COVID-19 Deaths" = "covid_deaths", 
                                                   "Total Deaths" = "total_deaths"),
                                       selected = c("covid_deaths", "total_deaths"))
                ),
                box(title = "Select Age Range for Table", width = 6,
                    uiOutput("age_range_ui_table") # Dynamic slider for age range in the table
                ),
                box(title = "COVID-19 Data Table", width = 12,
                    dataTableOutput("covidTable")
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
                value = c(18, length(levels(covid_data$age))-5),
                step = 1,
                ticks = FALSE,
                animate = TRUE)
  })
  
  # Dynamic UI for age range slider for table
  output$age_range_ui_table <- renderUI({
    sliderInput("age_range_table", "Age Range for Table:",
                min = 1, max = length(levels(covid_data$age)),
                value = c(1, length(levels(covid_data$age))),
                step = 1,
                ticks = FALSE,
                animate = TRUE)
  })
  
  # Filtered data based on user inputs
  filtered_data <- reactive({
    age_levels <- levels(covid_data$age)
    selected_ages <- age_levels[input$age_range[1]:input$age_range[2]]
    covid_data %>%
      mutate(age = as.character(age)) %>%
      filter(sex %in% input$sex_select, age %in% selected_ages) %>%
      pivot_longer(cols = c(total_deaths, covid_deaths), 
                   names_to = "death_type", values_to = "death_count") %>%
      filter(death_type %in% input$death_type_select)
  })
  
  filtered_table <- reactive({
    age_levels <- levels(covid_data$age)
    selected_ages <- age_levels[input$age_range_table[1]:input$age_range_table[2]]
    covid_data %>%
      mutate(age = as.character(age)) %>%
      filter(sex %in% input$sex_select_table, age %in% selected_ages) %>%
      pivot_longer(cols = c(total_deaths, covid_deaths), 
                   names_to = "death_type", values_to = "death_count") %>%
      filter(death_type %in% input$death_type_select_table)
  })
  
  # Line plot
  output$linePlot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(NULL) # Avoid errors if no data matches filters
    }
    
    x_breaks <- levels(covid_data$age)[seq(5, length(levels(covid_data$age)), by = 5)]
    x_breaks <- c(x_breaks, "85+")
    
    gg <- ggplot(data, aes(x = age, y = death_count, 
                           color = sex, linetype = death_type, 
                           group = interaction(sex, death_type))) +
      geom_line() +
      scale_x_discrete(breaks = x_breaks, limits = levels(covid_data$age)) +
      labs(title = "Death Counts by Age and Death Type",
           x = "Age Group", y = "Death Count") +
      scale_color_manual(values = c("Male" = "blue", "Female" = "red"), 
                         labels = c("Male" = "Men", "Female" = "Women")) +
      scale_linetype_manual(values = c("covid_deaths" = "dashed", "total_deaths" = "solid"),
                            labels = c("covid_deaths" = "COVID-19 Deaths", "total_deaths" = "Total Deaths (All Causes)")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
    ggplotly(gg)
  })
  
  # Bar plot
  output$barPlot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(NULL) # Avoid errors if no data matches filters
    }
    gg <- ggplot(data, aes(x = sex, y = death_count, fill = death_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Deaths by Sex and Type", x = "Sex", y = "Death Count") +
      theme_minimal()
    ggplotly(gg)
  })
  
  # Data table
  output$covidTable <- renderDataTable({
    filtered_table()
  })
  
  # Downloadable source code
  output$downloadSource <- downloadHandler(
    filename = function() { "dashboard_source_code.txt" },
    content = function(file) {
      writeLines(capture.output(dump("shinyApp", ""), sep = "\n"), file)
    }
  )
}

shinyApp(ui = ui, server = server)
