library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggpattern)
library(tidyr)
library(plotly)
library(scales)

covid_data <- read.csv("covid.csv")
covid_data <- covid_data[, -(1:3)]
colnames(covid_data) <- c("sex", "age", "total_deaths", "covid_deaths")
covid_data <- covid_data[-(1:4), ]
covid_data$sex <- factor(covid_data$sex)
covid_data$age <- c(rep(1:84, each = 2), "85+", "85+")
covid_data$age <- factor(covid_data$age, levels = c(seq(1, 84), "85+"), ordered = TRUE)
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
              h2("U.S. COVID-19 Deaths Data Dashboard (2020-2023)"),
              h3("Overview and Instructions"),
              p(style = "text-indent: 20px;", "This interactive dashboard provides visualizations and tables data on COVID-19 and total deaths from January 1st 2020 to June 28th, 2023. The data comes from the U.S. Department of Health & Human Services and provides the number of COVID-19 deaths and the number of total deaths in the United States for every age and sex combination. One important detail to note is that the date categorizes the age of everyone over the age of 84 as “85+” which may cause their deaths to appear overrepresented in the visualizations."),
              p(style = "text-indent: 20px;", "In the “Data Visualizations” tab, users can select from three inputs that alter which data is represented and taken into account on the visualizations. The user can select the sex of the population under the “Select Sex” input, the types of deaths under the “Select Death Types” input, and the age range under the “Select Age Range” input. Altering these inputs dictates which parts of the data are represented on the three visualizations. However, the “Death Type” input does not impact the proportion graph since both death types (COVID-19 deaths and total deaths) are required to compute the proportion of total deaths due to COVID-19. For the visualizations, within the age range input users can not only alter the age range, but they can also play an animation of the changing graph as it alters to the age range of the users by one year at a time. Users can hover over these visualizations on the lines or bars to get more detailed and specific information such as age, deaths, etc."), 
              p(style = "text-indent: 20px;", "Users also have the option to explore two similar tables. The “Proportions Table” provides the COVID-19 and total deaths by sex and age. It also provides an additional variable regarding the proportion of deaths due to COVID-19 for each age/sex combination. Similar to the “Data Visualizations” page, users have the option to provide inputs on sex and age range as well as the ability to filter between certain proportions of deaths due to COVID-19 deaths. The second table provided is the “Counts Table” which has deaths information for the age/sex combinations. Here, users can provide three inputs: sex, death type, and age range. For both the tables, users can utilize the search bar on the top right to search for a certain value, age, sex, or proportion. Users can also alter the number of entries shown on the top left of the tables."),
              p("Data Source:", a("U.S. Government COVID Deaths Data", href = "https://catalog.data.gov/dataset/provisional-covid-19-deaths-counts-by-age-in-years")),
              h3("Description of Variables and Inputs:"),
              tags$ul(
                tags$li("Sex: The sex of the group (Male or Female)"),
                tags$li("Age: The age group of the population"),
                tags$li("Total Deaths: Total deaths in the group"),
                tags$li("COVID Deaths: Deaths attributed to COVID-19 in the group"), 
                tags$li("Death Type: Type of death category, either Total Deaths or COVID Deaths"),
                tags$li("Proportion: The proportion of deaths caused by COVID-19 in the group")
              ),
              h3("Download Source Code"),
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
                    uiOutput("age_range_ui")
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
                                       selected = levels(covid_data$sex))
                ),
                box(title = "Select Age Range for Table", width = 6,
                    uiOutput("age_range_ui_props")
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
                                       selected = levels(covid_data$sex))
                ),
                box(title = "Select Death Types", width = 3,
                    checkboxGroupInput("death_type_select_counts", "Death Types:",
                                       choices = c("COVID-19 Deaths" = "covid_deaths", 
                                                   "Total Deaths" = "total_deaths"),
                                       selected = c("covid_deaths", "total_deaths"))
                ),
                box(title = "Select Age Range for Table", width = 6,
                    uiOutput("age_range_ui_counts")
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
  
  filtered_data <- reactive({
    req(input$sex_select, input$age_range, input$death_type_select)
    age_levels <- levels(covid_data$age)
    selected_ages <- age_levels[input$age_range[1]:input$age_range[2]]
    covid_data %>%
      mutate(age = as.character(age)) %>%
      filter(sex %in% input$sex_select, age %in% selected_ages) %>%
      pivot_longer(cols = c(total_deaths, covid_deaths), names_to = "death_type", values_to = "death_count") %>%
      filter(death_type %in% input$death_type_select)
  })
  
  filtered_counts <- reactive({
    req(input$sex_select_counts, input$age_range_counts, input$death_type_select_counts)
    age_levels <- levels(covid_data$age)
    selected_ages <- age_levels[input$age_range_counts[1]:input$age_range_counts[2]]
    covid_data %>%
      mutate(age = as.character(age)) %>%
      filter(sex %in% input$sex_select_counts, age %in% selected_ages) %>%
      pivot_longer(cols = c(total_deaths, covid_deaths), names_to = "death_type", values_to = "death_count") %>%
      filter(death_type %in% input$death_type_select_counts) %>%
      mutate(age = factor(age, levels = c(as.character(seq(1, 84)), "85+"), ordered = TRUE),
             death_type = case_when(
               death_type == "covid_deaths" ~ "COVID Deaths",
               death_type == "total_deaths" ~ "Total Deaths"
             )) %>%
      arrange(age) %>%
      select(sex, age, death_type, death_count)
  })
  
  filtered_props <- reactive({
    req(input$sex_select_props, input$age_range_props, input$prop_range)
    age_levels <- levels(count_data$age)
    selected_ages <- age_levels[input$age_range_props[1]:input$age_range_props[2]]
    count_data %>%
      filter(sex %in% input$sex_select_props, age %in% selected_ages,
             prop_covid_death >= input$prop_range[1], prop_covid_death <= input$prop_range[2]) %>%
      select(sex, age, covid_deaths, total_deaths, prop_covid_death)
  })
  
  output$linePlot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    data <- data %>%
      mutate(
        death_type_label = case_when(
          death_type == "covid_deaths" ~ " COVID Deaths",
          death_type == "total_deaths" ~ " Total Deaths",
          TRUE ~ death_type
        )
      )
    x_breaks <- levels(covid_data$age)
    x_labels <- ifelse(as.integer(x_breaks) %% 5 == 0 | x_breaks == "85+", x_breaks, "")
    gg <- ggplot(data, aes(x = age, y = death_count, 
                           color = sex, linetype = death_type_label, 
                           group = interaction(sex, death_type_label),
                           text = paste("Age:", age,
                                        "<br>Sex:", sex,
                                        "<br>Death Type:", death_type_label,  
                                        "<br>Death Count:", comma(death_count)))) +
      geom_line() +
      scale_x_discrete(labels = x_labels, limits = levels(covid_data$age)) +
      labs(title = "Death Counts by Age and Death Type",
           x = "Age Group", y = "Death Count") +
      scale_color_manual(
        values = c("Male" = "blue", "Female" = "red"), 
        name = "Sex, Death Type") +
      scale_linetype_manual(
        values = c(" COVID Deaths" = "dashed", " Total Deaths" = "solid"),
        name = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
    ggplotly(gg, tooltip = "text")
  })
  
  output$barPlot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    summarized_data <- data %>%
      group_by(sex, death_type) %>%
      summarise(death_count = sum(death_count, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(
        death_type_label = case_when(
          death_type == "covid_deaths" ~ "COVID Deaths",
          death_type == "total_deaths" ~ "Total Deaths",
          TRUE ~ death_type
        ),
        sex_death_type = case_when(
          sex == "Male" & death_type == "covid_deaths" ~ "Male COVID Deaths",
          sex == "Male" & death_type == "total_deaths" ~ "Male Total Deaths",
          sex == "Female" & death_type == "covid_deaths" ~ "Female COVID Deaths",
          sex == "Female" & death_type == "total_deaths" ~ "Female Total Deaths",
          TRUE ~ paste(sex, death_type, sep = "_")
        )
      )
    gg <- ggplot(summarized_data, aes(
      x = sex,
      y = death_count,
      fill = sex_death_type,
      text = paste("Sex:", sex,
                   "<br>Death Type:", death_type_label,
                   "<br>Death Count:", scales::comma(death_count))
    )) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Deaths by Sex and Type",
        x = "Sex",
        y = "Death Count",
        fill = "Death Type"
      ) +
      theme_minimal() +
      scale_fill_manual(
        values = c("Male COVID Deaths" = "lightblue",
                   "Male Total Deaths" = "blue",
                   "Female COVID Deaths" = "pink",
                   "Female Total Deaths" = "red"),
        name = "Sex and Death Type")
    ggplotly(gg, tooltip = "text")
  })
  
  output$propPlot <- renderPlotly({
    req(input$sex_select, input$age_range)
    age_levels <- levels(covid_data$age)
    selected_ages <- age_levels[input$age_range[1]:input$age_range[2]]
    filtered_data_for_prop <- count_data %>%
      filter(sex %in% input$sex_select, age %in% selected_ages)
    req(nrow(filtered_data_for_prop) > 0)
    gg <- ggplot(data = filtered_data_for_prop, aes(x = age, y = prop_covid_death, group = sex, color = sex,
                                                    text = paste("Age:", age,
                                                                 "<br>Sex:", sex,
                                                                 "<br>COVID Death Proportion:", round(prop_covid_death, 4), 
                                                                 "<br>COVID Death Percentage:", round(prop_covid_death*100, 2), "%"))) +
      geom_line(size = 1) +
      labs(title = "Proportion of Deaths Due to COVID-19 by Age and Sex",
           x = "Age Group", y = "COVID-19 Proportion of Deaths") +
      scale_color_manual(values = c("Male" = "blue", "Female" = "red"),
                         name = "Sex") +
      scale_x_discrete(labels = ifelse(levels(covid_data$age) %in% c(seq(5, 85, by = 5), "85+"), levels(covid_data$age), ""),
                       limits = levels(covid_data$age)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
    ggplotly(gg, tooltip = "text")
  })
  
  output$countsTable <- renderDataTable({
    filtered_counts() %>%
      mutate(death_count = comma(death_count)) %>%
      rename(
        "Sex" = sex,
        "Age Group" = age,
        "Death Type" = death_type,
        "Death Count" = death_count
      )
  }, options = list(pageLength = 15, autoWidth = TRUE))
  
  output$propsTable <- renderDataTable({
    filtered_props() %>%
      mutate(prop_covid_death = round(prop_covid_death, 4),
             covid_deaths = comma(covid_deaths), 
             total_deaths = comma(total_deaths)) %>%
      rename(
        "Sex" = sex,
        "Age Group" = age,
        "COVID Deaths" = covid_deaths,
        "Total Deaths" = total_deaths,
        "COVID Death Proportion" = prop_covid_death
      )
  }, options = list(pageLength = 15, autoWidth = TRUE))
  
  output$downloadSource <- downloadHandler(
    filename = function() { "dashboard_source_code.txt" },
    content = function(file) {
      writeLines(capture.output(dump("shinyApp", ""), sep = "\n"), file)
    }
  )
}

shinyApp(ui = ui, server = server)
