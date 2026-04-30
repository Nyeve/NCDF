# ---------------------------------------------------
# Shiny Dashboard: NCDF Allocation Trends (Stakeholder-Friendly)
# ---------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(scales)

# Assume NCDF_long is preloaded with columns: County, Constituency, Year, Allocation

ui <- dashboardPage(
  dashboardHeader(title = "NCDF Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Trend Plot", tabName = "trend", icon = icon("line-chart")),
      menuItem("Comparison", tabName = "comparison", icon = icon("bar-chart")),
      menuItem("Growth Rates", tabName = "growth", icon = icon("chart-area")),
      menuItem("Top Performers", tabName = "top", icon = icon("trophy")),
      menuItem("Cross-County", tabName = "cross", icon = icon("columns"))
    ),
    selectInput("county", "Select County:",
                choices = unique(NCDF_long$County),
                selected = unique(NCDF_long$County)[1]),
    uiOutput("constituencyUI"),
    sliderInput("yearRange", "Select Year Range:",
                min = min(NCDF_long$Year, na.rm = TRUE),
                max = max(NCDF_long$Year, na.rm = TRUE),
                value = c(min(NCDF_long$Year, na.rm = TRUE), max(NCDF_long$Year, na.rm = TRUE)),
                step = 1,
                sep = ""),
    radioButtons("scaleType", "Choose Scale:",
                 choices = c("Raw Totals" = "raw", 
                             "Millions" = "millions", 
                             "Percentages" = "percent"),
                 selected = "millions"),
    downloadButton("downloadData", "Download Filtered Data")
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("totalBox"),
                valueBoxOutput("avgBox"),
                valueBoxOutput("growthBox")
              )
      ),
      tabItem(tabName = "trend",
              fluidRow(
                box(title = "Allocation Trend", status = "primary", solidHeader = TRUE,
                    width = 12, plotOutput("trendPlot"))
              )
      ),
      tabItem(tabName = "comparison",
              fluidRow(
                box(title = "Allocation Comparison", status = "info", solidHeader = TRUE,
                    width = 12, plotOutput("barChart"))
              )
      ),
      tabItem(tabName = "growth",
              fluidRow(
                box(title = "Growth Rate of Allocations", status = "warning", solidHeader = TRUE,
                    width = 12, plotOutput("growthChart"))
              )
      ),
      tabItem(tabName = "top",
              fluidRow(
                box(title = "Top Constituency per Year", status = "success", solidHeader = TRUE,
                    width = 12, DTOutput("topPerformers"))
              )
      ),
      tabItem(tabName = "cross",
              fluidRow(
                box(title = "Cross-County Stacked Comparison", status = "danger", solidHeader = TRUE,
                    width = 12, plotOutput("facetStackedBar"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$constituencyUI <- renderUI({
    selectInput("constituency", "Select Constituency:",
                choices = unique(NCDF_long$Constituency[NCDF_long$County == input$county]),
                selected = unique(NCDF_long$Constituency[NCDF_long$County == input$county])[1])
  })
  
  filteredData <- reactive({
    NCDF_long %>%
      filter(County == input$county,
             Year >= input$yearRange[1],
             Year <= input$yearRange[2],
             Constituency == input$constituency)
  })
  
  output$totalBox <- renderValueBox({
    total <- sum(filteredData()$Allocation, na.rm = TRUE)
    valueBox(comma(total), "Total Allocation", icon = icon("coins"), color = "blue")
  })
  
  output$avgBox <- renderValueBox({
    avg <- mean(filteredData()$Allocation, na.rm = TRUE)
    valueBox(round(avg, 2), "Average Allocation", icon = icon("calculator"), color = "green")
  })
  
  output$growthBox <- renderValueBox({
    df <- filteredData()
    growth <- ifelse(nrow(df) > 1,
                     (last(df$Allocation) - first(df$Allocation)) / first(df$Allocation) * 100,
                     NA)
    valueBox(ifelse(is.na(growth), "N/A", paste0(round(growth, 2), "%")),
             "Growth Rate", icon = icon("chart-line"), color = "purple")
  })
  
  output$trendPlot <- renderPlot({
    df <- filteredData()
    ggplot(df, aes(x = Year, y = Allocation)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(color = "darkorange", size = 2) +
      labs(title = paste("Allocation Trend:", input$constituency),
           y = "Allocation (KES)") +
      theme_minimal()
  })
  
  # Bar Chart with scale toggle
  output$barChart <- renderPlot({
    df <- NCDF_long %>%
      filter(County == input$county,
             Year >= input$yearRange[1],
             Year <= input$yearRange[2]) %>%
      group_by(Constituency) %>%
      summarise(Total_Allocation = sum(Allocation, na.rm = TRUE), .groups = "drop")
    
    if (input$scaleType == "millions") {
      df <- df %>% mutate(Display = Total_Allocation/1e6)
      ylab <- "Allocation (Million KES)"
      label_fun <- function(x) paste0(round(x,1), " M")
    } else if (input$scaleType == "percent") {
      df <- df %>% mutate(Display = Total_Allocation/sum(Total_Allocation)*100)
      ylab <- "Share of County Allocation (%)"
      label_fun <- function(x) paste0(round(x,1), "%")
    } else {
      df <- df %>% mutate(Display = Total_Allocation)
      ylab <- "Allocation (KES)"
      label_fun <- function(x) comma(x)
    }
    
    ggplot(df, aes(x = reorder(Constituency, -Display), y = Display, fill = Constituency)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = label_fun(Display)), vjust = -0.5, size = 3) +
      labs(title = paste("Allocation Comparison in", input$county),
           y = ylab) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
  })
  
  output$growthChart <- renderPlot({
    df <- NCDF_long %>%
      filter(County == input$county) %>%
      group_by(Constituency) %>%
      summarise(GrowthRate = (last(Allocation) - first(Allocation)) / first(Allocation) * 100,
                .groups = "drop")
    
    ggplot(df, aes(x = reorder(Constituency, GrowthRate), y = GrowthRate, fill = Constituency)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(round(GrowthRate,1), "%")), hjust = -0.1) +
      coord_flip() +
      labs(title = paste("Growth Rate of Allocations (2014–2026) in", input$county),
           y = "Growth Rate (%)") +
      theme_minimal()
  })
  
  output$topPerformers <- renderDT({
    df <- NCDF_long %>%
      filter(County == input$county,
             Year >= input$yearRange[1],
             Year <= input$yearRange[2]) %>%
      group_by(Year) %>%
      slice_max(order_by = Allocation, n = 1, with_ties = FALSE)
    
    datatable(df[, c("Year", "County", "Constituency", "Allocation")],
              options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  output$facetStackedBar <- renderPlot({
    df <- NCDF_long %>%
      filter(Year >= input$yearRange[1],
             Year <= input$yearRange[2],
             County %in% c("Kisii", "Nyamira", "Kajiado"))
    
    ggplot(df, aes(x = factor(Year), y = Allocation/1e6, fill = Constituency)) +
      geom_bar(stat = "identity") +
      facet_wrap(~County, scales = "free_y") +
      labs(title = "Yearly Allocations by Constituency (Faceted by County)",
           y = "Allocation (Million KES)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    