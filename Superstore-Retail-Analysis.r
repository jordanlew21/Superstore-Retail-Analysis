# =========================================================
# PACKAGES
# =========================================================
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(shiny)
library(plotly)
library(stringr)
library(scales)

# =========================================================
# LOAD DATA
# =========================================================
superstore <- read_excel("superstore.xlsx")
gdp <- read_excel("API_NY.GDP.PCAP.CD_DS2_en_excel_v2_260577.xls", skip = 4)
cpi <- read_excel("CPIAUCSL.xlsx", sheet = 2)
weather <- read_excel("open-meteo-40.74N74.04W32m.xlsx", skip = 2)

# =========================================================
# CLEAN GDP
# =========================================================
colnames(gdp)[1:4] <- c("Country", "Country_Code", "Indicator", "Indicator_Code")
gdp <- gdp[, -c(2,3,4)]

years <- seq(1960, 1960 + ncol(gdp) - 2)
colnames(gdp)[2:ncol(gdp)] <- years

gdp <- gdp %>%
  pivot_longer(cols = -Country,
               names_to = "Year",
               values_to = "GDP_per_capita")

gdp$Year <- as.integer(gdp$Year)
gdp$GDP_per_capita <- as.numeric(gdp$GDP_per_capita)

gdp <- gdp %>%
  filter(Country %in% c("United States", "United States of America"),
         Year >= 2011, Year <= 2014)

gdp$Country[gdp$Country == "United States of America"] <- "United States"

# =========================================================
# SUPERSTORE CLEAN
# =========================================================
superstore_us <- superstore %>%
  filter(Country == "United States") %>%
  mutate(
    Order.Date = as.Date(Order.Date),
    Year = year(Order.Date),
    YearMonth = floor_date(Order.Date, "month")
  ) %>%
  filter(Year >= 2011, Year <= 2014)

# =========================================================
# CPI CLEAN
# =========================================================
cpi_us <- cpi %>%
  mutate(
    observation_date = as.Date(observation_date),
    YearMonth = floor_date(observation_date, "month")
  ) %>%
  filter(observation_date >= as.Date("2011-01-01"),
         observation_date <= as.Date("2014-12-31")) %>%
  group_by(YearMonth) %>%
  summarise(CPI = mean(CPIAUCSL, na.rm = TRUE))

# =========================================================
# WEATHER CLEAN
# =========================================================
weather_us <- weather %>%
  mutate(
    time = ymd_hms(time),
    YearMonth = floor_date(time, "month")
  ) %>%
  filter(time >= as.Date("2011-01-01"),
         time <= as.Date("2014-12-31")) %>%
  group_by(YearMonth) %>%
  summarise(
    avg_temp = mean(`temperature_2m (°C)`, na.rm = TRUE),
    total_precip = sum(`precipitation (mm)`, na.rm = TRUE)
  )

# =========================================================
# MERGE DATA
# =========================================================
data_merged <- superstore_us %>%
  left_join(gdp, by = c("Country", "Year")) %>%
  left_join(cpi_us, by = "YearMonth") %>%
  left_join(weather_us, by = "YearMonth") %>%
  filter(Year >= 2011, Year <= 2014)

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  
  titlePanel("US Retail Performance vs External Factors (2011–2014)"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectizeInput(
        "region", "Region:",
        choices = unique(data_merged$Region),
        selected = NULL,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      
      actionButton("select_all_region", "Select All Regions"),
      actionButton("clear_region", "Clear Regions"),
      
      tags$hr(),
      
      selectizeInput(
        "category", "Category:",
        choices = unique(data_merged$Category),
        selected = NULL,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      
      actionButton("select_all_category", "Select All Categories"),
      actionButton("clear_category", "Clear Categories"),
      
      tags$hr(),
      
      sliderInput("year", "Year Range:",
                  min = 2011, max = 2014,
                  value = c(2011, 2014),
                  sep = "")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Sales vs CPI & GDP", plotlyOutput("p1")),
        tabPanel("Discount vs Sales", plotlyOutput("p2")),
        tabPanel("Inflation Impact", plotlyOutput("p3")),
        tabPanel("Weather Impact", plotlyOutput("p4")),
        tabPanel("Profit Efficiency", plotlyOutput("p5"))
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session) {
  
  # ================= BUTTON LOGIC =================
  
  observeEvent(input$select_all_region, {
    updateSelectizeInput(session, "region",
                         selected = unique(data_merged$Region))
  })
  
  observeEvent(input$clear_region, {
    updateSelectizeInput(session, "region",
                         selected = character(0))
  })
  
  observeEvent(input$select_all_category, {
    updateSelectizeInput(session, "category",
                         selected = unique(data_merged$Category))
  })
  
  observeEvent(input$clear_category, {
    updateSelectizeInput(session, "category",
                         selected = character(0))
  })
  
  # ================= FILTER =================
  filtered_data <- reactive({
    
    df <- data_merged %>%
      filter(
        Country == "United States",
        Year >= input$year[1],
        Year <= input$year[2]
      )
    
    if (length(input$region) > 0) {
      df <- df %>% filter(Region %in% input$region)
    }
    
    if (length(input$category) > 0) {
      df <- df %>% filter(Category %in% input$category)
    }
    
    df
  })
  
  # =========================================================
  # P1
  # =========================================================
  output$p1 <- renderPlotly({
    
    df <- filtered_data() %>%
      group_by(YearMonth) %>%
      summarise(
        Sales = sum(Sales, na.rm = TRUE),
        CPI = mean(CPI, na.rm = TRUE),
        GDP = mean(GDP_per_capita, na.rm = TRUE)
      ) %>%
      mutate(
        CPI_scaled = CPI * max(Sales) / max(CPI),
        GDP_scaled = GDP * max(Sales) / max(GDP)
      )
    
    p <- ggplot(df, aes(x = YearMonth)) +
      geom_line(aes(y = Sales, color = "Sales"), size = 1.2) +
      geom_point(aes(y = Sales,
                     text = paste("Sales:", dollar(Sales)))) +
      geom_line(aes(y = CPI_scaled, color = "CPI"), linetype = "dashed") +
      geom_line(aes(y = GDP_scaled, color = "GDP"), linetype = "dotted") +
      scale_color_manual(values = c(
        "Sales" = "blue",
        "CPI" = "red",
        "GDP" = "darkgreen"
      )) +
      theme_minimal() +
      labs(title = "Sales vs Macro Factors",
           x = "Time", y = "Scaled Values")
    
    ggplotly(p, tooltip = "text")
  })
  
  # =========================================================
  # P2
  # =========================================================
  output$p2 <- renderPlotly({
    
    df <- filtered_data() %>%
      mutate(
        Discount_group = cut(
          Discount,
          breaks = c(-0.01, 0, 0.2, 0.4, 1),
          labels = c(
            "No Discount (0%)",
            "Low Discount (1–20%)",
            "Medium Discount (21–40%)",
            "High Discount (41–100%)"
          )
        )
      ) %>%
      group_by(Discount_group) %>%
      summarise(
        Avg_Sales = mean(Sales, na.rm = TRUE),
        Avg_Profit = mean(Profit, na.rm = TRUE)
      )
    
    p <- ggplot(df, aes(
      x = Discount_group,
      y = Avg_Sales,
      text = paste(
        "Discount Category:", Discount_group,
        "<br>Average Sales:", dollar(Avg_Sales),
        "<br>Average Profit:", dollar(Avg_Profit),
        "<br><br>Interpretation:",
        "<br>• No Discount = Full price sales",
        "<br>• Low = 1–20% discount",
        "<br>• Medium = 21–40% discount",
        "<br>• High = 41–100% discount"
      )
    )) +
      geom_col(fill = "steelblue") +
      theme_minimal() +
      labs(
        title = "Discount Level vs Average Sales",
        subtitle = "Comparison of pricing strategies and their impact on sales performance",
        x = "Discount Category",
        y = "Average Sales ($)"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # =========================================================
  # P3 
  # =========================================================
  output$p3 <- renderPlotly({
    
    df <- filtered_data() %>%
      mutate(
        CPI_group = cut(CPI,
                        breaks = 3,
                        labels = c("Low Inflation", "Medium", "High"))
      )
    
    df$Sales_label <- dollar(df$Sales)
    
    plot_ly(
      df,
      x = ~CPI_group,
      y = ~Sales,
      type = "box",
      color = ~CPI_group,
      text = ~paste("Inflation:", CPI_group,
                    "<br>Sales:", Sales_label),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Sales Across Inflation Levels",
        xaxis = list(title = "Inflation"),
        yaxis = list(title = "Sales ($)")
      )
  })
  
  # =========================================================
  # P4
  # =========================================================
  output$p4 <- renderPlotly({
    
    df <- filtered_data() %>%
      group_by(YearMonth) %>%
      summarise(
        Sales = sum(Sales, na.rm = TRUE),
        Temp = mean(avg_temp, na.rm = TRUE)
      ) %>%
      mutate(
        Month = month(YearMonth, label = TRUE),
        Year = year(YearMonth)
      )
    
    p <- ggplot(df, aes(
      x = Month,
      y = Sales,
      group = Year,
      color = factor(Year),
      text = paste(
        "Year:", Year,
        "<br>Month:", Month,
        "<br>Sales:", dollar(Sales),
        "<br>Temp:", round(Temp, 1)
      )
    )) +
      geom_line() +
      geom_point() +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # =========================================================
  # P5
  # =========================================================
  output$p5 <- renderPlotly({
    
    df <- superstore_us %>%
      group_by(Category) %>%
      summarise(
        Sales = sum(Sales),
        Profit = sum(Profit),
        Margin = Profit / Sales
      )
    
    p <- ggplot(df, aes(
      x = reorder(Category, Margin),
      y = Margin,
      text = paste(
        "Category:", Category,
        "<br>Sales:", dollar(Sales),
        "<br>Profit:", dollar(Profit),
        "<br>Margin:", round(Margin, 3)
      )
    )) +
      geom_col(fill = "purple") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Profit Efficiency",
           x = "Category", y = "Margin")
    
    ggplotly(p, tooltip = "text")
  })
}

# =========================================================
# RUN APP
# =========================================================
shinyApp(ui = ui, server = server)