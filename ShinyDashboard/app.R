# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)  # For displaying the dataset
library(reshape2)  # For reshaping data for heatmap

# Load the dataset
data <- read.csv("processed_shoppers_data.csv")


# Create a new feature for total time spent on the site
data <- data %>% 
  mutate(Time_on_Site = Administrative_Duration + Informational_Duration + ProductRelated_Duration)

# Define the UI with enhanced design
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = tagList(
    span("Online Shopping Behavior Dashboard", style = "font-weight: bold;")
  )),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset Overview", tabName = "overview", icon = icon("table")),
      menuItem("Plot Analysis", tabName = "plots", icon = icon("chart-bar")),
      menuItem("KPIs", tabName = "kpi", icon = icon("tachometer-alt")),
      menuItem("About Dataset", tabName = "about_dataset", icon = icon("info-circle")),
      menuItem("Heatmap Analysis", tabName = "heatmap", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("/* Custom CSS styling for an enhanced design */
                              .box {border-radius: 15px;}
                              .value-box {background-color: #f0f0f0; border-radius: 15px;}
                              .tab-pane {margin-top: 15px;}"))),
    
    tabItems(
      # Tab 1: Dataset Overview
      tabItem(tabName = "overview",
              h3("Dataset Overview"),
              fluidRow(
                box(width = 12, dataTableOutput("data_summary"))
              )
              
      ),
      
      # Tab 2: KPIs for quick stats
      tabItem(tabName = "kpi",
              h3("Key Performance Indicators"),
              fluidRow(
                valueBoxOutput("total_sessions", width = 4),
                valueBoxOutput("total_revenue", width = 4),
                valueBoxOutput("avg_time_on_site", width = 4)
              )
      ),
      
      # Tab 3: Plot Analysis - Display all plots with more reactivity and functionality
      tabItem(tabName = "plots",
              h3("Interactive Plot Analysis"),
              fluidRow(
                box(width = 6, title = "Product Related Duration vs Page Value (Scatter Plot)", solidHeader = TRUE, status = "primary",
                    plotlyOutput("scatter_plot", height = "400px")),
                box(width = 6, title = "Revenue Comparison Across Visitor Types (Bar Plot)", solidHeader = TRUE, status = "primary",
                    plotlyOutput("bar_plot", height = "400px"))
              ),
              fluidRow(
                box(width = 6, title = "Time Spent on Site by Region and Revenue (Box Plot)", solidHeader = TRUE, status = "primary",
                    plotlyOutput("box_plot", height = "400px")),
                box(width = 6, title = "Traffic Source Distribution (Pie Chart)", solidHeader = TRUE, status = "primary",
                    plotlyOutput("pie_chart", height = "400px"))
              ),
              fluidRow(
                box(width = 6, title = "Violin Plot: Distribution of Time_on_Site Across Revenue", solidHeader = TRUE, status = "primary",
                    plotlyOutput("violin_plot", height = "400px")),
                box(width = 6, title = "Monthly Revenue Trends by Revenue Status (Bar Plot)", solidHeader = TRUE, status = "primary",
                    plotlyOutput("monthly_revenue_plot", height = "400px"))
              ),
              fluidRow(
                box(width = 6, title = "Revenue Generation Across Regions (Bar Plot)", solidHeader = TRUE, status = "primary",
                    plotlyOutput("region_revenue_plot", height = "400px")),
                box(width = 6, title = "Revenue Patterns Across Visitor Type and Weekend Behavior", solidHeader = TRUE, status = "primary",
                    plotlyOutput("facet_grid_plot", height = "400px"))
              ),
              fluidRow(
                box(width = 6, title = "Lollipop Chart for Traffic Type", solidHeader = TRUE, status = "primary",
                    plotlyOutput("lollipop_chart", height = "400px")),
                box(width = 6, title = "Monthly Conversion Trends by Traffic Type", solidHeader = TRUE, status = "primary",
                    plotlyOutput("monthly_conversion_plot", height = "400px"))  # New box for the monthly conversion line plot
                
              ),
              fluidRow(
                box(width = 6, title = "Treemap: Traffic Source Revenue Contributions", solidHeader = TRUE, status = "primary",
                    plotlyOutput("treemap_plot", height = "400px")),
                box(width = 6, title = "Cumulative Revenue by Visitor Type Across Months", solidHeader = TRUE, status = "primary",
                    plotlyOutput("cumulative_revenue_plot", height = "400px"))
              ),
      ),
      
      # Tab 4: About Dataset
      tabItem(tabName = "about_dataset",
              h3("About the Dataset"),
              fluidRow(
                box(title = "Dataset Description", width = 12, solidHeader = TRUE, status = "primary",
                    tags$p("This dataset comprises 12,330 sessions from an e-commerce platform, capturing visitor interactions and behaviors that influence user engagement and income generation. It includes a variety of attributes such as session duration, page views, products viewed, clicks, purchase actions, device types, and referral sources. These attributes are crucial in analyzing key business outcomes like user engagement (e.g., time spent on site, bounce rate) and revenue generation (e.g., conversion rates, average order value). The data provides valuable insights into patterns of user behavior that drive conversions and enhance customer experience, helping to identify opportunities for optimizing both engagement and profitability."),
                    tags$h4("Features Description:"),
                    tags$ul(
                      tags$li(tags$b("Administrative: "), "The number of pages a user visited under the administrative section. Range: 0 to 27."),
                      tags$li(tags$b("Administrative Duration: "), "Total time spent on administrative pages in seconds. Range: 0 to 3398 seconds."),
                      tags$li(tags$b("Informational: "), "Number of pages visited under the informational section, such as help pages or product info. Range: 0 to 24."),
                      tags$li(tags$b("Informational Duration: "), "Total time spent on informational pages in seconds. Range: 0 to 2549 seconds."),
                      tags$li(tags$b("Product Related: "), "Number of pages related to products viewed by the user. Range: 0 to 705 pages."),
                      tags$li(tags$b("Product Related Duration: "), "Total time spent on product-related pages in seconds. Range: 0 to 63172 seconds."),
                      tags$li(tags$b("Bounce Rates: "), "Percentage of visitors who leave after viewing only one page. Range: 0 to 0.2 (normalized)."),
                      tags$li(tags$b("Exit Rates: "), "Percentage of page exits during the session. Range: 0 to 0.2 (normalized)."),
                      tags$li(tags$b("Page Values: "), "Average value of a page viewed by a visitor before completing a transaction. Range: 0 to 361.76."),
                      tags$li(tags$b("Special Day: "), "Indicates closeness to a significant holiday or sales event. Range: 0 to 1 (normalized)."),
                      tags$li(tags$b("Month: "), "Month in which the session occurred (e.g., 'Feb', 'Mar')."),
                      tags$li(tags$b("Operating Systems: "), "Code representing the operating system used by the visitor. Range: 1 to 8."),
                      tags$li(tags$b("Browser: "), "Code representing the browser used by the visitor (e.g., Chrome, Firefox). Range: 1 to 13."),
                      tags$li(tags$b("Region: "), "Code representing the geographic region of the visitor. Range: 1 to 9."),
                      tags$li(tags$b("Traffic Type: "), "Source of traffic encoded as integers (e.g., organic search, paid advertising). Range: 1 to 20."),
                      tags$li(tags$b("Visitor Type: "), "Indicates whether the visitor is a Returning or New Visitor."),
                      tags$li(tags$b("Weekend: "), "Boolean indicating if the session occurred on a weekend (True/False)."),
                      tags$li(tags$b("Revenue: "), "Boolean indicating if the session resulted in a purchase (True/False)."),
                      tags$li(tags$b("Time on Site: "), "Total time spent on the site, calculated as the sum of Administrative, Informational, and Product Related durations.")
                    )
                )
              )
      ),
      
      # Tab 5: Heatmap Analysis
      tabItem(tabName = "heatmap",
              h3("Heatmap Analysis"),
              fluidRow(
                box(width = 4, title = "Select Features for Heatmap", solidHeader = TRUE, status = "primary",
                    selectInput("heatmap_features", "Choose Features:",
                                choices = names(data),
                                selected = c("ProductRelated_Duration", "Administrative_Duration", "Informational_Duration", "Time_on_Site"),
                                multiple = TRUE)),
                box(width = 8, title = "Heatmap Output", solidHeader = TRUE, status = "primary",
                    plotlyOutput("heatmap_plot", height = "600px"))
              )
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Dataset Overview
  output$data_summary <- renderDataTable({
    datatable(data, options = list(pageLength = 10))
  })
  
  # KPIs
  output$total_sessions <- renderValueBox({
    valueBox(
      value = formatC(nrow(data), format = "d", big.mark = ","),
      subtitle = "Total Sessions",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$total_revenue <- renderValueBox({
    valueBox(
      value = formatC(sum(data$Revenue), format = "d", big.mark = ","),
      subtitle = "Total Revenue Generating Sessions",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$avg_time_on_site <- renderValueBox({
    valueBox(
      value = round(mean(data$Time_on_Site), 2),
      subtitle = "Average Time on Site (seconds)",
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  # Scatter Plot: Product Related Duration vs Page Value
  output$scatter_plot <- renderPlotly({
    plot_ly(data = data, x = ~ProductRelated_Duration, y = ~PageValues, type = 'scatter', mode = 'markers') %>%
      layout(title = 'Product Related Duration vs Page Value')
  })
  
  # Bar Plot: Revenue Comparison Across Visitor Types
  output$bar_plot <- renderPlotly({
    p <- ggplot(data, aes(x = VisitorType, fill = as.factor(Revenue))) +
      geom_bar(position = "dodge") +
      labs(title = "Revenue Comparison Across Visitor Types", x = "Visitor Type", fill = "Revenue") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Box Plot: Time Spent on Site by Region and Revenue
  output$box_plot <- renderPlotly({
    p <- ggplot(data, aes(x = as.factor(Region), y = Time_on_Site, fill = as.factor(Revenue))) +
      geom_boxplot() +
      labs(title = "Time Spent on Site by Region and Revenue", x = "Region", y = "Time on Site", fill = "Revenue") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Pie Chart: Traffic Source Distribution
  output$pie_chart <- renderPlotly({
    traffic_summary <- data %>% group_by(TrafficType) %>% summarise(count = n())
    plot_ly(traffic_summary, labels = ~TrafficType, values = ~count, type = 'pie') %>%
      layout(title = 'Traffic Source Distribution')
  })
  
  # Violin Plot: Distribution of Time_on_Site Across Revenue
  output$violin_plot <- renderPlotly({
    p <- ggplot(data, aes(x = as.factor(Revenue), y = Time_on_Site, fill = as.factor(Revenue))) +
      geom_violin() +
      labs(title = "Violin Plot: Distribution of Time_on_Site Across Revenue", x = "Revenue", y = "Time on Site") +
      theme_minimal()
    ggplotly(p)
  })
  
  
  output$monthly_conversion_plot <- renderPlotly({
    monthly_conversion <- data %>%
      group_by(Month, TrafficType) %>%
      summarise(Revenue_Sessions = sum(Revenue))
    
    p <- ggplot(monthly_conversion, aes(x = as.factor(Month), y = Revenue_Sessions, color = as.factor(TrafficType), group = TrafficType)) +
      geom_line(size = 1) +
      geom_point() +
      labs(title = "Monthly Conversion Trends by Traffic Type", x = "Month", y = "Revenue Sessions", color = "TrafficType") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p)
  })
  
  
  # Monthly Revenue Trends by Revenue Status
  output$monthly_revenue_plot <- renderPlotly({
    p <- ggplot(data, aes(x = as.factor(Month), fill = as.factor(Revenue))) +
      geom_bar(position = "dodge") +
      labs(title = "Monthly Revenue Trends by Revenue Status", x = "Month", y = "Count", fill = "Revenue") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # Revenue Generation Across Regions
  output$region_revenue_plot <- renderPlotly({
    p <- ggplot(data, aes(x = as.factor(Region), fill = as.factor(Revenue))) +
      geom_bar(position = "dodge") +
      labs(title = "Revenue Generation Across Regions", x = "Region", y = "Count of Users", fill = "Revenue") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Bubble Chart: Bounce Rates vs Exit Rates by Revenue
  output$bubble_chart <- renderPlotly({
    p <- ggplot(data, aes(x = BounceRates, y = ExitRates, size = PageValues, color = as.factor(Revenue))) +
      geom_point(alpha = 0.6) +
      labs(title = "Bounce Rates vs Exit Rates by Revenue", x = "Bounce Rates", y = "Exit Rates") +
      theme_minimal()
    ggplotly(p)
  })
  
  # New facet grid plot for Revenue Patterns Across Visitor Type and Weekend Behavior
  output$facet_grid_plot <- renderPlotly({
    p <- ggplot(data, aes(x = as.factor(Revenue), fill = as.factor(Revenue))) +
      geom_bar() +
      facet_grid(VisitorType ~ Weekend) +
      labs(title = "Revenue Patterns Across Visitor Type and Weekend Behavior", x = "Revenue", y = "Count", fill = "Revenue") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p)
  })
  
  # New Plot 6: Cumulative Revenue by Visitor Type (Line Plot)
  output$cumulative_revenue_plot <- renderPlotly({
    cumulative_revenue <- data %>%
      arrange(Month) %>%
      group_by(Month, VisitorType) %>%
      mutate(Cumulative_Revenue = cumsum(Revenue))
    
    p <- ggplot(cumulative_revenue, aes(x = as.factor(Month), y = Cumulative_Revenue, color = VisitorType, group = VisitorType)) +
      geom_line(size = 1) +
      geom_point() +
      labs(title = "Cumulative Revenue by Visitor Type Across Months", x = "Month", y = "Cumulative Revenue", color = "VisitorType") +
      theme_minimal()
    ggplotly(p)
  })
  
  # New Lollipop chart for Traffic Type
  output$lollipop_chart <- renderPlotly({
    traffic_data <- data %>%
      group_by(TrafficType) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    p <- ggplot(traffic_data, aes(x = TrafficType, y = count)) +
      geom_point(size = 4) +
      geom_segment(aes(x = TrafficType, xend = TrafficType, y = 0, yend = count)) +
      coord_flip() +
      ggtitle("Visualizing Traffic Source Counts Across Types") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p)
  })
  
  output$facet_grid_plot <- renderPlotly({
    p <- ggplot(data, aes(x = as.factor(Revenue), fill = as.factor(Revenue))) +
      geom_bar() +
      facet_grid(VisitorType ~ Weekend) +
      labs(title = "Revenue Patterns Across Visitor Type and Weekend Behavior", x = "Revenue", y = "Count", fill = "Revenue") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p)
  })
  
  output$treemap_plot <- renderPlotly({
    treemap_data <- data %>%
      group_by(TrafficType) %>%
      summarise(Sessions = n(), Revenue_Sessions = sum(Revenue))
    
    p <- plot_ly(
      type = "treemap",
      labels = treemap_data$TrafficType,
      parents = "",  # Empty since no parent categories are being used
      values = treemap_data$Sessions,
      textinfo = "label+value+percent entry",
      marker = list(colorscale = "Blues", color = treemap_data$Revenue_Sessions)
    ) %>%
      layout(title = list(text = "Treemap: Traffic Source Revenue Contributions", x = 0.5))
    
    p
  })
  
  
  
  # Heatmap Plot based on user input
  output$heatmap_plot <- renderPlotly({
    req(input$heatmap_features)
    
    # Select features for heatmap
    selected_features <- input$heatmap_features
    
    # Subset data to selected features
    heatmap_data <- data %>% select(all_of(selected_features))
    
    # Calculate correlation matrix
    corr_matrix <- cor(heatmap_data, use = "complete.obs")
    
    # Melt correlation matrix for heatmap
    melted_corr <- melt(corr_matrix)
    
    # Create heatmap
    p <- ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
      theme_minimal() +
      labs(title = "Correlation Heatmap", x = "Feature", y = "Feature") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
