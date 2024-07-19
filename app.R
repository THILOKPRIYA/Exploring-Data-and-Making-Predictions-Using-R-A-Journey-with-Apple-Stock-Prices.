library(shiny)

# Load the data
apple_data <- read.csv("AAPL.csv")

# Convert the Date column to Date type
apple_data$Date <- as.Date(apple_data$Date)

# UI
ui <- fluidPage(
  titlePanel("Apple Stock Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("dateRange",
                  "Select Date Range:",
                  min = min(apple_data$Date),
                  max = max(apple_data$Date),
                  value = c(min(apple_data$Date), max(apple_data$Date)),
                  timeFormat = "%Y-%m-%d")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Closing Price Scatter Plot", plotOutput("scatterPlot")),
        tabPanel("Volume Histogram", plotOutput("histogram")),
        tabPanel("Open vs Close Prices Line Plot", plotOutput("linePlot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    apple_data[apple_data$Date >= input$dateRange[1] & apple_data$Date <= input$dateRange[2], ]
  })
  
  output$scatterPlot <- renderPlot({
    data <- filtered_data()
    plot(data$Date, data$Close, 
         main = "Scatter Plot of Closing Prices",
         xlab = "Date", ylab = "Closing Price", 
         pch = 19, col = "darkgreen")
  })
  
  output$histogram <- renderPlot({
    data <- filtered_data()
    hist(data$Volume, 
         main = "Histogram of Trading Volume",
         xlab = "Volume",
         col = "purple", breaks = 30)
  })
  
  output$linePlot <- renderPlot({
    data <- filtered_data()
    plot(data$Date, data$Open, 
         type = "l", col = "black", 
         xlab = "Date", ylab = "Price", 
         main = "Open vs Close Prices Over Time")
    lines(data$Date, data$Close, col = "red")
    legend("topright", legend = c("Open", "Close"), col = c("black", "red"), lty = 1)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
