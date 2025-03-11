# Install required packages if needed:
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("ggplot2")

library(shiny)
library(shinydashboard)
library(ggplot2)

# Create a list of explanations for each variable
explanations <- list(
  mpg   = "Miles per gallon (mpg) indicates the fuel efficiency of the car. Higher mpg means better fuel economy.",
  cyl   = "Number of cylinders (cyl) reflects the engine size and potential power. More cylinders can imply more power, but may lower fuel efficiency.",
  disp  = "Displacement (disp) measures the engine's volume in cubic inches. Larger displacement usually signals more power.",
  hp    = "Horsepower (hp) represents the engine's power output. Higher horsepower typically means a more powerful engine.",
  drat  = "Rear axle ratio (drat) affects acceleration and fuel efficiency. A higher ratio can mean quicker acceleration.",
  wt    = "Weight (wt) of the car (in 1000 lbs) influences performance and fuel economy; heavier cars often perform slower.",
  qsec  = "Quarter mile time (qsec) measures performance. Lower times indicate better acceleration.",
  vs    = "Engine shape (vs): 0 = V-shaped engine, 1 = straight engine. This relates to the design of the engine.",
  am    = "Transmission type (am): 0 = automatic, 1 = manual. This affects driving experience and performance.",
  gear  = "Number of forward gears (gear) can influence the car's speed range and performance.",
  carb  = "Number of carburetors (carb) relates to the engine's fuel delivery, affecting performance."
)

ui <- dashboardPage(
  dashboardHeader(title = "MTCars Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              # First row: Summary stats & Scatter plot side by side
              fluidRow(
                box(
                  title = "Variable Summary", status = "primary",
                  solidHeader = TRUE, width = 6,
                  # Dropdown for selecting the variable to summarize
                  selectInput("summaryVar", "Select Variable:", 
                              choices = names(mtcars)[sapply(mtcars, is.numeric)],
                              selected = "mpg"),
                  verbatimTextOutput("variableSummary")
                ),
                box(
                  title = "Scatter Plot", status = "primary",
                  solidHeader = TRUE, width = 6,
                  plotOutput("scatterPlot", height = 250)
                )
              ),
              # Second row: Explanation of summary stats
              fluidRow(
                box(
                  title = "Summary Explanation", status = "info",
                  solidHeader = TRUE, width = 12,
                  textOutput("summaryExplanation")
                )
              ),
              # Third row: Controls for scatter plot customization
              fluidRow(
                box(
                  title = "Customize Scatter Plot", status = "warning",
                  solidHeader = TRUE, width = 12,
                  selectInput("xvar", "X-axis Variable", 
                              choices = names(mtcars)[sapply(mtcars, is.numeric)],
                              selected = "wt"),
                  selectInput("yvar", "Y-axis Variable", 
                              choices = names(mtcars)[sapply(mtcars, is.numeric)],
                              selected = "mpg")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # Render summary statistics for the selected variable
  output$variableSummary <- renderPrint({
    varData <- mtcars[[input$summaryVar]]
    summary(varData)
  })
  
  # Render a text explanation for the selected variable
  output$summaryExplanation <- renderText({
    selected <- input$summaryVar
    explanation <- explanations[[selected]]
    if (is.null(explanation)) {
      "No explanation available for this variable."
    } else {
      explanation
    }
  })
  
  # Render a scatter plot based on user-selected variables
  output$scatterPlot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(color = "#2C3E50", size = 3) +
      theme_minimal() +
      labs(title = paste("Scatter Plot:", input$yvar, "vs", input$xvar),
           x = input$xvar, y = input$yvar)
  })
}

shinyApp(ui, server)
