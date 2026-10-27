library(shiny)
library(datasets)
library(ggplot2)

# Load mtcars dataset
data(mtcars)

# Fit linear model for prediction
fit <- lm(mpg ~ cyl + hp + wt + am, data = mtcars)

# Define UI
ui <- fluidPage(
  titlePanel("Motor Trend MPG Predictor & Vehicle Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Documentation & Instructions"),
      p("This application predicts a car's fuel efficiency in Miles Per Gallon (MPG) based on key vehicle attributes from the R 'mtcars' dataset."),
      h5("How to use:"),
      tags$ol(
        tags$li("Select the number of engine Cylinders."),
        tags$li("Choose the Transmission Type (Automatic or Manual)."),
        tags$li("Adjust the slider for Gross Horsepower."),
        tags$li("Adjust the slider for Vehicle Weight (in 1,000 lbs)."),
        tags$li("Click 'Calculate MPG' to compute the prediction.")
      ),
      hr(),
      
      h4("Vehicle Specifications Input"),
      radioButtons("cyl", "Number of Cylinders:",
                   choices = c("4 Cylinders" = 4, "6 Cylinders" = 6, "8 Cylinders" = 8),
                   selected = 4),
      
      selectInput("am", "Transmission Type:",
                  choices = c("Automatic" = 0, "Manual" = 1),
                  selected = 1),
      
      sliderInput("hp", "Gross Horsepower (HP):",
                  min = 50, max = 340, value = 110, step = 5),
      
      sliderInput("wt", "Vehicle Weight (1,000 lbs):",
                  min = 1.5, max = 5.5, value = 2.5, step = 0.1),
      
      submitButton("Calculate MPG")
    ),
    
    mainPanel(
      h3("Prediction Summary"),
      h4("Your Inputs:"),
      verbatimTextOutput("user_summary"),
      
      h4("Predicted Fuel Efficiency:"),
      h2(textOutput("mpg_result"), style = "color: #2c3e50; font-weight: bold;"),
      
      hr(),
      h3("Dataset Context"),
      plotOutput("mpg_chart")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  # Reactive prediction calculation
  predicted_mpg <- reactive({
    newdata <- data.frame(
      cyl = as.numeric(input$cyl),
      hp = as.numeric(input$hp),
      wt = as.numeric(input$wt),
      am = as.numeric(input$am)
    )
    predict(fit, newdata = newdata)
  })
  
  # Render summary of user selection
  output$user_summary <- renderText({
    trans <- ifelse(input$am == 0, "Automatic", "Manual")
    paste0("Cylinders: ", input$cyl, 
           " | Transmission: ", trans, 
           " | Horsepower: ", input$hp, " HP", 
           " | Weight: ", input$wt, ",000 lbs")
  })
  
  # Render prediction output text
  output$mpg_result <- renderText({
    paste(round(predicted_mpg(), 2), "Miles Per Gallon (MPG)")
  })
  
  # Render exploratory plot highlighting input point
  output$mpg_chart <- renderPlot({
    ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
      geom_point(size = 3, alpha = 0.7) +
      geom_point(aes(x = input$wt, y = predicted_mpg()), color = "red", size = 6, shape = 18) +
      labs(
        title = "MPG vs Weight Across Dataset (Red Diamond = Your Prediction)",
        x = "Weight (1,000 lbs)",
        y = "Miles Per Gallon (MPG)",
        color = "Cylinders"
      ) +
      theme_minimal()
  })
}

# Run Application
shinyApp(ui = ui, server = server)
