#install.packages("shiny")
#install.packages("shinythemes")
#install.packages("randomForest")
#install.packages("e1071")




library(shiny)
library(shinythemes)  # For theme options
library(caret)

# Load the pre-trained RandomForest model and the scaler
rfc <- readRDS("../models/random_forest_model.rds")
preProcess_scale <- readRDS("../models/preprocess_scaler.rds")

# Crop dictionary for mapping predicted crop number to crop name
crop_dict_rev <- c(
  1 = 'Tea', 2 = 'Coffee', 3 = 'Avocado', 4 = 'Macadamia Nuts', 5 = 'French Beans', 
  6 = 'Snow Peas', 7 = 'Passion Fruit', 8 = 'Mango', 9 = 'Pineapple', 10 = 'Flowers (Roses)', 
  11 = 'Cabbage', 12 = 'Sugarcane', 13 = 'Cashew Nuts', 14 = 'Tomatoes', 15 = 'Spinach', 
  16 = 'Carrots', 17 = 'Coconuts', 18 = 'Sisal', 19 = 'Sesame Seeds', 20 = 'Tobacco', 
  21 = 'Chillies', 22 = 'Pyrethrum'
)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Using a nice theme
  titlePanel("🌾 Crop Recommendation System"),
  
  tags$style(HTML("
        body { font-family: 'Arial', sans-serif; background-color: #f5f5f5; }
        h3 { color: #2c3e50; font-weight: bold; }
        .well { background-color: #ffffff; border: 1px solid #dce4ec; }
        .btn { background-color: #3498db; color: white; }
    ")),
  
  sidebarLayout(
    sidebarPanel(
      h4("Enter the Environmental Factors", style = "color: #2980b9; font-weight: bold;"),
      numericInput("N", "Nitrogen (N)", value = 20, min = 0),
      numericInput("P", "Phosphorus (P)", value = 30, min = 0),
      numericInput("K", "Potassium (K)", value = 40, min = 0),
      numericInput("temperature", "Temperature (°C)", value = 40, min = 0, step = 0.1),
      numericInput("humidity", "Humidity (%)", value = 20, min = 0, step = 0.1),
      numericInput("ph", "Soil pH", value = 30, min = 0, step = 0.1),
      numericInput("rainfall", "Rainfall (mm)", value = 50, min = 0),
      actionButton("submit", "Recommend Crop", class = "btn btn-primary")
    ),
    
    mainPanel(
      wellPanel(
        h3("Recommended Crop:"),
        textOutput("crop_result"),
        tags$style("#crop_result { font-size: 24px; color: #27ae60; font-weight: bold; }")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Predictive function to recommend crop
  recommendation <- function(N, P, K, temperature, humidity, ph, rainfall) {
    features <- data.frame(N = N, P = P, K = K, temperature = temperature,
                           humidity = humidity, ph = ph, rainfall = rainfall)
    
    # Scale the input features using the saved scaler
    features_scaled <- predict(preProcess_scale, features)
    
    # Make prediction using the loaded RandomForest model
    prediction <- predict(rfc, features_scaled)
    return(prediction)
  }
  
  # Respond to user input and display the recommended crop
  observeEvent(input$submit, {
    predict_crop <- recommendation(input$N, input$P, input$K, input$temperature, 
                                   input$humidity, input$ph, input$rainfall)
    
    # Translate the numeric prediction to the actual crop name
    crop_name <- crop_dict_rev[as.character(predict_crop)]
    
    output$crop_result <- renderText({
      if (!is.null(crop_name)) {
        paste(crop_name, "is the best crop to be cultivated.")
      } else {
        "Sorry, we are unable to recommend a crop for this environment."
      }
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
