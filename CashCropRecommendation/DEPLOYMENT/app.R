# Load necessary libraries
library(shiny)
library(shinythemes)  # For theme options
library(caret)  # For pre-processing
library(randomForest)  # For prediction model

# Load the pre-trained RandomForest model and scaler
rfc <- readRDS("random_forest_model.rds")
preProcess_scale <- readRDS("preprocess_scaler.rds")

# Crop dictionary for mapping predicted crop number to crop name
crop_dict_rev <- c(
  '1' = 'Tea', '2' = 'Coffee', '3' = 'Avocado', '4' = 'Macadamia Nuts',
  '5' = 'French Beans', '6' = 'Snow Peas', '7' = 'Passion Fruit',
  '8' = 'Mango', '9' = 'Pineapple', '10' = 'Flowers (Roses)',
  '11' = 'Cabbage', '12' = 'Sugarcane', '13' = 'Cashew Nuts',
  '14' = 'Tomatoes', '15' = 'Spinach', '16' = 'Carrots',
  '17' = 'Coconuts', '18' = 'Sisal', '19' = 'Sesame Seeds',
  '20' = 'Tobacco', '21' = 'Chillies', '22' = 'Pyrethrum'
)

# Define UI with a landing page and recommendation tab
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("🌍 Kenyan Export Crop Advisor"),
  
  tabsetPanel(
    id = "main_tabs",
    
    # Landing Page Tab
    tabPanel(
      title = "Welcome",
      icon = icon("info-circle"),
      h2("Welcome to the Kenyan Export Crop Advisor"),
      p("Discover the ideal export crops for your farm based on soil and climate conditions, optimized specifically for Kenya's diverse environment. Get tailored insights to grow crops with high potential in global markets."),
      br(),
      h4("How It Works:"),
      tags$ul(
        tags$li("Navigate to the 'Recommendation System' tab."),
        tags$li("Enter key environmental and soil factors."),
        tags$li("Click 'Recommend Crop' to receive the best export crop suggestion.")
      ),
      br(),
      div(
        style = "text-align: center; margin-top: 20px;",
        actionButton(
          inputId = "go_to_recommendation", 
          label = "Get Started", 
          class = "btn btn-success",
          style = "font-size: 18px; padding: 10px 20px; background-color: #27ae60; color: white; border: none; border-radius: 5px; cursor: pointer;"
        )
      )
    ),
    
    # Crop Recommendation System Tab
    tabPanel(
      title = "Recommendation System",
      icon = icon("seedling"),
      sidebarLayout(
        sidebarPanel(
          h4("Enter Environmental Factors:", style = "color: #2980b9; font-weight: bold;"),
          numericInput("N", "Nitrogen (N)", value = 20, min = 0),
          numericInput("P", "Phosphorus (P)", value = 30, min = 0),
          numericInput("K", "Potassium (K)", value = 40, min = 0),
          numericInput("temperature", "Temperature (°C)", value = 25, min = 0, step = 0.1),
          numericInput("humidity", "Humidity (%)", value = 70, min = 0, step = 0.1),
          numericInput("ph", "Soil pH", value = 6.5, min = 0, step = 0.1),
          numericInput("rainfall", "Rainfall (mm)", value = 100, min = 0),
          actionButton("submit", "Recommend Crop", class = "btn btn-primary")
        ),
        
        mainPanel(
          wellPanel(
            h3("Recommended Crop:", style = "color: #2980b9;"),
            textOutput("crop_result"),
            tags$style("#crop_result { font-size: 24px; color: #27ae60; font-weight: bold; }")
          ),
          wellPanel(
            h4("Crop Insights:", style = "color: #2980b9;"),
            htmlOutput("crop_info"),
            tags$style("#crop_info { font-size: 18px; color: #34495e; font-weight: bold; }")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
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
  
  # Display the recommended crop
  observeEvent(input$submit, {
    output$crop_result <- renderText("")
    output$crop_info <- renderText("")
    
    tryCatch({
      predict_crop <- recommendation(input$N, input$P, input$K, input$temperature, 
                                     input$humidity, input$ph, input$rainfall)
      
      # Translate prediction to crop name
      crop_name <- crop_dict_rev[as.character(predict_crop)]
      
      output$crop_result <- renderText({
        if (!is.null(crop_name)) {
          paste(crop_name, "is the ideal export crop for your environmental and soil conditions.")
        } else {
          "Sorry, we are unable to recommend a crop for this environment."
        }
      })
      
      # Additional crop info
      output$crop_info <- renderUI({
        if (!is.null(crop_name)) {
          HTML(paste(
            "🌱 Crop Insight: ", crop_name, "<br><br>",
            "💼 Export Potential: ", crop_name, " is in high demand globally, especially in markets like Europe and North America.<br><br>",
            "💡 Growing Tip: Optimize yields by ensuring balanced soil nutrition and keeping track of climate variations.<br><br>",
            "📈 Revenue Potential: Exporting ", crop_name, " can provide consistent income, especially with high-quality or certified varieties."
          ))
        } else {
          "Sorry, we are unable to provide additional information for this crop."
        }
      })
      
    }, error = function(e) {
      output$crop_result <- renderText("An error occurred: Please check your inputs.")
      output$crop_info <- renderText(as.character(e))
    })
  })
  
  # Redirect to Recommendation System tab on button click
  observeEvent(input$go_to_recommendation, {
    updateTabsetPanel(session, "main_tabs", selected = "Recommendation System")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
