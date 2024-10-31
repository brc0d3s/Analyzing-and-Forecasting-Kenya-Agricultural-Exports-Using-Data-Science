# Load necessary libraries
library(shiny)
library(shinythemes)
library(caret)
library(randomForest)
library(shinyjs)  # For controlling UI visibility

# Load the pre-trained RandomForest model and scaler
rfc <- readRDS("./models/random_forest_model.rds")
preProcess_scale <- readRDS("./models/preprocess_scaler.rds")

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
  useShinyjs(),  # Initialize shinyjs for UI control
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
          numericInput("N", "Nitrogen (N, ppm)", value = 50, min = 0, max = 150),
          numericInput("P", "Phosphorus (P, ppm)", value = 20, min = 0, max = 60),
          numericInput("K", "Potassium (K, ppm)", value = 100, min = 0, max = 200),
          numericInput("temperature", "Temperature (°C)", value = 25, min = 0, max = 50, step = 0.1),
          numericInput("humidity", "Humidity (%)", value = 70, min = 0, max = 100, step = 0.1),
          numericInput("ph", "Soil pH", value = 6.5, min = 4, max = 8, step = 0.1),
          numericInput("rainfall", "Rainfall (mm)", value = 100, min = 0, max = 2000),
          actionButton("submit", "Recommend Crop", class = "btn btn-primary")
        ),
        
        mainPanel(
          wellPanel(
            h3("Recommended Crop:", style = "color: #2980b9;"),
            htmlOutput("crop_result"),
            tags$style("#crop_result { font-size: 24px; color: #27ae60; font-weight: bold; }")
          ),
          wellPanel(
            h4("Crop Insights:", style = "color: #2980b9;"),
            htmlOutput("crop_info"),
            tags$style("#crop_info { font-size: 18px; color: #34495e; font-weight: bold; }")
          ),
          hidden(
            div(
              id = "error_panel",
              wellPanel(
                h4("Error Messages:", style = "color: #e74c3c;"),
                htmlOutput("error_message"),
                tags$style("#error_message { font-size: 18px; color: #e74c3c; font-weight: bold; }")
              )
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Function to validate numeric inputs
  validate_numeric <- function(value, name, min_val, max_val) {
    if (!is.numeric(value) || is.na(value) || value < min_val || value > max_val) {
      return(paste(name, "should be a number between", min_val, "and", max_val, ".<br>"))
    }
    return(NULL)
  }
  
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
    output$crop_result <- renderUI({ "" })
    output$crop_info <- renderUI({ "" })
    output$error_message <- renderUI({ "" })
    
    # Hide the error panel initially
    hide("error_panel")
    
    # Validate inputs and show errors if any values are out of a realistic range
    error_message <- ""
    error_message <- paste0(error_message, validate_numeric(input$N, "Nitrogen", 0, 150))
    error_message <- paste0(error_message, validate_numeric(input$P, "Phosphorus", 0, 60))
    error_message <- paste0(error_message, validate_numeric(input$K, "Potassium", 0, 200))
    error_message <- paste0(error_message, validate_numeric(input$temperature, "Temperature", 0, 50))
    error_message <- paste0(error_message, validate_numeric(input$humidity, "Humidity", 0, 100))
    error_message <- paste0(error_message, validate_numeric(input$ph, "Soil pH", 4, 8))
    error_message <- paste0(error_message, validate_numeric(input$rainfall, "Rainfall", 0, 2000))
    
    if (error_message != "") {
      output$error_message <- renderUI({ HTML(error_message) })
      show("error_panel")
      return(NULL)
    }
    
    tryCatch({
      predict_crop <- recommendation(input$N, input$P, input$K, input$temperature, 
                                     input$humidity, input$ph, input$rainfall)
      
      # Translate prediction to crop name
      crop_name <- crop_dict_rev[as.character(predict_crop)]
      
      output$crop_result <- renderUI({
        if (!is.null(crop_name)) {
          HTML(paste(crop_name, "is the ideal export crop for your environmental and soil conditions."))
        } else {
          HTML("Sorry, we are unable to recommend a crop for this environment.")
        }
      })
      
      # Additional crop info
      output$crop_info <- renderUI({
        if (!is.null(crop_name)) {
          HTML(paste(
            "🌱 Crop Insight: ", crop_name, "<br><br>",
            "💼 Export Potential: ", crop_name, " is in high demand globally, especially in markets like Europe and North America.<br><br>",
            "📈 Growth Benefits: Ideal for areas with moderate to high rainfall and optimal soil nutrients.",
            sep = ""
          ))
        }
      })
      
    }, error = function(e) {
      output$error_message <- renderUI({ HTML("An unexpected error occurred.") })
      show("error_panel")
    })
  })
  
  observeEvent(input$go_to_recommendation, {
    updateTabsetPanel(session, "main_tabs", selected = "Recommendation System")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
