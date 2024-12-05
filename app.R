library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(visreg)
library(rsconnect)

# Loading and cleaning data
data_url = 'https://raw.githubusercontent.com/nsbrumble/DS501/refs/heads/main/motodata.csv'
data <- read.csv(data_url, encoding = "UTF-8")


colnames(data) <- make.names(colnames(data))
data <- na.omit(data)
data$year <- as.numeric(data$year)
data$km_driven <- as.numeric(data$km_driven)

# Function to remove outliers using IQR
remove_outliers <- function(data, cols) {
  for (col in cols) {
    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    data <- data[data[[col]] >= lower_bound & data[[col]] <= upper_bound, ]
  }
  return(data)
}

# Apply outlier removal to relevant columns
numerical_cols <- c("selling_price", "km_driven") 
cleaned_data <- remove_outliers(data, numerical_cols)

# Update the cleaned dataset
data <- cleaned_data



# Define UI
ui <- navbarPage(
  "Motorcycle Sales Data",
  
  # Page 1
  tabPanel(
    "Sorted Data",
    sidebarLayout(
      sidebarPanel(
        h4("Sort Options"),
        selectInput(
          inputId = "sort_by",
          label = "Sort data by:",
          choices = list(
            "Mileage (km_driven)" = "km_driven",
            "Selling Price" = "selling_price",
            "Year of Manufacture" = "year"
          ),
          selected = "km_driven"
        ),
        radioButtons(
          inputId = "sort_order",
          label = "Sort order:",
          choices = c("Ascending" = "asc", "Descending" = "desc"),
          selected = "asc"
        ),
        sliderInput(
          inputId = "year_range",
          label = "Select Year Range:",
          min = min(data$year, na.rm = TRUE),
          max = max(data$year, na.rm = TRUE),
          value = c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE))
        ),
        sliderInput(
          inputId = "mileage_range",
          label = "Select Mileage Range (km_driven):",
          min = min(data$km_driven, na.rm = TRUE),
          max = max(data$km_driven, na.rm = TRUE),
          value = c(min(data$km_driven, na.rm = TRUE), max(data$km_driven, na.rm = TRUE))
        )
      ),
      mainPanel(
        h3("Filter and Sort Motorcycle Data"),
        DTOutput("filtered_table")
      )
    )
  ),
  
  # Page 2
  tabPanel(
    "Plot Data",
    sidebarLayout(
      sidebarPanel(
        h4("Plot Options"),
        selectInput(
          inputId = "x_axis",
          label = "Select X-axis:",
          choices = names(data),
          selected = names(data)[3]
        ),
        selectInput(
          inputId = "y_axis",
          label = "Select Y-axis:",
          choices = names(data),
          selected = names(data)[2]
        ),
        h4("Summary Statistics"),
        tableOutput("summary_stats")
      ),
      mainPanel(
        h3("Scatter Plot of Raw Data"),
        plotOutput("data_plot")
      )
    )
  ),
  
  # Page 3: Linear Regression
  tabPanel(
    "Linear Regression",
    sidebarLayout(
      sidebarPanel(
        h4("Regression Options"),
        selectInput(
          inputId = "response",
          label = "Select Response (Y):",
          choices = names(data),
          selected = names(data)[2]
        ),
        checkboxGroupInput(
          inputId = "predictors",
          label = "Select Predictors (X):",
          choices = setdiff(names(data), c("name", "seller_type", "owner", "response","selling_price")),
          selected = setdiff(names(data), c("name", "seller_type", "owner", "response", "selling_price"))[1]
        ),
        uiOutput("predictor_inputs"),  
        actionButton("predict", "Make Prediction"),
        verbatimTextOutput("prediction_result")
      ),
      mainPanel(
        h3("Linear Regression Visualization"),
        plotOutput("regression_plot"),
        verbatimTextOutput("regression_summary")
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Logic for Page 1
  output$filtered_table <- renderDT({
    filtered_data <- data[data$year >= input$year_range[1] & 
                            data$year <= input$year_range[2] & 
                            data$km_driven >= input$mileage_range[1] & 
                            data$km_driven <= input$mileage_range[2], ]
    
    if (nrow(filtered_data) == 0) {
      return(datatable(data.frame(Message = "No data matches the selected criteria")))
    }
    
    sorted_data <- filtered_data[order(filtered_data[[input$sort_by]], 
                                       decreasing = (input$sort_order == "desc")), ]
    datatable(sorted_data, options = list(pageLength = 10))
  })
  
  # Logic for Page 2
  output$data_plot <- renderPlot({
    ggplot(data, aes_string(x = input$x_axis, y = input$y_axis)) +
      geom_point() +
      labs(
        x = input$x_axis,
        y = input$y_axis,
        title = paste("Scatter Plot of", input$y_axis, "vs", input$x_axis)
      ) +
      theme_minimal()
  })
  
  # Summary statistics
  output$summary_stats <- renderTable({
    selected_data <- data[, c(input$x_axis, input$y_axis)]
    summary_data <- data.frame(
      Statistic = c("Min", "Max", "Mean", "Median", "SD"),
      X = c(
        min(selected_data[[input$x_axis]], na.rm = TRUE),
        max(selected_data[[input$x_axis]], na.rm = TRUE),
        mean(selected_data[[input$x_axis]], na.rm = TRUE),
        median(selected_data[[input$x_axis]], na.rm = TRUE),
        sd(selected_data[[input$x_axis]], na.rm = TRUE)
      ),
      Y = c(
        min(selected_data[[input$y_axis]], na.rm = TRUE),
        max(selected_data[[input$y_axis]], na.rm = TRUE),
        mean(selected_data[[input$y_axis]], na.rm = TRUE),
        median(selected_data[[input$y_axis]], na.rm = TRUE),
        sd(selected_data[[input$y_axis]], na.rm = TRUE)
      )
    )
    summary_data
  })
  
  # Dynamically generate numeric input fields for selected predictors
  output$predictor_inputs <- renderUI({
    req(input$predictors)  # Ensure predictors are selected
    lapply(input$predictors, function(predictor) {
      numericInput(
        inputId = paste0("pred_", predictor),
        label = paste("Value for", predictor, ":"),
        value = mean(data[[predictor]], na.rm = TRUE)
      )
    })
  })
  
  # Logic for regression and prediction
  output$regression_plot <- renderPlot({
    req(input$predictors)
    formula <- as.formula(paste(input$response, "~", paste(input$predictors, collapse = " + ")))
    model <- lm(formula, data = data)
    
    visreg(model, xvar = input$predictors[1], gg = TRUE) +
      theme_minimal() +
      labs(title = paste("Effect of", input$predictors[1], "on", input$response))
  })
  
  output$regression_summary <- renderPrint({
    req(input$predictors)
    formula <- as.formula(paste(input$response, "~", paste(input$predictors, collapse = " + ")))
    model <- lm(formula, data = data)
    summary(model)
  })
  
  # Prediction logic
  output$prediction_result <- renderPrint({
    req(input$predict)
    req(input$predictors)
    
    # Build the regression model
    formula <- as.formula(paste(input$response, "~", paste(input$predictors, collapse = " + ")))
    model <- lm(formula, data = data)
    
    # Collect input values for predictors
    new_data <- data.frame(lapply(input$predictors, function(predictor) {
      input[[paste0("pred_", predictor)]]
    }))
    colnames(new_data) <- input$predictors
    
    # Make prediction
    prediction <- predict(model, newdata = new_data)
    paste("Predicted", input$response, ":", round(prediction, 2))
  })
}

# Run the app
shinyApp(ui = ui, server = server)


