library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  

  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      #Select column name
      varSelectInput(
        inputId = "Explanatorycolumn" ,
        label = "Explanatory Column name",
        data = tableOutput("contents"),
        selected = NULL,
        multiple = TRUE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      #Select column name
      varSelectInput(
        inputId = "Responsecolumn" ,
        label = "Response Column name",
        data = tableOutput("contents"),
        selected = NULL,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      
      
      # Output: Data file ----
      tableOutput("contents"),
      verbatimTextOutput("model"), 
      textOutput("lms_text"),
      textOutput("lms_text2"),
      verbatimTextOutput("lms_model"),
      uiOutput("regPlot"),
      uiOutput("residPlot")
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {

 
  lm_model <- reactive({
    req(input$Responsecolumn, input$Explanatorycolumn, df())
    formula <- as.formula(paste(input$Responsecolumn, "~", paste(input$Explanatorycolumn, collapse = "+")))
    lm(formula, data = df())
  })
  
  lms_model <- reactive({
    req(input$Responsecolumn, input$Explanatorycolumn, df())
    
    fits <- function(coeffs) {
      X <- as.matrix(df()[, as.character(input$Explanatorycolumn), drop = FALSE])
      X <- cbind(1, X)
      X %*% coeffs
    }
    residuals_fn <- function(coeffs) {
      df()[, as.character(input$Responsecolumn)] - fits(coeffs)
    }
    rss <- function(coeffs) median(residuals_fn(coeffs)^2)
    
    starting_points <- seq(-2, 4, by = 0.5)
    results <- lapply(starting_points, function(start)
      optim(par = rep(start, length(input$Explanatorycolumn) + 1), fn = rss, method = "BFGS"))
    
    objective_values <- sapply(results, function(r) r$value)
    results[[which.min(objective_values)]]
  }) 
  
  output$model <- renderPrint({ print(lm_model()) })
  output$lms_model <- renderPrint({ print(lms_model()) })
  
  
    # output$lms_text <- renderText({ "Least Median Square Regression explanation" })
    # output$lms_text2 <- renderText({ "Median square regression is a robust statistical method 
    # used to find a line of best fit. Unlike standard linear regression that minimizes the sum 
    # of squared errors, LMS minimizes the median of the squared errors, making it highly resistant 
    # to extreme outliers" })
    # best_result <- results[[which.min(objective_values)]]
    # names(best_result$par) <- c("(Intercept)", input$Explanatorycolumn)
    # cat("Coefficients:\n")
    # print(best_result$par)
    

  df <- reactiveVal()
  observe({
    req(input$file1)
    df.temp <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    df(df.temp) 
  })
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(df())
    
    if (input$disp == "head") {
      return(head(df()))
    } else {
      return(df())
    }
  })
  
  observe({
    x <- colnames(df())
    if (is.null(x)) x <- character(0)
    
    updateSelectizeInput(session, "Responsecolumn",
                         label = "Select Response Variable",
                         choices = x,
                         selected = character(0),
                         options = list(placeholder = "Choose a response variable")
    )
    
    updateSelectizeInput(session, "Explanatorycolumn",
                         label = "Select Explantory Variables",
                         choices = x,
                         selected = character(0),
                         options = list(placeholder = "Choose explanatory variable(s)")
    
    )
  })
  
  output$regPlot <- renderUI({
    req(lm_model(), lms_model())
    
    x_cols <- as.character(input$Explanatorycolumn)
    
    if (length(x_cols) == 1) {
      plotOutput("actualPlot")
    } else {
      h4("Please choose 1 explanatory variable to see a visualisation.")
    }
  })
  
  output$actualPlot <- renderPlot({
    req(length(input$Explanatorycolumn) == 1)
    
    y_col <- as.character(input$Responsecolumn)
    x_cols <- as.character(input$Explanatorycolumn)
    
    ols_coef <- coef(lm_model())
    lms_coef <- lms_model()$par
    
    plot_df <- df()
    ggplot(plot_df, aes(x = .data[[x_cols]], y = .data[[y_col]])) +
      geom_point(color = "gray40") +
      geom_abline(intercept = ols_coef[1], slope = ols_coef[2],
                  color = "blue", linewidth = 1) +
      geom_abline(intercept = lms_coef[1], slope = lms_coef[2],
                  color = "red", linewidth = 1) +
      labs(title = "OLS (blue) vs LMS (red)", x = x_cols, y = y_col) +
      theme_minimal()
  })
  
  output$residPlot <- renderUI({
    req(lm_model(), lms_model())
    
    x_cols <- as.character(input$Explanatorycolumn)
    
    if (length(x_cols) == 1) {
      plotOutput("residualsPlot")
    } else {
      h4("Please choose 1 explanatory variable to see a visualisation.")
    }
  })
  
  output$residPlot <- renderUI({
    req(lm_model(), lms_model())
    plotOutput("residualsPlot")
  })
  
  output$residualsPlot <- renderPlot({
    req(lm_model(), lms_model())
    
    x_cols <- as.character(input$Explanatorycolumn)
    y_col <- as.character(input$Responsecolumn)
    
    ols_coef <- coef(lm_model())
    lms_coef <- lms_model()$par
    
    X <- as.matrix(cbind(1, df()[, x_cols, drop = FALSE]))
    actual <- df()[[y_col]]
    
    ols_fitted <- as.vector(X %*% ols_coef)
    lms_fitted <- as.vector(X %*% lms_coef)
    
    ols_resid <- actual - ols_fitted
    lms_resid <- actual - lms_fitted
    
    plot_df <- data.frame(
      fitted = c(ols_fitted, lms_fitted),
      residual = c(ols_resid, lms_resid),
      model = rep(c("OLS", "LMS"), each = length(actual))
    )
    
    ggplot(plot_df, aes(x = fitted, y = residual, color = model)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = c("OLS" = "blue", "LMS" = "red")) +
      labs(title = "Residuals vs Fitted (OLS vs LMS)",
           x = "Fitted values", y = "Residual") +
      theme_minimal()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
