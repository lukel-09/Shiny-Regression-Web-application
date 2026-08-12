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
      verbatimTextOutput("lms_model")
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {

 
  #Output column names 
  output$model <- renderPrint({
    req(input$Responsecolumn, input$Explanatorycolumn, df())
    formula <- as.formula(paste(input$Responsecolumn, "~", paste(input$Explanatorycolumn, collapse = "+")))
   model <- lm(formula, data= df())
    print(model)
    })
  
  output$lms_model <- renderPrint({
    req(input$Responsecolumn, input$Explanatorycolumn, df())
    fits <- function(coeffs) {
      X <- df()[, as.character(input$Explanatorycolumn), drop = FALSE]
      X <- as.matrix(X)
      X <- cbind(1, X)
      X %*% coeffs
    }
    residuals <- function(coeffs) {
      df()[, as.character(input$Responsecolumn)] - fits(coeffs)
    }
    
    rss <- function(coeffs) {
      median(residuals(coeffs)^2)
    }
    
    starting_points <- seq(-2, 4, by = 0.5)
    results <- lapply(starting_points, function(start)
      optim(par = rep(start, length(input$Explanatorycolumn) + 1), fn = rss, method = "BFGS"))
    
    objective_values <- sapply(results, function(r) r$value)
    best_result <- results[[which.min(objective_values)]]
    print(best_result)
  }
  ) 
    

  df <- reactiveVal()
  observe({
    req(input$file1)
    df.temp <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    df(df.temp) # this line updates the df on line 90
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
  
}

# Create Shiny app ----
shinyApp(ui, server)
