library(shiny)
source("module.R")
# Define UI for random distribution app ----
ui <- fluidPage(theme = "www/bootstrap.css",
                #==== ⭐️ html head ====
                HTML('<nav class="navbar navbar-inverse">
                     <div class="container-fluid">
                     
                     <div class="navbar-header">
                     <button type="button" class="navbar-toggle collapsed" data-toggle="collapse">
                     <span class="sr-only">Toggle navigation</span>
                     <span class="icon-bar"></span>
                     </button>
                     <a class="navbar-brand" href="#">Student Zero</a>
                     </div>
                     
                     <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-2">
                     <ul class="nav navbar-nav">
                     <li class="active"><a href="https://jixing.netlify.com/">Jixing\' blog <span class="sr-only">(current)</span></a></li>
                     </ul>
                     </div>
                     
                     </div>
                     </nav>'),
  
  # App title ----
  titlePanel("Data Input"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      multiFileInput("data_files")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      DT::dataTableOutput("table")
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  df <- callModule(multiFile, "data_files")
  
  # Generate an HTML table view of the data ----
  output$table <- DT::renderDataTable({
    zero_DT_table(df())
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
