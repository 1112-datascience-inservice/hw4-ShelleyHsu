library(shiny)
library(FactoMineR)
library(factoextra)
library(ggplot2)

# Define UI for app that performs PCA and CA ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("PCA and CA Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select for analysis type ----
      selectInput(inputId = "analysis_type", label = "Select analysis type:",
                  choices = c("PCA", "CA"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot ----
      plotOutput(outputId = "plot")
      
    )
  )
)

# Define server logic required for PCA and CA ----
server <- function(input, output) {
  
  # Perform PCA or CA based on user input ----
  output$plot <- renderPlot({
    
    # Perform PCA if selected
    if (input$analysis_type == "PCA") {
      result <- PCA(iris[, -5], graph = FALSE)
      factoextra::fviz_pca_ind(result, geom.ind = "point", col.ind = iris$Species)
    }
    
    # Perform CA if selected
    if (input$analysis_type == "CA") {
      result <- CA(iris[, -5], graph = FALSE)
      fviz_ca_row(result, geom.row = "point", col.row = iris$Species)
    }
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
