library(shiny)
library(ggplot2)
library(factoextra)
library(FactoMineR)

data(iris)

# UI
ui <- navbarPage(
  title = "PCA and CA analysis",
  tabPanel("PCA",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "xcol", label = "X-axis", choices = names(iris)[-5]),
               selectInput(inputId = "ycol", label = "Y-axis", choices = names(iris)[-5])
             ),
             mainPanel(
               plotOutput(outputId = "pca_plot")
             )
           )
  ),
  tabPanel("CA",
           sidebarPanel(
             sliderInput(inputId = "k_value", label = "K-means Centers (k)", min = 1, max = 10, value = 2)
           ),
           mainPanel(
             plotOutput(outputId = "ca_plot")
           )
  )
)

# Server
server <- function(input, output) {
  
  # PCA plot
  output$pca_plot <- renderPlot({
    validate(
      need(input$xcol != input$ycol, "X-axis and Y-axis should not be the same variable.")
    )
    data <- iris[, -5]
    result <- prcomp(data, scale. = TRUE)
    xcol <- input$xcol
    ycol <- input$ycol
    plot <- fviz_pca_ind(result, geom = "point", 
                         axes = c(which(colnames(data) == xcol), which(colnames(data) == ycol)), 
                         col.ind = iris$Species)
    print(plot)
  })
  
  # CA plot
  output$ca_plot <- renderPlot({
    data <- iris[, -5]
    result <- CA(data, graph = FALSE)
    k_value <- input$k_value
    result$kmeans <- k_value
    plot <- factoextra::fviz_ca_row(result, geom = "point", kmeans = result$kmeans)
    print(plot)
  })
}

# Create Shiny app
#shinyApp(ui = ui, server = server)
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
