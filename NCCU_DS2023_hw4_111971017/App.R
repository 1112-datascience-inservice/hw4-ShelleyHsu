library(shiny)
library(ggplot2)
library(ggbiplot)
library(FactoMineR)
library(factoextra)
library(shinydashboard)

data(iris)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("     
    /* Logo and Navbar styles */
    .skin-blue .main-header .navbar {
      background-color: black;
    }
    .skin-blue .main-header .navbar .nav>li>a,
    .skin-blue .main-header .logo,
    .skin-blue .main-header .logo:hover {
      color: white;
      background-color: black;
    }
    
    /* Sidebar styles */
    .skin-blue .wrapper,
    .skin-blue .main-sidebar,
    .skin-blue .left-side,
    .skin-blue .sidebar-menu>li>.treeview-menu {
      background-color: black;
    }
    .skin-blue .sidebar a,
    .skin-blue .sidebar-menu>li.header {
      color: white;
    }
    
    /* Content styles */
    .skin-blue .content,
    .skin-blue .content-header,
    #PCAPlot,
    #PCs,
    #CADims {
      background: black;
      color: white;
    }
    
    /* Box and tabs styles */
    .box.box-solid.box-primary>.box-header {
      color: white;
      background: black;
    }
    .box.box-solid.box-primary {
      border: 1px solid black;
    }
    .nav-tabs-custom>.nav-tabs>li.active {
      border-top-color: black;
    }
    
    /* Other styles */
    .skin-blue .sidebar-menu>li>a {
      border-left: 0;
    }
    .main-sidebar, .skin-blue .main-sidebar, .content-wrapper, .main-footer, .skin-blue .wrapper {
      background-color: black;
    }
    
    .content, .content-header, .sidebar-menu>li.header {
      background-color: black;
      color: white;
    }
    
    .sidebar-menu>li>a {
      color: white;
    }
    
    h2 {
      background-color: black;
      color: white;
    }
    
    .box.box-solid.box-primary {
      border: 1px solid black;
    }
    
    .box.box-solid.box-primary>.box-header {
      color: white;
      background: black;
      background-color: black;
    }
    
    .nav-tabs-custom>.nav-tabs>li.active {
      border-top-color: black;
    }
    
    #PCAPlot, #PCs, #CADims, #cos2Plot {
      background-color: black;
      color: white;
    }
    
    .col-sm-4 .well {
      background-color: black;
      color: white;
    }

"))
  ),
  
  ui <- dashboardPage(
    dashboardHeader(title = tags$span("資碩一 111971017 許瑋如", 
                                      style = "font-size: 16px;")),
    dashboardSidebar(
      sidebarMenu(
        menuItem("iris Data", tabName = "irisData", icon = icon("dashboard")),
        menuItem("PCA", tabName = "pca", icon = icon("th")),
        menuItem("PCA_Cos2", tabName = "pcaCos2", icon = icon("th")),
        menuItem("CA", tabName = "ca", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
        # First tab
        tabItem(tabName = "irisData",
                h2("Plot"),
                fluidRow(
                  column(4,
                         checkboxGroupInput("iris_variables", "Select Principal Components.",
                                            choices=colnames(iris[, 1:4]),
                                            selected=c("Sepal.Length", "Sepal.Width"))
                  ),
                  column(8,
                         plotOutput("irisPlot")
                  )
                )
        ),
        
        # Second tab
        tabItem(tabName = "pca",
                h2("Plot"),
                sidebarLayout(
                  sidebarPanel(
                    checkboxGroupInput("PCs", "選擇主成分 (限2)：", 
                                       choices=c("PC1", "PC2", "PC3", "PC4"),
                                       selected=c("PC1", "PC2"),
                                       inline=TRUE
                    ),
                    sliderInput("n", "請選擇輸入的資料量:", min = 10, max = 150, value = 30)
                  ),
                  mainPanel(
                    plotOutput("pca_plot")
                  )
                )
        ),
        
        # Third tab
        tabItem(tabName = "pcaCos2",
                h2("Cos2 Plot"),
                sidebarLayout(
                  sidebarPanel(
                    checkboxGroupInput("PCs2", "選擇主成分：", 
                                       choices=c("PC1", "PC2", "PC3", "PC4"),
                                       selected=c("PC1", "PC2"),
                                       inline=TRUE
                    )
                  ),
                  mainPanel(
                    plotOutput("cos2Plot")
                  )
                )
        ),
        
        # Fourth tab
        tabItem(tabName = "ca",
                h2("Plot"),
                sidebarLayout(
                  sidebarPanel(
                    checkboxGroupInput("CADims", "選擇維度：", 
                                       choices = c("Dim1", "Dim2", "Dim3"),
                                       selected = c("Dim1", "Dim2"),
                                       inline = TRUE
                    ),
                    sliderInput("n_ca", "請選擇輸入的資料量:", min = 10, max = 150, value = 30)
                  ),
                  mainPanel(
                    plotOutput("ca_plot")  # 修改這裡的輸出ID
                  )
                )
        )
      )
    )
  )
)

server <- function(input, output) {
  # iris data Plot
  output$irisPlot <- renderPlot({
    req(input$iris_variables)
    if (length(input$iris_variables) < 2) {
      return(NULL)
    }
    
    data(iris)
    x_var <- input$iris_variables[1]
    y_var <- input$iris_variables[2]
    
    ggplot(iris, aes_string(x=x_var, y=y_var, color="Species")) +
      geom_point() +
      labs(title=paste("Iris Dataset", x_var, "vs", y_var), x=x_var, y=y_var) +
      theme_minimal() +
      theme(plot.background=element_rect(fill="black"),
            panel.background=element_rect(fill="black"),
            text=element_text(color="white"))
  })
  
  # PCA
  iris.pca <- prcomp(iris[, 1:4], center=TRUE, scale.=TRUE)
  
  # PCA Plot
  output$pca_plot <- renderPlot({
    req(input$PCs)
    req(input$n)
    if (length(input$PCs) < 2) {
      return(NULL)
    }
    
    data_subset <- iris[1:input$n, 1:4]
    iris.pca_subset <- prcomp(data_subset, center=TRUE, scale.=TRUE)
    
    selected_PCs <- as.integer(gsub("PC", "", input$PCs))
    ggbiplot(iris.pca_subset, choices=selected_PCs, obs.scale=1, var.scale=1,
             groups=iris$Species[1:input$n], circle=TRUE, ellipse=TRUE) +
      scale_color_manual(values = c("setosa" = "#FC4E07", "versicolor" = "#E7B800", "virginica" = "#00AFBB")) +
      theme_minimal() +
      theme(plot.background=element_rect(fill="black"),
            panel.background=element_rect(fill="black"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            text=element_text(color="white"))
  })
  
  # PCA_Cos2 Plot
  output$cos2Plot <- renderPlot({
    req(input$PCs2)
    
    selected_PCs <- as.integer(gsub("PC", "", input$PCs2))
    cos2_plot <- fviz_cos2(iris.pca, choice = "var", axes = selected_PCs) +
      theme(
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        axis.text.x = element_text(color = "white", size = 18),
        axis.text.y = element_text(color = "white"),
        text = element_text(color = "white", size = 18)
      )
    
    print(cos2_plot)  # 輸出 cos2 圖表
  })
  
  # CA
  iris_ca <- CA(iris[, -5])
  
  # CA Plot
  output$ca_plot <- renderPlot({
    req(input$CADims)
    req(input$n_ca)
    if (length(input$CADims) < 2) {
      return(NULL)
    }
    
    data_subset <- iris[1:input$n_ca, -5]
    iris_ca_subset <- CA(data_subset)
    
    selected_dims <- as.integer(gsub("Dim", "", input$CADims))
    fviz_ca_biplot(iris_ca_subset, axes=selected_dims, repel=TRUE, col.row=iris$Species[1:input$n_ca],
                   ggtheme=theme_minimal() +
                     theme(plot.background=element_rect(fill="black"),
                           panel.background=element_rect(fill="black"),
                           text=element_text(color="white")))
  })  
}

shinyApp(ui=ui, server=server)

               