library(shiny)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(ggbiplot)
library(DT)

# Define UI
ui <- fluidPage(
  navbarPage(
    "111753122 HW4",
    tabPanel(
      title = "Dataset",
      titlePanel("Iris Dataset"),
      mainPanel(datatable(iris))
    ),
    tabPanel(
      title = "PCA",
      titlePanel(h2("Principal Component Analysis")),
      sidebarLayout(
        sidebarPanel(
          sliderInput("rangePCA", "Range of Input Data", min = 1, max = 150, value = c(1, 150)),
          selectInput("x", "Choose X axis of PCA", 
                      c("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4), selected = 1),
          selectInput("y", "Choose Y axis of PCA",
                      c("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4), selected = 2),
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(h3("PCA"), plotOutput("pcaPlot"), width = "100%"),
            tabPanel(h3("Result"), verbatimTextOutput("pcaResult")),
            tabPanel(h3("Summary"), verbatimTextOutput("pcaSummary"))
          )
        )
      )
    ),
    tabPanel(
      title = "CA",
      titlePanel(h2("Correspondence Analysis")),
      sidebarLayout(
        sidebarPanel(
          sliderInput("rangeCA", "Range of Input Data", min = 1, max = 150, value = c(1, 150)),
          selectInput("Dstart", "Choose Start Dimension of Contribution : ", 
                      c("DIM 1" = 1, "DIM 2" = 2, "DIM 3" = 3), selected = 1),
          selectInput("Dend", "Choose Last Dimension of Contribution : ",
                      c("DIM 1" = 1, "DIM 2" = 2, "DIM 3" = 3), selected = 1)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(h3("CA Biplot"), plotOutput("caPlot"), width = "100%"),
            tabPanel(h3("Summary"), verbatimTextOutput("caSummary")),
            tabPanel(h3("Row Contribution Plot"), plotOutput("caRowContribPlot")),
            tabPanel(h3("Column Contribution Plot"), plotOutput("caColContribPlot")),
            tabPanel(h3("Scree Plot"), plotOutput("caScreePlot"))
          )
        )
      )
    )
  )
)
# Define server
server <- function(input, output) {
  # Data 
  data(iris)
  log.ir <- reactive(log(iris[input$rangePCA[1]:input$rangePCA[2], 1:4]))
  ir.species <- reactive(iris[input$rangePCA[1]:input$rangePCA[2], 5])
  
  # PCA
  ir.pca <- reactive(prcomp(log.ir(), center = TRUE, scale. = TRUE))
  output$pcaPlot <- renderPlot({
    g <- ggbiplot(ir.pca(), choices = c(as.numeric(input$x), as.numeric(input$y)),
                  obs.scale = 1, var.scale = 1, groups = ir.species())
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    print(g)
  })
  output$pcaResult <- renderPrint({ir.pca()})
  output$pcaSummary <- renderPrint(summary(ir.pca()))
  
  
  # CA
  ir.ca <- reactive(CA(iris[input$rangeCA[1]:input$rangeCA[2], 1:4], graph = FALSE))
  output$caPlot <- renderPlot(fviz_ca_biplot(ir.ca(), title = ""))
  output$caSummary <- renderPrint(summary(ir.ca()))
  output$caRowContribPlot <- renderPlot(
    fviz_contrib(ir.ca(), choice = "row", 
                 axes = input$Dstart:input$Dend, 
                 fill = "#9A8E8E", color = "black")
  )
  output$caColContribPlot <- renderPlot(
    fviz_contrib(ir.ca(), choice = "col", 
                 axes = input$Dstart:input$Dend, 
                 fill = "#9A8E8E", color = "black")
  )
  output$caScreePlot <- renderPlot(
    fviz_screeplot(ir.ca(), addlabels = TRUE, 
                   barfill = "#9A8E8E", barcolor = "black", title = "")
    + geom_hline(yintercept = 33.33, linetype = 2, color = "red")
  ) 
}

# Run the app
shinyApp(ui = ui, server = server)
