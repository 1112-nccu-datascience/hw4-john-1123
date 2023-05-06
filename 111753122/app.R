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
      titlePanel("Principal Component Analysis"),
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
            tabPanel("PCA", plotOutput("pcaPlot"), width = "100%"),
            tabPanel("Result", verbatimTextOutput("pcaResult")),
            tabPanel("Summary", plotOutput("pcaSummaryPlot"), verbatimTextOutput("pcaSummary"))
          )
        )
      )
    ),
    tabPanel(
      title = "CA",
      titlePanel("Correspondence Analysis"),
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
            id = "tab-panel",
            tabPanel("CA Biplot", plotOutput("caPlot"), width = "100%"),
            tabPanel("Summary", verbatimTextOutput("caSummary")),
            tabPanel("Row Contribution", plotOutput("caRowPlot"), plotOutput("caRowContribPlot")),
            tabPanel("Column Contribution", plotOutput("caColPlot"), plotOutput("caColContribPlot")),
            tabPanel("Scree", plotOutput("caScreePlot"))
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
                  obs.scale = 1, var.scale = 1, groups = ir.species(),
                  ellipse = TRUE)
    g <- g + scale_color_discrete(name = "")
    g <- g + theme(legend.direction = "horizontal", legend.position = "top")
    print(g)
  })
  output$pcaResult <- renderPrint({ir.pca()})
  output$pcaSummary <- renderPrint(summary(ir.pca()))
  output$pcaSummaryPlot <- renderPlot(
    plot(ir.pca(), type="l")
  )

  # CA
  ir.ca <- reactive(CA(iris[input$rangeCA[1]:input$rangeCA[2], 1:4], graph = FALSE))
  output$caPlot <- renderPlot(fviz_ca_biplot(ir.ca(), title = ""))
  output$caSummary <- renderPrint(summary(ir.ca()))
  output$caRowContribPlot <- renderPlot(
    fviz_contrib(ir.ca(), choice = "row",
                 axes = input$Dstart:input$Dend,
                 fill = "#9A8E8E", color = "black")
  )
  output$caRowPlot <- renderPlot(
    fviz_ca_row(ir.ca(), col.row = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE)
  )
  output$caColContribPlot <- renderPlot(
    fviz_contrib(ir.ca(), choice = "col",
                 axes = input$Dstart:input$Dend,
                 barfill = "#9A8E8E", color = "black")
  )
  output$caColPlot <- renderPlot(
    fviz_ca_col(ir.ca(), col.col = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE)
  )
  output$caScreePlot <- renderPlot(
    fviz_screeplot(ir.ca(), addlabels = TRUE,
                   barfill = "#9A8E8E", barcolor = "black", title = "")
    + geom_hline(yintercept = 33.33, linetype = 2, color = "red")
  )
}

# Run the app
shinyApp(ui = ui, server = server)
