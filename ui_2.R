#install.packages("shiny")
#install.packages("shinyFiles")
library(shiny)
library(shinyFiles)

ui<-fluidPage(
  
  titlePanel("Sample NSPC/Control Volume for new stores"),
  
  fluidRow(
    column(4, wellPanel(
           h4("STEP 1"),
           p("General information"),
           p("Please fill in all data requested"),
           
           numericInput("periodos", label = "How many periods have happened since last UU?", value = 1, min = 1, max = 24),
           
           numericInput("periodo_actual", label = "Which is the current period?", value = 2018013),
           
           
           numericInput("index", label = "Index", value = 1, min = 1, max = 100)
           
           
    )),
    
    column(4, wellPanel(
           h4("STEP 2"),
           p("VUE/Sales inputs"),
           p("Upload VUE outputs and additional information."),
           
           fileInput("Basket", "Choose Basket File",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           
           # Input: Select a file ----
           fileInput("Constant", "Choose Constant File",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           
           # Input: Select a file ----
           fileInput("IBD", "Choose IBD File",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           
           # Input: Select a file ----
           fileInput("Outlier", "Choose Outlier File",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"))
          
    )),
    
    
    
    column(4, wellPanel(
           h4("STEP 3"),
           p("Output"),
           p("Where do you want to receive the output?"),
           
           textInput("adress", label = "Output Address", value = "Enter text..."),
           actionButton("action","Run!")
           
    )
  ))
)


# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$value <- renderPrint({ input$periodos })
  
  output$value <- renderPrint({ input$periodo_actual })
  
  output$value <- renderPrint({ input$index })
  
  output$value <- renderPrint({ input$adress })
  
  #basket <- read.table(paste(ruta,(paste(paste("VUESample_Project",project,sep=""),paste("_Period_",period,"_",sep=""),"Basket.txt",sep="")),sep = "/"), 
  #                   header = T, sep = "\t", quote = "\"", skipNul = TRUE)
  #constant <- read.table(paste(ruta,paste(paste("VUESample_Project",project,sep=""),paste("_Period_",period,"_",sep=""),"Constant.txt",sep=""),sep = "/"), 
  #                       header = T, sep = "\t", quote = "\"", skipNul = TRUE)
  #ibds <- read.table(paste(ruta,paste(paste("VUESample_Project",project,sep=""),paste("_Period_",period,"_",sep=""),"IBDs.txt",sep=""),sep = "/"), 
  #                   header = T, sep = "\t", quote = "\"", skipNul = TRUE)
  #outliers <- read.table(paste(ruta,paste(paste("VUESample_Project",project,sep=""),paste("_Period_",period,"_",sep=""),"NSPC_Outliers.txt",sep=""),sep = "/"), 
  #                       header = T, sep = "\t", quote = "\"", skipNul = TRUE)
  #ventas.z <- read.csv(paste(ruta,"ventas_zero_15.csv",sep = "/"), header = TRUE, sep = ",", quote = "\"", 
  #                     dec = ".", fill = TRUE, comment.char = "") # ventas extraidas directamente de SI
  
  output$value <- renderPrint({
    str(input$Basket)
  })
  
  output$value <- renderPrint({
    str(input$Constant)
  })
  
  output$value <- renderPrint({
    str(input$IBD)
  })
  
  output$value <- renderPrint({
    str(input$Outlier)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
