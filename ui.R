
requiredPackages = c("shiny","shinyFiles","dplyr","RODBC","plyr","ragtop","expss","bazar","reshape","reshape2","devtools","rpivotTable")
for (i in 1:length(requiredPackages)) {
  if (! require(requiredPackages[i], character.only = T)) {
    install.packages(requiredPackages[i]) 
  }
  suppressMessages(library(requiredPackages[i], character.only = T))
}

library(dplyr)
library(RODBC)
library(plyr)
library(ragtop)
library(expss)
library(bazar)
library(reshape)
library(reshape2)
library(devtools)
library(rpivotTable)
library(shiny)
library(shinyFiles)

ui<-fluidPage(
  
  titlePanel(tags$a(tags$b("Sample NSPC/Control Volume for new stores"))),
  
  fluidRow(
    column(3, wellPanel(
      
      p(tags$a(tags$b("General information"))),
      tags$h6("STEP 1"),
      numericInput("periodos", label = "How many periods have happened since last UU?", value = 1, min = 1, max = 24),
      
      numericInput("periodo_actual", label = "Which is the current period?", value = 2018013),
      
      numericInput("index", label = "Index", value = 1, min = 1, max = 100)),
      
      wellPanel(
      p(tags$a(tags$b("OBDC INFORMATION"))),
      tags$h6("STEP 2"),
      
      
      tags$b(tags$em(p("Sales Data information"))),
      textInput("Server", label = "Server", value = "Enter text..."),
      textInput("Table", label = "Table", value = "Enter text..."),
      textInput("User", label = "User", value = "Enter text..."),
      textInput("Password", label = "Password", value = "Enter text..."),
      
      tags$b(tags$em(p("SMS History information"))),
      textInput("Server_1", label = "Server", value = "Enter text..."),
      textInput("Table_1", label = "Table", value = "Enter text..."),
      textInput("User_1", label = "User", value = "Enter text..."),
      textInput("Password_1", label = "Password", value = "Enter text...")
      
      
    )),
    
    column(3, wellPanel(
      
      p(tags$a(tags$b("VUE inputs"))),
      
      tags$h6("STEP 3"),
      
      tags$b(tags$em(p("Upload VUE outputs"))),
      
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
                           ".csv"))),
      
      wellPanel(
        
        p(tags$a(tags$b("Output"))),
        tags$h6("STEP 3"),
        actionButton("Calculate","Calculate", class= "b1"),
        tags$head(tags$style(".b1{background-color:orange;} .b1{color: black;}"))  
        
      
    )),
    
    column(4, wellPanel(
      tags$a(tags$b(h4("RESULTADO"))),
      downloadButton('download',"Download the data", class = "b2"),
      tags$head(tags$style(".b2{background-color:lightgreen;} .b2{color: black;}")),  
      tableOutput('tabloide')
    )
    )),
  p("*Please fill all data requested")
)
