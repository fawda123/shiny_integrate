library(shiny)
 
# Define UI
shinyUI(pageWithSidebar(
 
  # Application title
  headerPanel("Column-based integration"),
 
  # Sidebar user input
  sidebarPanel(
  
    textInput("infun", "Enter function:", value = "dnorm(x)"),    
    numericInput("poly", "Number of columns:", min=1, max=10000, value=20),
    numericInput("fromx", "Starting x:", value=-1.96),
    numericInput("tox", "Ending x:", value=1.96),
    checkboxInput("intcum", "Show cumulative integration", FALSE),
    submitButton("Submit")
 
  ),
 
  # Show the plots
  mainPanel(
    plotOutput("intPlot")
  )
))