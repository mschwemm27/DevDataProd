# ui.R

# User interface for the shiny app

library(shiny)
require(rCharts)
shinyUI(fluidPage(
  headerPanel(strong("")),
  
  sidebarPanel(
    
    # User input
    uiOutput("choices"),
    uiOutput("htext1"),
    
    ######### Option 1 #########
    uiOutput("statchoice"),
    uiOutput("htext2"),
    uiOutput("slider"),
    uiOutput("htext3"),
    uiOutput("tid"),
    
    ######### Option 2 #########
    uiOutput("salchoicex"),
    uiOutput("salchoicey"),
    uiOutput("leg"),
    uiOutput("htext4"),
    uiOutput("lines"),
    
    ## Was getting some odd error messages that did not interfere with functionality of 
    ## the app.  This hides error messages from showing up in the app.
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }")
    
  ),
  
  
  mainPanel(
    div(h2(strong("Exploratory Analysis of the Lahman Baseball Data Set")),
        style="margin:-20px 0px 10px 20px;"),
    uiOutput('doctext'),
    showOutput("myChart","polycharts"),
    plotOutput('myplot')
  )
  
  
))