library(shiny)
library(shinyjs)

shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    titlePanel(div("Coursera Data Science Capstone - Word Prediction Application", align = "center")),
    mainPanel(style = "margin-left:150px",
              p(strong("Instructions"), align = "center", style = "font-size: 20px; margin-top: 80px"),
              p(strong("1. Enter the first word in the text box below."), align = "center", style = "font-size: 20px"),
              p(strong("2. Wait a few seconds for the dictionaries to load."), align = "center", style = "font-size: 20px"),
              p(strong("3. Add a new word to the text by just clicking on it."), align = "center", style = "font-size: 20px"),
              tags$div(align = "center", 
                       textInput("text", label = "", width = '85%'),
                       tags$style("#text {font-size: 20px}"),
                       uiOutput("wordOneButton", inline = TRUE),
                       uiOutput("wordTwoButton", inline = TRUE),
                       uiOutput("wordThreeButton", inline = TRUE),
                       uiOutput("wordFourButton", inline = TRUE),
                       uiOutput("wordFiveButton", inline = TRUE)
              )
    )
    
    
))