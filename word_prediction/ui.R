library(shiny)
library(shinyjs)

shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    tags$div(
    titlePanel(div(p("Coursera Data Science Capstone - Word Prediction Application", align = "center", style= "color: white"),
                   style = "background-color: #1f2378; width: 100%; height: 40px")),
    
    mainPanel(style = "margin-left:150px",
              p(strong("Instructions:"), align = "center", style = "font-size: 20px; margin-top: 80px"),
              p(strong("1. Enter the first word in the text box below;"), align = "center", style = "font-size: 20px"),
              p(strong("2. Wait a few seconds for the dictionaries to load;"), align = "center", style = "font-size: 20px"),
              p(strong("3. Add a new word to the text by just clicking on it."), align = "center", style = "font-size: 20px"),
            
              tags$div(align = "center", 
                       textInput("text", label = "", width = '85%'),
                       tags$style("#text {font-size: 20px}"),
                       uiOutput("wordOneButton", inline = TRUE),
                       uiOutput("wordTwoButton", inline = TRUE),
                       uiOutput("wordThreeButton", inline = TRUE),
                       uiOutput("wordFourButton", inline = TRUE),
                       uiOutput("wordFiveButton", inline = TRUE),
                       
         
                       p("Important information:", align = "center", style = "font-size: 20px; margin-top: 80px"),
                       tags$ul(
                         tags$li("The model does not understand punctuation (write im instead of i'm);", align = "left"), 
                         tags$li("The application does not check for spelling errors;", align = "left"), 
                         tags$li("The application does not recognize capital letters;", align = "left"), 
                         tags$li("If a word is not recognized by the model, the output will be NA. Simply delete the word and try another.", align = "left")
                       ),
                   
                       tags$footer(p("Model built on HC Corpora provided by Swiftkey", style = "margin-top: 20px; color: white", align = "center"), 
                                   style = "position: absoulte; bottom:0; width: 100%; height: 30px; background-color: #1f2378")
                    
              )
    )  
    )

))