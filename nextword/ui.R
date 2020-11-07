#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Next word sugesttion"),


    sidebarLayout(
        sidebarPanel(
                textInput('sentence','Write you sentence','My beloved New York'),
                submitButton('Click for word suggestion')
        ),

        mainPanel( tabsetPanel(type = 'tabs',
                               tabPanel('App',
                                        textOutput('nextword')),
                               tabPanel('Instructions',
                                        p("Write your sentence on the left panel
                                        then click for the word
                                          suggestion."),
                                        p("The app will do a Katz's back off
                                          probability estimation of the next word
                                          based on the last words"),
                                        p('More info about the app can be found
                                          at the link below'),
                                        a('App Github page', 
                                          href='https://github.com/wildsonbbl/NLPwithswiftkey')
                               )
            
        )
            
        )
    )
))
