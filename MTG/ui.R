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
  titlePanel("MTG Sealed Draft Tool"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(2,
      wellPanel(
        HTML("<strong>INSTRUCTIONS</strong><br/>"),
        downloadLink("downloadData", "1. Download the cardlist"),
        HTML("<br/>2. Fill in the number of cards you drafted in the 'quantity' column and save."),
        fileInput("draft", "3. Upload completed csv here:"),
        HTML("4. Play with sliders to create the best deck you can!"),
        sliderInput("num.colors",
                    "Number of colors:",
                    min = 1,
                    max = 5,
                    value = 2,
                    step = 1),
        uiOutput("deck.choice"),
        sliderInput("num.land",
                    "Number of Lands:",
                    min = 10,
                    max = 30,
                    value = 17),
        sliderInput("curve.penalty",
                    "Mana curve penalty:",
                    min = 0,
                    max = 10,
                    value = 2.5,
                    step = 0.5) #,
        # sliderInput("removal.penalty",
        #             "Removal penalty:",
        #             min = 0,
        #             max = 10,
        #             value = 2.5,
        #             step = 0.5),
        # sliderInput("Artifact.boost", "Artifact Boost:", min = -5, max = 5,
        #             value = 0, step = .1),
        # sliderInput("Ramp.boost", "Ramp Boost:", min = -5, max = 5,
        #             value = 0, step = .1),
        # sliderInput("Goblin.boost", "Goblin Boost:", min = -5, max = 5,
        #             value = 0, step = .1),
        # sliderInput("Dragon.boost", "Dragon Boost:", min = -5, max = 5,
        #             value = 0, step = .1),
        # sliderInput("Lifelink.boost", "Lifelink Boost:", min = -5, max = 5,
        #             value = 0, step = .1),
        # sliderInput("Weenie.boost", "Weenie Boost:", min = -5, max = 5,
        #             value = 0, step = .1),
        # sliderInput("Zombie.boost", "Zombie Boost:", min = -5, max = 5,
        #             value = 0, step = .1)
        )
      ),
    
    # Show a plot of the generated distribution
    column(6,
           HTML("<strong>YOUR DECK</strong>"),
           dataTableOutput("decklist"),
           HTML("<strong>UNUSED CARDS</strong>"),
           dataTableOutput("unused.cards")
    ),
    # Show a plot of the generated distribution
    column(4,
           HTML("<strong>DECK RANKINGS </strong>"),
           tableOutput("objective.values"),
           plotOutput("distPlot"),
           HTML("<strong>DECK STATS</strong>"),
           tableOutput("deckstats")#,
           # HTML("<strong>DECK TRIBES</strong>"),
           # tableOutput("tribal.table")
           )
    )
))
