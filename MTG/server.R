#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("~/MTGcsv/MTGopt_funcs.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    fit.deck <- reactive({
      optimize.draft(draft = read.csv("~/MTGcsv/m19_draft_180715_comp.csv",
                                      stringsAsFactors = FALSE),
                     num.colors = input$num.colors,
                     num.non.land = 40 - input$num.land)
    })
    output$decklist <- renderDataTable({
      fit.deck <- fit.deck()
      show.deck <- fit.deck$deck[, -c(2, 7:11)]
      row.names(show.deck) <- NULL
      colnames(show.deck)[c(2, 6, 7)] <- c("colID", "num", "num.used")
      show.deck
    })
    output$distPlot <- renderPlot({
      fit.deck <- fit.deck()
      hist(fit.deck$deck$cmc, main = "Mana Curve", xlab = "cmc",
           col = 'darkgray', border = 'white')
  })
})
