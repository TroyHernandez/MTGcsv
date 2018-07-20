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
      optimize.draft(draft = read.csv("~/MTGcsv/m19_draft_180719_fr.csv",
                                      stringsAsFactors = FALSE),
                     num.colors = input$num.colors,
                     num.non.land = 40 - input$num.land,
                     curve.penalty = input$curve.penalty,
                     removal.penalty = input$removal.penalty)
    })
    output$decklist <- renderDataTable({
      fit.deck <- fit.deck()$fit.deck
      hidden.cols <- which(colnames(fit.deck$deck) %in% c("rarity", "B", "G", "R", "U", "W"))
      show.deck <- fit.deck$deck[, -hidden.cols]
      row.names(show.deck) <- NULL
      # colnames(show.deck)[c(2, 6, 7)] <- c("colID", "num", "num.used")
      show.deck
    })
    output$deckstats <- renderDataTable({
      deck <- fit.deck()$fit.deck$deck
      table(deck$colorIdentity, deck$types)
    })
    output$distPlot <- renderPlot({
      fit.deck <- fit.deck()$fit.deck
      hist(fit.deck$deck$cmc, main = "Mana Curve", xlab = "cmc",
           col = 'darkgray', border = 'white')
    })
    output$objective.values <- renderDataTable({
      objective.values <- fit.deck()$objective.values
      ov.colors <- apply(objective.values[, -ncol(objective.values)],
                       1, paste0, collapse = "")
      obj.vals <- data.frame(colors = ov.colors, value = objective.values$obj.val)
      obj.vals <- obj.vals[order(obj.vals$value, decreasing = TRUE), ]
      # show.deck <- fit.deck$deck[, -c(2, 7:11)]
      # row.names(show.deck) <- NULL
      # colnames(show.deck)[c(2, 6, 7)] <- c("colID", "num", "num.used")
      # show.deck
    })
})
