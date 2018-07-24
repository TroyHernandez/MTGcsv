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
  output$deck.choice <- renderUI({
    num.colors = input$num.colors
    Mtg.colors <- c("B", "G", "R", "U", "W")
    deck.colors <- apply(t(combn(Mtg.colors, num.colors)), 1,
                         paste0, collapse = "")
    selectInput("deck.choice",
                "Choose Deck:",
                choices = c("Best", deck.colors),
                selected = "Best")
  })
  fit.deck <- reactive({
      optimize.draft(draft = read.csv("~/MTGcsv/m19_draft_180721_fr.csv",
                                      stringsAsFactors = FALSE),
                     num.colors = input$num.colors,
                     num.non.land = 40 - input$num.land,
                     curve.penalty = input$curve.penalty,
                     removal.penalty = input$removal.penalty,
                     deck.choice = input$deck.choice)
    })
  output$decklist <- renderDataTable({
    fit.deck <- fit.deck()$fit.deck
    hidden.cols <- which(colnames(fit.deck$deck) %in%
                           c("rarity", "B", "G", "R", "U", "W",
                             "Artifact", "Ramp", "Goblin", "Dragon",
                             "Lifelink", "Weenie", "Zombie"))
    show.deck <- fit.deck$deck[, -hidden.cols]
    row.names(show.deck) <- NULL
    # colnames(show.deck)[c(2, 6, 7)] <- c("colID", "num", "num.used")
    show.deck
    })
  output$unused.cards <- renderDataTable({
    fit.deck <- fit.deck()$fit.deck
    hidden.cols <- which(colnames(fit.deck$unused.cards) %in%
                           c("rarity", "B", "G", "R", "U", "W",
                             "Artifact", "Ramp", "Goblin", "Dragon",
                             "Lifelink", "Weenie", "Zombie"))
    show.cards <- fit.deck$unused.cards[, -hidden.cols]
    row.names(show.cards) <- NULL
    # colnames(show.deck)[c(2, 6, 7)] <- c("colID", "num", "num.used")
    show.cards
    })
  output$deckstats <- renderTable({
    deck <- fit.deck()$fit.deck$deck
    mat <- as.matrix(table(deck$colorIdentity, deck$types))
    df <- as.data.frame(matrix(0, ncol = ncol(mat), nrow = nrow(mat)))
    df[1:nrow(df), 1:ncol(df)] <- mat
    colnames(df) <- colnames(mat)
    rownames(df) <- rownames(mat)
    df
    }, rownames = TRUE, digits = 0)
  output$distPlot <- renderPlot({
    fit.deck <- fit.deck()$fit.deck
      hist(fit.deck$deck$cmc, main = "Mana Curve", xlab = "cmc",
           col = 'darkgray', border = 'white')
    })
  output$objective.values <- renderTable({
    objective.values <- fit.deck()$objective.values
    # show.deck <- fit.deck$deck[, -c(2, 7:11)]
    # row.names(show.deck) <- NULL
    # colnames(show.deck)[c(2, 6, 7)] <- c("colID", "num", "num.used")
    # show.deck
    })
})
