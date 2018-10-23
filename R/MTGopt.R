# MTGopt.R

source("MTG/MTGopt_funcs.R")
source("MTG/read_utils.R")

draft <- parse_decklist("sample_grn_decklist.txt")

num.colors <- 4
num.non.land <- 23

Mtg.colors <- c("B", "G", "R", "U", "W")
deck.colors <- t(combn(Mtg.colors, num.colors))
deck.color.ind <- t(combn(1:5, num.colors))
objective.values <- data.frame(deck.colors, obj.val = rep(0, nrow(deck.colors)))

for(i in 1:nrow(objective.values)){
  fit.deck <- optimize.colors(i = i, draft = draft,
                              deck.color.ind = deck.color.ind,
                              num.non.land = num.non.land,
                              tribes = NULL,
                              tribal.boost = c(5, rep(0, length(tribes))))
  objective.values[i, "obj.val"] <- fit.deck$fit$objval
}
fit.deck <- optimize.colors(i = which.max(objective.values$obj.val), draft = draft,
                       deck.color.ind = deck.color.ind, tribes = NULL)

fit <- optimize.draft(draft = draft,
               num.colors = num.colors,
               num.non.land = 40 - 17,
               curve.penalty = 2.5,
               removal.penalty = 0,
               deck.choice = "Best",
               tribes = NULL)

fit$fit.deck$deck
