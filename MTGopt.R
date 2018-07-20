# MTGopt.R

draft <- read.csv("m19_draft_180719_fr.csv", stringsAsFactors = FALSE)
num.colors <- 3
num.non.land <- 23

Mtg.colors <- c("B", "G", "R", "U", "W")

deck.colors <- t(combn(Mtg.colors, num.colors))
deck.color.ind <- t(combn(1:5, num.colors))
objective.values <- data.frame(deck.colors, obj.val = rep(0, nrow(deck.colors)))
# subset mat
for(i in 1:nrow(objective.values)){
  fit.deck <- optimize.colors(i = i, draft = draft,
                              deck.color.ind = deck.color.ind,
                              num.non.land = num.non.land)
  objective.values[i, "obj.val"] <- fit.deck$fit$objval
}
fit.deck <- optimize.colors(i = which.max(objective.values$obj.val), draft = draft,
                       deck.color.ind = deck.color.ind)
fit.deck$fit
deck <- fit.deck$deck

hist(deck$cmc)
head(deck[order(deck$score),])

#################
#################
# Rd 2
# 2 color | 3 - 1
# 3 color | 2 - 1

#################
#################
# Rd 3
# 2 color | 1 - 2
# 3 color | 1 - 2

###########################
# Competitive
# m19_draft_180713_comp
#################
#################
# Rd 1
# 2 color  | 5 - 2
# 3 color  | 1 - 1
# 2col_RU  | 1 - 1
# 3col_BRU | 0 - 1
#################

###########################
# Competitive
# m19_draft_180714_comp
#################
#################
# Rd 1
# 2 color  | 0 - 0
# 3 color  | 0 - 0
# 2col_RU  | 0 - 0
# 3col_BRU | 0 - 0

# Think Zombies!
#################
