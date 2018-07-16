# MTGopt.R

draft <- read.csv("m19_draft_180715_comp.csv", stringsAsFactors = FALSE)
install.packages("lpSolve")
library("lpSolve")

num.colors <- 3
num.non.land <- 23

Mtg.colors <- c("B", "G", "R", "U", "W")

deck.colors <- t(combn(Mtg.colors, num.colors))
deck.color.ind <- t(combn(1:5, num.colors))
objective.values <- data.frame(deck.colors, obj.val = rep(0, nrow(deck.colors)))
# subset mat
for(i in 1:nrow(objective.values)){
  fit <- optimize.colors(i = i, draft = draft,
                         deck.color.ind = deck.color.ind)
  objective.values[i, "obj.val"] <- fit$objval
}
fit <- optimize.colors(i = which.max(objective.values$obj.val), draft = draft,
                       deck.color.ind = deck.color.ind)
fit.deck$fit
deck <- fit.deck$deck

hist(deck$cmc)
head(deck[order(deck$score),])

#########################################################
# Optimize 2-color deck
objective.values_2col <- rep(0, 10)
# subset mat
obj.ind <- 0
for(i in 1:4){
  for(j in (i + 1):5){
    obj.ind <- obj.ind + 1
    colors.ind <- which(rowSums(draft[, c("B", "G", "R", "U", "W")[-c(i, j)]]) == 0)
    colors.mat <- draft[colors.ind, ]
    
    # We are trying to maximize the collective score
    f.obj <- colors.mat$score
    
    # We can only select the cards we have, the constraint will be on the rhs
    f.con <- diag(1, nrow = nrow(colors.mat), ncol = nrow(colors.mat))
    # We need to have 23 non-land cards
    non.land.vec <- rep(1, nrow(colors.mat))
    non.land.vec[which(colors.mat$types == "Land")] <- 0
    f.con <- rbind(f.con, non.land.vec)
    
    # The constraint to only pick cards we have
    f.dir <- c(rep("<=", nrow(colors.mat)), "==")
    
    # Using the cards we have for the RHS
    f.rhs <- c(colors.mat$quantity, 23)
    
    all.int <- TRUE
    
    fit <- lp("max", f.obj, f.con, f.dir, f.rhs, all.int = TRUE)
    
    # Quality
    objective.values_2col[obj.ind] <- fit$objval
  }
}
objective.values_2col
i=2;j=5
deck_2col <- cbind(colors.mat[which(fit$solution!=0),], # -c(7:11)],
                   quantity.used = fit$solution[which(fit$solution!=0)])
deck_2col
hist(deck_2col$cmc)
head(deck_2col[order(deck_2col$score),])
######################
######################

# Optimize 3-color deck
objective.values <- rep(0, 10)
# subset mat
obj.ind <- 0
for(i in 1:3){
  for(j in (i + 1):4){
    for(k in (j + 1):5){
      obj.ind <- obj.ind + 1
      colors.ind <- which(rowSums(draft[, c("B", "G", "R", "U", "W")[-c(i, j, k)]]) == 0)
      colors.mat <- draft[colors.ind, ]
      
      # We are trying to maximize the collective score
      f.obj <- colors.mat$score
      
      # We can only select the cards we have, the constraint will be on the rhs
      f.con <- diag(1, nrow = nrow(colors.mat), ncol = nrow(colors.mat))
      # We need to have 23 non-land cards
      non.land.vec <- rep(1, nrow(colors.mat))
      non.land.vec[which(colors.mat$types == "Land")] <- 0
      f.con <- rbind(f.con, non.land.vec)
      
      # The constraint to only pick cards we have
      f.dir <- c(rep("<=", nrow(colors.mat)), "==")
      
      # Using the cards we have for the RHS
      f.rhs <- c(colors.mat$quantity, 23)
      
      fit <- lp("max", f.obj, f.con, f.dir, f.rhs, all.int = TRUE)
      
      # Quality
      objective.values[obj.ind] <- fit$objval
    }
  }
}

objective.values
i=1;j=3;k=4

deck_3col <- cbind(colors.mat[which(fit$solution!=0), -c(7:11)],
              quantity.used = fit$solution[which(fit$solution!=0)])
deck_3col
hist(deck_3col$cmc)
head(deck_3col[order(deck_3col$score),])
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
