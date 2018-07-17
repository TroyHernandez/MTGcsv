# MTGopt_funcs.R

library("lpSolve")

optimize.colors <- function(i, draft, deck.color.ind,
                            num.non.land = 23,
                            curve.penalty = 5,
                            Mtg.colors = c("B", "G", "R", "U", "W")){
  remove.colors <- c(i)
  colors.ind <- which(rowSums(draft[, Mtg.colors[-deck.color.ind[i,]]]) == 0)
  colors.mat <- draft[colors.ind, ]
  
  # We are trying to maximize the collective score
  f.obj <- c(colors.mat$score)
  # adding in creature curve
  f.obj <- c(f.obj, rep(-1 * curve.penalty, 12))

  # We can only select the cards we have, the constraint will be on the rhs
  f.con <- diag(1, nrow = nrow(colors.mat), ncol = nrow(colors.mat))
  # We need to have 23 non-land cards
  non.land.vec <- rep(1, nrow(colors.mat))
  non.land.vec[which(colors.mat$types == "Land")] <- 0
  f.con <- rbind(f.con, non.land.vec)
  # Add in creature curve 
  creature.mat <- matrix(0, ncol = nrow(colors.mat), nrow = 6)
  creature.mat[1, which(colors.mat$types == "Creature" & colors.mat$cmc == 1)] <- 1
  creature.mat[2, which(colors.mat$types == "Creature" & colors.mat$cmc == 2)] <- 1
  creature.mat[3, which(colors.mat$types == "Creature" & colors.mat$cmc == 3)] <- 1
  creature.mat[4, which(colors.mat$types == "Creature" & colors.mat$cmc == 4)] <- 1
  creature.mat[5, which(colors.mat$types == "Creature" & colors.mat$cmc == 5)] <- 1
  creature.mat[6, which(colors.mat$types == "Creature" & colors.mat$cmc > 5)] <- 1
  f.con <- rbind(f.con, creature.mat)
  # Add in slack.mat
  creature.slack.mat <- matrix(0, nrow = 6, ncol = 12)
  creature.slack.mat[cbind(1:6, seq(1, 11, 2))] <- 1
  creature.slack.mat[cbind(1:6, seq(2, 12, 2))] <- -1
  f.con <- cbind(f.con,
                 rbind(matrix(0, nrow = nrow(f.con) - nrow(creature.slack.mat),
                              ncol = 12),
                       creature.slack.mat))

  # The constraint to only pick cards we have
  f.dir <- c(rep("<=", nrow(colors.mat)), rep("==", 7))
  
  # Using the cards we have for the RHS
  creature.curve <- c(1, 5, 4, 4, 3, 1)
  f.rhs <- c(colors.mat$quantity, num.non.land, creature.curve)
  
  all.int <- TRUE
  
  fit <- lp("max", f.obj, f.con, f.dir, f.rhs, all.int = TRUE)
  
  deck <- cbind(colors.mat[which(fit$solution[1:nrow(colors.mat)] != 0),],
                quantity.used = fit$solution[which(fit$solution[1:nrow(colors.mat)] != 0)])
  fit.deck <- list(fit = fit, deck = deck)
}

optimize.draft <- function(draft, num.colors, num.non.land, curve.penalty = 5){
  Mtg.colors <- c("B", "G", "R", "U", "W")
  deck.colors <- t(combn(Mtg.colors, num.colors))
  deck.color.ind <- t(combn(1:5, num.colors))
  objective.values <- data.frame(deck.colors, obj.val = rep(0, nrow(deck.colors)))
  # subset mat
  for(i in 1:nrow(objective.values)){
    fit.deck <- optimize.colors(i = i, draft = draft,
                                deck.color.ind = deck.color.ind,
                                num.non.land = num.non.land,
                                curve.penalty = curve.penalty)
    objective.values[i, "obj.val"] <- fit.deck$fit$objval
  }
  fit.deck <- optimize.colors(i = which.max(objective.values$obj.val),
                              draft = draft,
                              deck.color.ind = deck.color.ind,
                              num.non.land = num.non.land,
                              curve.penalty = curve.penalty)
  fit.deck <- list(fit.deck = fit.deck,
                   objective.values = objective.values)
}
