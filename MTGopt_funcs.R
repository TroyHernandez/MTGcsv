# MTGopt_funcs.R

library("lpSolve")
library("quadprog")
library("ROI")

optimize.colors <- function(i, draft, deck.color.ind,
                            num.non.land = 23,
                            curve.penalty = 5,
                            removal.penalty = 5,
                            Mtg.colors = c("B", "G", "R", "U", "W"),
                            tribes = c("Artifact", "Ramp", "Goblin", "Dragon",
                                       "Lifelink", "Weenie", "Zombie"),
                            tribal.boost = rep(5, length(tribes))){
  remove.colors <- c(i)
  # Take out cards with other colors
  colors.ind <- which(rowSums(draft[, Mtg.colors[-deck.color.ind[i,]]]) == 0)
  colors.mat <- draft[colors.ind, ]
  
  # We are trying to maximize the collective score
  f.obj <- c(colors.mat$score)
  # adding in creature curve
  f.obj <- c(f.obj, rep(-1 * curve.penalty, 12))
  # adding in removal requirement
  f.obj <- c(f.obj, -1 * removal.penalty)
  
  
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
  # Add in removal requirement
  removal.vec <- c(colors.mat$Removal, rep(0, length = ncol(f.con) - length(colors.mat$Removal)))
  f.con <- rbind(f.con, removal.vec)
  f.con <- cbind(f.con, c(rep(0, nrow(f.con) - 1), 1))
  
  # The constraint to only pick cards we have
  f.dir <- c(rep("<=", nrow(colors.mat)), rep("==", 7), ">=")
  
  # Using the cards we have for the RHS
  creature.curve <- c(1, 5, 4, 4, 3, 2)
  min.removal <- 4
  f.rhs <- c(colors.mat$quantity, num.non.land, creature.curve, min.removal)
  
  all.int <- TRUE
  
  fit <- lp("max", f.obj, f.con, f.dir, f.rhs, all.int = TRUE)
  
  cards <- cbind(colors.mat, quantity.used = fit$solution[1:nrow(colors.mat)])

  unused.cards <- cards[which(cards$quantity > cards$quantity.used), ]

  deck <- cbind(colors.mat[which(fit$solution[1:nrow(colors.mat)] != 0),],
                quantity.used = fit$solution[which(fit$solution[1:nrow(colors.mat)] != 0)])
  fit.deck <- list(fit = fit, deck = deck, unused.cards = unused.cards)
}

optimize.colors.qp <- function(i, draft, deck.color.ind,
                            num.non.land = 23,
                            curve.penalty = 5,
                            removal.penalty = 5,
                            Mtg.colors = c("B", "G", "R", "U", "W"),
                            tribes = c("Artifact", "Ramp", "Goblin", "Dragon",
                                       "Lifelink", "Weenie", "Zombie"),
                            tribal.boost = rep(5, length(tribes))){
  # Take out cards with other colors
  colors.ind <- which(rowSums(draft[, Mtg.colors[-deck.color.ind[i,]]]) == 0)
  colors.mat <- draft[colors.ind, ]
  
  # We are trying to maximize the collective score
  Dmat <- create.qp.mats(colors.mat, tribes = tribes,
                         tribal.boost = tribal.boost)
  diag(Dmat) <- 0
  
  dvec <- c(colors.mat$score)
  # adding in creature curve
  dvec <- c(dvec, rep(-1 * curve.penalty, 12))
  # adding in removal requirement
  dvec <- c(dvec, -1 * removal.penalty)
  d.diff <- length(dvec) - ncol(Dmat)
  Dmat <- rbind(Dmat, matrix(0, nrow = d.diff, ncol = ncol(Dmat)))
  Dmat <- cbind(Dmat, matrix(0, nrow = nrow(Dmat), ncol = d.diff))
  
  # We can only select the cards we have, the constraint will be on the rhs
  Amat <- diag(1, nrow = nrow(colors.mat), ncol = nrow(colors.mat))
  # We need to have 23 non-land cards
  non.land.vec <- rep(1, nrow(colors.mat))
  non.land.vec[which(colors.mat$types == "Land")] <- 0
  Amat <- rbind(Amat, non.land.vec)
  # Add in creature curve 
  creature.mat <- matrix(0, ncol = nrow(colors.mat), nrow = 6)
  creature.mat[1, which(colors.mat$types == "Creature" & colors.mat$cmc == 1)] <- 1
  creature.mat[2, which(colors.mat$types == "Creature" & colors.mat$cmc == 2)] <- 1
  creature.mat[3, which(colors.mat$types == "Creature" & colors.mat$cmc == 3)] <- 1
  creature.mat[4, which(colors.mat$types == "Creature" & colors.mat$cmc == 4)] <- 1
  creature.mat[5, which(colors.mat$types == "Creature" & colors.mat$cmc == 5)] <- 1
  creature.mat[6, which(colors.mat$types == "Creature" & colors.mat$cmc > 5)] <- 1
  Amat <- rbind(Amat, creature.mat)
  # Add in slack.mat
  creature.slack.mat <- matrix(0, nrow = 6, ncol = 12)
  creature.slack.mat[cbind(1:6, seq(1, 11, 2))] <- 1
  creature.slack.mat[cbind(1:6, seq(2, 12, 2))] <- -1
  Amat <- cbind(Amat,
                 rbind(matrix(0, nrow = nrow(Amat) - nrow(creature.slack.mat),
                              ncol = 12),
                       creature.slack.mat))
  # Add in removal requirement
  removal.vec <- c(colors.mat$Removal, rep(0, length = ncol(Amat) - length(colors.mat$Removal)))
  Amat <- rbind(Amat, removal.vec)
  Amat <- cbind(Amat, c(rep(0, nrow(Amat) - 1), 1))

  # The constraint to only pick cards we have
  f.dir <- c(rep("<=", nrow(colors.mat)), rep("==", 7), ">=")
  
  # Using the cards we have for the RHS
  creature.curve <- c(1, 5, 4, 4, 3, 2)
  min.removal <- 4
  bvec <- c(colors.mat$quantity, num.non.land, creature.curve, min.removal)
  
  # Add in non-negative contraints
  Amat <- rbind(Amat, diag(1, ncol = ncol(Amat), nrow = ncol(Amat)))
  bvec <- c(bvec, rep(0, ncol(Amat)))
  f.dir <- c(f.dir, rep(">=", ncol(Amat)))
  
  # all.int <- TRUE
  # require("ROI")
  fit <- OP(Q_objective(Dmat*0, L =  dvec),
          L_constraint(L = Amat,
                       dir = f.dir,
                       rhs = bvec))
  opt <- ROI_solve(fit, solver="qpoases")
  opt
  solution(opt)
  # fit <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = t(Amat), bvec = bvec)
  
  deck <- cbind(colors.mat[which(fit$solution[1:nrow(colors.mat)] != 0),],
                quantity.used = fit$solution[which(fit$solution[1:nrow(colors.mat)] != 0)])
  fit.deck <- list(fit = fit, deck = deck)
}

optimize.draft <- function(draft, num.colors, num.non.land,
                           curve.penalty = 5, removal.penalty = 5,
                           deck.choice = "Best"){
  Mtg.colors <- c("B", "G", "R", "U", "W")
  deck.colors <- t(combn(Mtg.colors, num.colors))
  deck.color.ind <- t(combn(1:5, num.colors))
  objective.values <- data.frame(deck.colors, obj.val = rep(0, nrow(deck.colors)))
  # subset mat
  for(i in 1:nrow(objective.values)){
    fit.deck <- optimize.colors(i = i, draft = draft,
                                deck.color.ind = deck.color.ind,
                                num.non.land = num.non.land,
                                curve.penalty = curve.penalty,
                                removal.penalty = removal.penalty)
    objective.values[i, "obj.val"] <- fit.deck$fit$objval
  }
  ov.colors <- apply(objective.values[, -ncol(objective.values)],
                     1, paste0, collapse = "")
  obj.vals <- data.frame(colors = ov.colors, value = objective.values$obj.val)
  
  if(deck.choice == "Best"){
    fit.deck <- optimize.colors(i = which.max(objective.values$obj.val),
                                draft = draft,
                                deck.color.ind = deck.color.ind,
                                num.non.land = num.non.land,
                                curve.penalty = curve.penalty,
                                removal.penalty = removal.penalty)
  } else {
    fit.deck <- optimize.colors(i = which(obj.vals$colors == deck.choice),
                                draft = draft,
                                deck.color.ind = deck.color.ind,
                                num.non.land = num.non.land,
                                curve.penalty = curve.penalty,
                                removal.penalty = removal.penalty)
  }
  
  obj.vals <- obj.vals[order(obj.vals$value, decreasing = TRUE), ]
  fit.deck <- list(fit.deck = fit.deck,
                   objective.values = obj.vals)
}


odds.no.land <- function(num.turns, deck.size = 40, num.land.hand = 2,
                         draw.size = 7, num.lands = 17){
  total.cards.deck <- deck.size - draw.size
  land.left <- num.lands - num.land.hand
  sample.vec <- rep(0, 10000)
  for(i in 1:10000){
    sample.vec[i] <- sum(sample(total.cards.deck,
                                size = num.turns) <= land.left)
  }
  quantile(sample.vec, seq(0, 1, .01))
}

# odds.no.land(num.turns = 12, deck.size = 40, draw.size = 6, num.land.hand = 2)

# tribes = c("Artifact", "Ramp", "Goblin", "Dragon", "Lifelink")
# new.tribes = c("Knight")
create.qp.mats <- function(draft,
                           tribes = c("Artifact", "Ramp", "Goblin", "Dragon",
                                      "Lifelink", "Weenie", "Zombie"),
                           tribal.boost = rep(5, length(tribes))){
  tribe.mat <- matrix(0, nrow = nrow(draft), ncol = nrow(draft))
  for(i in 1:length(tribes)){
    tribe.mat <- tribe.mat +
      tribal.boost[i] * draft[, tribes[i]] %*% t(draft[, tribes[i]])
  }
  tribe.mat
}