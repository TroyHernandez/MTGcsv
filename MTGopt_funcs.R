# MTGopt_funcs.R

optimize.colors <- function(i, draft, deck.color.ind,
                            Mtg.colors = c("B", "G", "R", "U", "W")){
  remove.colors <- c(i)
  colors.ind <- which(rowSums(draft[, Mtg.colors[-deck.color.ind[i,]]]) == 0)
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
  f.rhs <- c(colors.mat$quantity, num.non.land)
  
  all.int <- TRUE
  
  fit <- lp("max", f.obj, f.con, f.dir, f.rhs, all.int = TRUE)
  
  deck <- cbind(colors.mat[which(fit$solution != 0),],
                quantity.used = fit$solution[which(fit$solution != 0)])
  fit.deck <- list(fit = fit, deck = deck)
}