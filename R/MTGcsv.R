# MTGcsv.R

# https://mtgjson.com/

install.packages("jsonlite")
library(jsonlite)
mtg <- read_json(path = "AllSets.json")
# which(names(mtg)=="GRN")
length(mtg[[which(names(mtg)=="GRN")]])
names(mtg[[which(names(mtg)=="GRN")]])


GRN <- mtg$GRN$cards
col.names.GRN <- unique(unlist(lapply(GRN, names)))  
  # c("name", "color", "rarity", "cmc", "grade")
GRN.flat <- data.frame(matrix("", ncol = length(col.names.GRN), nrow = length(GRN)),
                        stringsAsFactors = FALSE)
colnames(GRN.flat) <- col.names.GRN

for(i in 1:length(GRN)){
  for(j in 1:ncol(GRN.flat)){
    if(sum(names(GRN[[i]])==col.names.GRN[j])){
      GRN.flat[i, j] <- GRN[[i]][[which(names(GRN[[i]])==col.names.GRN[j])]]
    }
  }
}

GRN.clean <- GRN.flat[, c("name", "rarity", "colorIdentity", "types", "cmc")]

GRN.clean <- cbind(GRN.clean, "score" = 0,
                   "B" = 0, "G" = 0, "R" = 0, "U" = 0, "W" = 0,
                   "quantity" = 0)
# Get color columns in order
for(i in 1:length(GRN)){
  card <- GRN[[i]]
  for(j in 1:length(card$colorIdentity)){
    GRN.clean[i, card$colorIdentity[[j]]] <- 1
  }
  if(length(card$colorIdentity) > 1){
    GRN.clean[i, "colorIdentity"] <- "M"
  }
}

# Fixing "colorIdentity"
GRN.clean[which(GRN.clean$type == "Land"), "colorIdentity"] <- "Land"
GRN.clean[which(GRN.clean$type == "Artifact"), "colorIdentity"] <- "Artifact"

# Account for artifact creatures as creatures
GRN.clean[which(lapply(sapply(GRN,function(x) x["types"]), length)>1),
          "types"] <- "Creature"

# Remove basic lands
basic.land <- which(GRN.clean$rarity=="Basic Land")
GRN.clean <- GRN.clean[-basic.land, ]

# Next time remember to delete Planeswalker deck cards before reordering
# Reordering to be able to copy down scores by hand more easily
GRN.clean <- GRN.clean[order(GRN.clean$colorIdentity, GRN.clean$name), ]

write.csv(GRN.clean, "GRN.csv", row.names = FALSE)

# Read back in and sort by color and cmc
GRN.clean <- read.csv("GRN.csv")
GRN.clean <- GRN.clean[order(GRN.clean$colorIdentity, GRN.clean$cmc), ]
write.csv(GRN.clean, "GRN.csv", row.names = FALSE)
