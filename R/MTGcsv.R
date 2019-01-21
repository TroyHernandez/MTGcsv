# MTGcsv.R

# https://mtgjson.com/

# install.packages("jsonlite")
library(jsonlite)
unzip(zipfile = "AllSets.json")
mtg <- read_json(path = "AllSets.json")
# which(names(mtg)=="RNA")
length(mtg[[which(names(mtg)=="RNA")]])
names(mtg[[which(names(mtg)=="RNA")]])


RNA <- mtg$RNA$cards
col.names.RNA <- unique(unlist(lapply(RNA, names)))  
  # c("name", "color", "rarity", "cmc", "grade")
RNA.flat <- data.frame(matrix("", ncol = length(col.names.RNA), nrow = length(RNA)),
                        stringsAsFactors = FALSE)
colnames(RNA.flat) <- col.names.RNA

for(i in 1:length(RNA)){
  for(j in 1:ncol(RNA.flat)){
    if(sum(names(RNA[[i]])==col.names.RNA[j])){
      if(length(RNA[[i]][[which(names(RNA[[i]])==col.names.RNA[j])]]) > 0){
        RNA.flat[i, j] <- RNA[[i]][[which(names(RNA[[i]])==col.names.RNA[j])]]
      }
    }
  }
}

RNA.clean <- RNA.flat[, c("number", "name", "rarity", "colorIdentity",
                          "types", "convertedManaCost")]

RNA.clean <- cbind(RNA.clean,
                   "B" = 0, "G" = 0, "R" = 0, "U" = 0, "W" = 0,
                   "quantity" = 0)
# Get color columns in order
for(i in 1:length(RNA)){
  card <- RNA[[i]]
  if(length(card$colorIdentity) > 0){
    for(j in 1:length(card$colorIdentity)){
      RNA.clean[i, card$colorIdentity[[j]]] <- 1
    }
  }
  if(length(card$colorIdentity) > 1){
    RNA.clean[i, "colorIdentity"] <- "M"
  }
}

# Fixing "colorIdentity"
RNA.clean[which(RNA.clean$types == "Land"), "colorIdentity"] <- "Land"
RNA.clean[which(RNA.clean$types == "Artifact"), "colorIdentity"] <- "Artifact"

# Account for artifact creatures as creatures
RNA.clean[which(lapply(sapply(RNA,function(x) x["types"]), length)>1),
          "types"] <- "Creature"

# Next time remember to delete Planeswalker deck cards before reordering
# Remove out of set cards
RNA.clean$number <- as.integer(RNA.clean$number)
RNA.clean <- RNA.clean[which(RNA.clean$number <= 259), ]

# Combine split cards; use smaller CMC
split.card.nums <- which(table(RNA.clean$number) > 1)
second.splits <- c()
for(i in 1:length(split.card.nums)){
  inds <- which(RNA.clean$number == split.card.nums[i])
  RNA.clean$name[inds[1]] <- paste0(RNA.clean$name[inds[1]], "/",
                                   RNA.clean$name[inds[2]])
  second.splits <- c(second.splits, inds[2])
}
RNA.clean <- RNA.clean[-second.splits, ]

# Add in MTG Community Review rankings
Com.Review <- read.csv("RNAcommunityReview.csv", header = FALSE)
colnames(Com.Review) <- c("number", "name", "score")
Com.Review$score <- round(Com.Review$score, 2)

# Merges, drop duplicate gates
RNA.clean <- merge(Com.Review, RNA.clean, "number")[, -c(1, 4)]
colnames(RNA.clean)[c(1, 6)] <- c("name", "cmc")

# Reorder columns
RNA.clean <- RNA.clean[, c("name", "rarity", "colorIdentity", "types", "cmc",
                           "score", "B", "G", "R", "U", "W", "quantity")]

# Reorder rows
RNA.clean <- RNA.clean[order(RNA.clean$colorIdentity, RNA.clean$cmc,
                             RNA.clean$name), ]
RNA.clean.multi <- RNA.clean[which(RNA.clean$colorIdentity == "M"), ]
RNA.clean.multi <- RNA.clean.multi[order(RNA.clean.multi$G, RNA.clean.multi$U,
                                         RNA.clean.multi$R, RNA.clean.multi$B,
                                         RNA.clean.multi$cmc,
                                         RNA.clean.multi$name), ]
RNA.clean[which(RNA.clean$colorIdentity == "M"), ] <- RNA.clean.multi
  
write.csv(RNA.clean, "RNA.csv", row.names = FALSE)
