# MTGcsv.R

# https://mtgjson.com/

set.name <- "M20"

download.file(paste0("https://mtgjson.com/json/", set.name, ".json"),
              paste0("data/", set.name, ".json"))
# install.packages("jsonlite")
library(jsonlite)
mtg <- read_json(path = paste0("data/", set.name, ".json"))
# names(mtg)
# length(mtg[[155]])
# names(mtg[[155]])


set <- mtg$cards
# good.cols <- c("name", "rarity", "colId", "types", "cmc")
col.names.set <-  c("number", "name", "rarity", "colorIdentity",
                    "types", "convertedManaCost") # unique(unlist(lapply(set, names)))
  # c("name", "color", "rarity", "cmc", "grade")
set.flat <- data.frame(matrix("", ncol = length(col.names.set), nrow = length(set)),
                        stringsAsFactors = FALSE)
colnames(set.flat) <- col.names.set

for(i in 1:length(set)){
  for(j in 1:ncol(set.flat)){
    if(sum(names(set[[i]])==col.names.set[j])){
      ind <- which(names(set[[i]])==col.names.set[j])
      if(!is.list(set[[i]][ind])){
        set.flat[i, j] <- set[[i]][[ind]]
      } else {
        if(is.list(set[[i]][ind]) && length(set[[i]][[ind]]) > 0){
          set.flat[i, j] <- set[[i]][[ind]]
        }
      }
    }
  }
}

set.clean <- cbind(set.flat,
                   "B" = 0, "G" = 0, "R" = 0, "U" = 0, "W" = 0,
                   "quantity" = 0)
# Get color columns in order
for(i in 1:length(set)){
  card <- set[[i]]
  if(length(card$colorIdentity) > 0){
    for(j in 1:length(card$colorIdentity)){
      set.clean[i, card$colorIdentity[[j]]] <- 1
    }
  }
  if(length(card$colorIdentity) > 1){
    set.clean[i, "colorIdentity"] <- "M"
  }
}

# Fixing "colorIdentity"
set.clean[which(set.clean$types == "Land"), "colorIdentity"] <- "Land"
set.clean[which(set.clean$types == "Artifact"), "colorIdentity"] <- "Artifact"

# Account for artifact creatures as creatures
set.clean[which(lapply(sapply(set,function(x) x["types"]), length)>1),
          "types"] <- "Creature"

set.clean$number <- as.integer(set.clean$number)

# Combine split cards; use smaller CMC
split.card.nums <- which(table(set.clean$number) > 1)

if(length(split.card.nums) > 0){
  second.splits <- c()
  for(i in 1:length(split.card.nums)){
    inds <- which(set.clean$number == split.card.nums[i])
    set.clean$name[inds[1]] <- paste0(set.clean$name[inds[1]], "/",
                                      set.clean$name[inds[2]])
    second.splits <- c(second.splits, inds[2])
  }
  set.clean <- set.clean[-second.splits, ]
}

# Add in MTG Community Review rankings
Com.Review <- read.csv(paste0("data/", set.name, "Ratings.csv"), skip = 1)
Com.Review <- Com.Review[, c(9, 2, 6)]
colnames(Com.Review) <- c("number", "name", "score")
Com.Review$score <- round(Com.Review$score, 2)
# Com.Review$number <- round(Com.Review$score, 2)

# Merges, drop duplicate gates
set.merge <- merge(Com.Review, set.clean, "number")[, -c(1, 4)]
colnames(set.merge)[c(1, 6)] <- c("name", "cmc")

# Reorder columns
set.merge <- set.merge[, c("name", "rarity", "colorIdentity", "types", "cmc",
                           "score", "B", "G", "R", "U", "W", "quantity")]

# Reorder rows
set.merge <- set.merge[order(set.merge$colorIdentity, set.merge$cmc,
                             set.merge$name), ]

write.csv(set.merge, paste0("data/", set.name, ".csv"), row.names = FALSE)
write.csv(set.merge, paste0("MTG/", set.name, ".csv"), row.names = FALSE)
