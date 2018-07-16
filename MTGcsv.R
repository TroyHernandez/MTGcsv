# MTGcsv.R

# https://mtgjson.com/

install.packages("jsonlite")
library(jsonlite)
mtg <- read_json(path = "AllSets.json/AllSets.json")
names(mtg)
length(mtg[[155]])
names(mtg[[155]])


m19 <- mtg$M19$cards
col.names.m19 <- unique(unlist(lapply(m19, names)))  
  # c("name", "color", "rarity", "cmc", "grade")
m19.flat <- data.frame(matrix("", ncol = length(col.names.m19), nrow = length(m19)),
                        stringsAsFactors = FALSE)
colnames(m19.flat) <- col.names.m19

for(i in 1:length(m19)){
  for(j in 1:ncol(m19.flat)){
    if(sum(names(m19[[i]])==col.names.m19[j])){
      m19.flat[i, j] <- m19[[i]][[which(names(m19[[i]])==col.names.m19[j])]]
    }
  }
}

m19.clean <- m19.flat[, c("name", "rarity", "colorIdentity", "types", "cmc")]

write.csv(m19.clean, "m19.csv", row.names = FALSE)
