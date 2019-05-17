
## Helper function for parse_decklist
read.mtg <- function(file_path) {
  ## Read in mtg decklists saved in mtgo format specified here:
  ## https://magic.wizards.com/en/articles/archive/magic-online-reads-text-files-2004-10-26
  ## This format of each line is expected to be:
  ##  "x card name"
  ##  where "x" is the number of "card name" in the deck.
  
  ## Read mtg file by line
  x <- readLines(file_path)
  
  ## Remove blank lines
  x <- x[which(x != "")]
  
  ## Extract card counts from first set of characters 
  ## appearing before the first space of each line
  counts <- as.numeric(sub(" .*", "", x))
  
  ## Extract card names appearing after the first space of each line.
  ##  The first space will still remain in the substring.
  card_names <- sub(" *.", "", x)
  
  ## Remove the remaining first space from the card names
  card_names <- sapply(card_names, substring, first = 2, USE.NAMES = FALSE)
  
  data.frame(name = card_names,
             quantity = counts)
}

## Convert Magic: the Gathering Online output decklist
## into format expected by optimization functions.
parse_decklist <- function(file_path) {
  ## Actual drafted decklist.
  ## Output from Magic: the Gathering Online client
  my_deck <- read.mtg(file_path)
  
  ## Premade Guilds of Ravnica cardlist data set
  grn_cardlist <- read.csv("GRN.csv", stringsAsFactors = FALSE)
  
  merge(x = grn_cardlist[,-ncol(grn_cardlist)], ## Drop the last column "quantity"
        y = my_deck, ## Provides the actual card quantity
        by = "name")
}
