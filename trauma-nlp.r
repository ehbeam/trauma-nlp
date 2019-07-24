# Install and import NLP library
install.packages("textstem")
library(textstem)

# Specify term lists for trauma types
type_names = list("MILITARY", "MURDER", "DEATH")
types = list(list("war", "battle", "gun", "grenade", "military"),
             list("murder", "kill", "strangulation", "weapon"),
             list("grave", "funeral", "death", "die", "on edge"))
n = length(types)

# Preprocess term lists
types_proc = list()
for (i in 1:n) {
  for (j in 1:length(types[[i]])) {
    words = types[[i]]
    words = stem_strings(words)
    words = gsub(" ", "_", words)
    types_proc[[i]] = words
  }
}

# Load inputs from a directory with one text file per patient
setwd(getwd())
files = list.files("texts")
m = length(files)
texts = c()
for (i in 1:m) {
  file = paste("texts/", files[i], sep="")
  text = readChar(file, file.info(file_name)$size)
  texts = c(texts, text)
}
input_names = gsub(".txt", "", files)

# Extract phrases to combine into terms
phrases = c()
for (i in 1:n) { 
  for (j in 1:length(types[[i]])) {
    if (grepl("_", types[[i]][[j]])) {
      phrases = c(phrases, types[[i]][[j]])
    }
  } 
}

# Perform basic text preprocessing
proc = list()
for (i in 1:n) {
  text = tolower(texts[i]) # Convert to lowercase
  text = (gsub("\r?\n|\r|\t", " ", text)) # Remove line breaks
  text = (gsub("[[:punct:] ]+", " ", text)) # Remove punctuation
  words = strsplit(text, " ") # Split into a list of words
  words = stem_words(as.vector(words[[1]]))# Perform stemming
  for (phrase in phrases) {
    phrase_with_spaces = gsub("_", " ", phrase)
    words = gsub(phrase_with_spaces, phrase, words)
  } 
  proc[[i]] = words
}

# Score trauma types in each text
scores = matrix(data=NA, nrow=n, ncol=length(types), 
                dimnames=list(input_names, type_names))
for (i in 1:m) {
  input_words = proc[[i]]
  for (j in 1:n) {
    type_words = types[[j]]
    intersection = intersect(input_words, type_words)
    scores[i,j] = length(intersection) / length(type_words)
  }
}
write.csv(scores, file="scores.csv")
