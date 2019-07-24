# Load the NLP library
require("textstem")

# Specify term lists for trauma types
type_names = list("MILITARY", "MURDER", "DEATH")
types = list(list("war", "battle", "gun", "grenade", "military"),
             list("murder", "kill", "strangulation", "weapon"),
             list("grave", "funeral", "death", "die", "on edge"))
n = length(types)

# Function to preprocess a list of words
preprocess = function(words) {
  words = tolower(words) # Convert to lowercase
  words = gsub("\r?\n|\r|\t", " ", words) # Remove line breaks
  words = gsub("[[:punct:] ]+", " ", words) # Remove punctuation
  words = stem_words(as.vector(words)) # Perform stemming
  return(words)
}

# Preprocess trauma type lists
proc_types = list()
for (i in 1:n) {
  words = preprocess(types[[i]])
  words = gsub(" ", "_", words) # Combine phrases into terms
  proc_types[[i]] = words
}

# Extract phrases from trauma type lists
phrases = c()
for (i in 1:n) { 
  for (j in 1:length(types[[i]])) {
    if (grepl("_", proc_types[[i]][[j]])) {
      phrases = c(phrases, proc_types[[i]][[j]])
    }
  } 
}

# Load and preprocess input text files
setwd(getwd())
files = list.files("texts")
input_names = gsub(".txt", "", files)
m = length(files)
proc_texts = list()
for (i in 1:m) {
  file = paste("texts/", files[i], sep="")
  text = readChar(file, file.info(file_name)$size)
  words = strsplit(text, " ") # Split into a list of words
  words = preprocess(words[[1]]) # Preprocess the word list
  text = paste(words, collapse = " ") # Recombine into a string
  for (phrase in phrases) { # Combine phrases from trauma types
    phrase_with_spaces = gsub("_", " ", phrase)
    text = gsub(phrase_with_spaces, phrase, text)
  }
  proc_texts[i] = strsplit(text, " ") # Split into a term list
}

# Score texts by the proportion of terms that occurred from each trauma type
scores = matrix(data=NA, nrow=n, ncol=length(types), 
                dimnames=list(input_names, type_names))
for (i in 1:m) {
  input_words = proc_texts[[i]]
  for (j in 1:n) {
    type_words = proc_types[[j]]
    intersection = intersect(input_words, type_words)
    scores[i,j] = length(intersection) / length(type_words) 
  }
}
write.csv(scores, file="scores.csv")
