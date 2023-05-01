extract_items <- function(text) {
  lines <- strsplit(text, "\n")[[1]]
  extracted <- list()
  for (line in lines) {
    words <- strsplit(line, ": ")[[1]][-1]
    tag <- strsplit(line, ": ")[[1]][1]
    values <- paste0("E", gsub("E", "", unlist(strsplit(words, ", "))))
    extracted[[tag]] <- values
  }
  return(extracted)
}