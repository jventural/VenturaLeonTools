extract_items <- function(text, prefix = "E") {
  lines <- strsplit(text, "\n")[[1]]
  extracted <- list()
  for (line in lines) {
    words <- strsplit(line, ": ")[[1]][-1]
    tag <- strsplit(line, ": ")[[1]][1]
    values <- paste0(prefix, gsub(prefix, "", unlist(strsplit(words, ", "))))
    extracted[[tag]] <- values
  }
  return(extracted)
}
