read.weka <-
function(filename, asList = FALSE) {
    options(warn = -1)
    my.lines <- readLines(filename)
    my.relat <- gsub("@relation[[:space:]]+", "", my.lines[grepl("@relation", my.lines)])
    my.attrs <- gsub("@attribute[[:space:]]+([[:alnum:]]+)[[:space:]].*$", "\\1", my.lines[grepl("@attribute", my.lines)])
    data.start <- which(grepl("@data", my.lines))
    
    data.w <- read.csv(filename, 
                       skip = data.start,
                       comment.char = "@", 
                       header = F,
                       col.names = my.attrs)
    options(warn = 0)
    if (asList) {
      return(list(data=data.w, relation=my.relat))
    } else {
      return(data.w)  
    }
  }
