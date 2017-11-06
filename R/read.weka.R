read.weka <- 
function (filename, asList = FALSE, tmpFileName = "myTMP.file.tmp") 
{
  options(warn = -1)
  my.lines <- readLines(filename)
  writeLines(text = my.lines[!grepl("^[[:space:]]*%", my.lines)], con = tmpFileName)
  my.lines <- readLines(tmpFileName)
  my.relat <- gsub("@relation[[:space:]]+", "", 
                   my.lines[grepl("@relation", my.lines, ignore.case = T)], ignore.case = T)
  
  my.attrs <- my.lines[grepl("@attribute", my.lines, ignore.case = T)]
  my.attrs <- gsub("@ATTRIBUTE[[:space:]]+([^[:space:]]+)[[:space:]].*$", "\\1", my.attrs, ignore.case = TRUE)

  data.start <- which(grepl("@data", my.lines, ignore.case = T))
  data.w <- read.csv(tmpFileName, skip = data.start, comment.char = "@", 
                     header = F, col.names = my.attrs)
  options(warn = 0)
  if (asList) {
    return(list(data = data.w, relation = my.relat))
  }
  else {
    return(data.w)
  }
}
