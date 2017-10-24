export.weka <-
function(data, filename, relation) {
    options(warn = -1)
    out.lines <- list()
    
    out.lines[[(length(out.lines) + 1)]] <- paste("@relation", relation)
    out.lines[[(length(out.lines) + 1)]] <- ""
    
    for (i in 1:ncol(data)) {
      
      # get var name
      if (is.null(names(data)[i])) {
        tmp.colnm <- paste("V", i, sep = "")
      } else {
        tmp.colnm <- names(data)[i]
      }
      
      # get data type: numeric?
      if (is.numeric(data[,i])) {
        attDecor <- "real"
      } else {
        attDecor <- unique(as.character(as.vector(data[,i])))
        attDecor <- paste(attDecor, collapse = ", ")
        attDecor <- paste( "{", attDecor, "}", sep = "")
      }
      
      # attach attribute
      out.lines[[(length(out.lines) + 1)]] <- paste("@attribute",  tmp.colnm, attDecor)
    }
    out.lines[[(length(out.lines) + 1)]] <- ""
    out.lines[[(length(out.lines) + 1)]] <- "@data"
    for (i in 1:nrow(data)) {
      out.lines[[(length(out.lines) + 1)]] <- paste(as.vector(data[i,]), collapse = ",")
    }
    
    # unlist
    out.lines <- do.call(c, out.lines)
    writeLines(out.lines, filename)
    options(warn = -0)
    message("Done!")
  }
