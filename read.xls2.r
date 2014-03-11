read.xls2 <- function(file, header=TRUE){
  os <- .Platform$OS.type
  if(os=="windows"){
    if(any(loadedNamespaces()=="gdata")) detach(package:gdata)
    require(xlsReadWrite)
    tf <- paste(tempfile(), "xls", sep = ".")
    download.file(file, tf, mode = "wb")
    out <- read.xls(tf, colNames=header)
  }else if(os=="unix"){
    if(any(loadedNamespaces()=="xlsReadWrite")) detach(package:xlsReadWrite)
    require(gdata)
    out <- read.xls(file, header=header)
  }else{
    warning("Only Mac and Windows")	
  }
  return(out)
}
