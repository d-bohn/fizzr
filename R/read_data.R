read_data <- function(data, file.type = NA, file.origin = NA, report = TRUE){

  if(report) cat("Reading Data: ", basename(data.file), "\n", sep="")

  if(is.na(file.type)){
    file.type <- unlist(strsplit(basename(data.file), "\\."))
    file.type <- file.type[length(file.type)]
  }

  in.data <- read.table(file=if(file.type == "gz"){gzfile(data.file)}else{data.file},
                        header = TRUE, sep = ",", quote = "\"'", dec = ".", as.is = TRUE,
                        na.strings = "NA", colClasses = NA, nrows = -1,
                        skip = 0, check.names = TRUE, fill = TRUE,
                        strip.white = TRUE, blank.lines.skip = TRUE,
                        comment.char = "#", allowEscapes = FALSE, flush = FALSE,
                        stringsAsFactors = FALSE, encoding = "unknown")

  if(!is.na(file.origin) && "time" %in% colnames(in.data)){
    in.data[,"time"] <- in.data[,"time"] + as.numeric(as.POSIXct(file.origin, tz="GMT"))
  }

  return(in.data)

}
