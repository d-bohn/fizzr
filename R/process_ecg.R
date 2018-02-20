#' Identifies R-Spikes (via file annotation) in submitted ECG
#'
#' @param in.file.list
#' @param pattern
#' @param processing.mode
#' @param path
#' @param recursive
#' @param overwrite
#' @param save.fecg
#' @param check.ecg
#' @param filter.width
#' @param fl
#' @param fh
#' @param search
#' @param threshold
#' @param safe
#'
#' @return
#' @export
#'
#' @examples
#'
#'
process_ecg <- function(file.list = c(), pattern = ".phys.gz", processing.mode = "batch",
                        path = file.path(), recursive = TRUE, overwrite = FALSE, save.fecg = FALSE,
                        check.ecg = FALSE, filter.width = .10, fl = 5, fh = 20, search = 10, threshold = 7,
                        safe = 300){
  # process.ecg <- function(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, overwrite = FALSE, save.fecg = FALSE, check.ecg = FALSE, width = round(in.file.info$fs/10)+1, fl = 5, fh = 20, search = 10, threshold = 7, safe = 300){


  #
  #
  # Description
  #
  # .
  #
  #
  # Usage
  #
  # process.ecg(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, overwrite = FALSE, save.fecg = FALSE, check.ecg = FALSE, width = round(in.file.info$fs/10)+1, fl = 5, fh = 20, search = 10, threshold = 7, safe = 300)
  #
  #
  # Arguments
  #
  # in.file.list = List of files to be run through the function. The default, an empty vector, c(), triggers a graphical file or directory selector.
  #
  # pattern = The typical input file format; defaults to ".phys.gz".
  #
  # processing.mode = Determines the mode of file selection if in.file.list is empty. Valid values are "interactive" and "batch"; defaults to "interactive".
  #
  # path = The starting directory for the graphical file or directory selector. Defaults to file.path(), an empty file path.
  #
  # recursive = When operating in batch mode, determines whether files nested in folders are searched for the pattern file type; defaults to TRUE.
  #
  # overwrite = Whether previously completed processing steps should be overwritten; defaults to FALSE.
  #
  # save.fecg = Determines whether the filtered ECG used by the QRS detection algorithm is saved in the data file along with the location of detected R-spikes; defaults to FALSE. This can considerably increas the size of data files and should only be necessary if the raw ECG is noisy.
  #
  # check.ecg = If TRUE, plots the first 5 seconds of the raw ECG to check if the ECG was recorded upside down; defaults to FALSE.
  #
  # filter.width = the width (in seconds) of the window used to bandpass filter the raw ECG; defaults to .10.
  #
  # fl = The lower cutoff frequency (in Hertz) used to bandpass filter the raw ECG; defaults to 5.
  #
  # fh = The higher cutoff frequency (in Hertz) used to bandpass filter the raw ECG; defaults to 20.
  #
  # search = Time (in milliseconds) algorithm searches forward and backward for R-spikes; defaults to 10.
  #
  # threshold = Determines the sensitivity of the R-spike detection. Larger numbers decrease sensitivity; defaults to 7.
  #
  # safe = Time (in milliseconds) R-spike detector is turned off following detection of an R-spike (prevents false detection of peaky T-waves); defaults to 300.
  #
  #


  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!length(file_list)) file_list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in file_list){
    #(in.file <- in.file.list[1])

    cat("\n\nProcessing: ",basename(in.file),"\n",sep=""); if(!interactive()) flush.console()

    out.file <- in.file

    if(!overwrite){
      if("r" %in% read.vars(in.file)){
        #if(file.exists(out.file)){
        cat("Processing halted to avoid overwrite.\n", sep=""); if(!interactive()) flush.console()
        next()
      }
    }

    file.data <- read.data(in.file)

    in.file.info <- import.file.info(in.file)

    if(check.ecg && check.inverted(file.data[1:(5*in.file.info$fs),"time"], file.data[1:(5*in.file.info$fs),"ecg"])) file.data[,"ecg"] <- -1 * file.data[,"ecg"]

    width <- filter.width * in.file.info$fs; if(as.integer(width %% 2 != 1)) width <- width + 1

    ecg.data <- qrs(ecg = file.data[,"ecg"], fs = in.file.info$fs, return.fecg = save.fecg, width = width, fl = fl, fh = fh, search = search, threshold = threshold, safe = safe)

    file.data <- cbind(file.data[, !colnames(file.data) %in% colnames(ecg.data)], ecg.data); rm(ecg.data)

    write.data(file.data, out.file)

  }

}
