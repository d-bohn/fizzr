artifact.ibi <- function(in.file.list = c(), pattern = ".ibi.gz", processing.mode = "interactive",
                         path = file.path(), recursive = TRUE, overwrite = FALSE,
                         threshold = c(200,200), limits = c(400,1800), safe = c(100,100),
                         width = 3, frs = 10, n.iterations = 1, artifact.plot = FALSE){


  # Descritption
  #
  # Identifies artifact in a submitted ibi time series file.
  #
  #
  # Usage
  #
  # artifact.ibi(in.file.list = c(), pattern = ".ibi.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, overwrite = FALSE, threshold = c(200,200), limits = c(400,1800), safe = c(100,100), width = 3, frs = 10, n.iterations = 1, artifact.plot = FALSE)
  #
  #
  # Arguments
  #
  # in.file.list = List of files to be run through the function. The default, c(), an empty list, which triggers a graphical file or directory selector.
  #
  # pattern = The typical input file format; defaults to ".ibi.gz".
  #
  # processing.mode = Determines the mode of file selection if in.file.list is empty. Valid values are "interactive" and "batch"; defaults to "interactive".
  #
  # path = The starting directory for the graphical file or directory selector. Defaults to file.path(), an empty file path.
  #
  # recursive = When operating in batch mode, determines whether files nested in folders are searched for the pattern file type; defaults to TRUE.
  #
  # overwrite = Whether previously completed processing steps should be overwritten; defaults to FALSE.
  #
  # threshold = The minimum change from the preceding non-artifactual beat that must be met to be considered as a candidate artifact. First value sets threshold for negative changes and second value for positive changes. Numbers less than 1 (e.g., .20) represent percent changes from previous IBI. If a single number is submitted, the same value is used for both negative and positive changes; defaults to c(200,200).
  #
  # limits = Sets hard limits for minimum (first value) and maximum (second value) acceptable range for IBI; defaults to c(400,1800).
  #
  # safe = The minimum deviation from a running window mean potential artifact must exceed in order to be identified as actual artifact; defaults to c(100,100).
  #
  # width = The width in seconds of the running mean window; defaults to 3.
  #
  # frs = The (re)sampling rate used in the creation of the running mean window; defaults to 10.
  #
  # n.iterations = Number of times the IBI artifact detection algorithm loops through the IBI series searching for artifact, each time removing previously identified artifact; defaults to 1.
  #
  # artifact.plot = Whether the submitted IBI series should be plotted and overlays identified artifact; defaults to FALSE.
  #

  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])

    cat("\n\nProcessing: ",basename(in.file),"\n",sep=""); if(!interactive()) flush.console()

    #in.file.info <- import.file.info(in.file)

    out.file <- in.file

    file.ibi <- read.data(in.file)

    if(!overwrite & "artifact" %in% colnames(file.ibi)){
      cat("Moving to next file.\n", sep=""); if(!interactive()) flush.console()
      next()
    }

    file.ibi[,"artifact"] <- derivative.artifact(x = file.ibi[,"ibi"], x.t = file.ibi[,"time"], threshold = threshold, limits = limits, safe = safe, frs = frs, width = width, n.iterations = n.iterations, artifact.plot = artifact.plot)

    write.data(file.ibi, out.file)

  }

}

extract.hrv <- function(in.file.list = c(), pattern = ".ibi.gz", processing.mode = "interactive",
                        path = file.path(), recursive = TRUE, overwrite = FALSE,
                        output.label = "hrv",
                        f.bands = list(hf = c(.15,.4), lf = c(.04,.15)),
                        frs = 10, width = 241){


  #
  #
  # Description
  #
  # Extracts HRV estimates for the epochs defined in the input ibi data file's epoch list using the band-limited variance method described in Allen, Chambers, & Towers (2007). Biological Psychology, 74, 243–262.
  #
  #
  # Usage
  #
  # extract.hrv(in.file.list = c(), pattern = ".ibi.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, overwrite = FALSE, output.label = "hrv", f.bands = list(hf = c(.15,.4), lf = c(.04,.15)), frs = 10, width = 241)
  #
  #
  # Arguments
  #
  # in.file.list = List of files to be run through the function. The default, an empty vector, c(), triggers a graphical file or directory selector.
  #
  # pattern = The typical input file format; defaults to ".ibi.gz".
  #
  # processing.mode = Determines the mode of file selection if in.file.list is empty. Valid values are "interactive" and "batch"; defaults to "interactive".
  #
  # path = The starting directory for the graphical file or directory selector. Defaults to file.path(), an empty file path.
  #
  # recursive = When operating in batch mode, determines whether files nested in folders are searched for the pattern file type; defaults to TRUE.
  #
  # overwrite = Whether previously completed processing steps should be overwritten; defaults to FALSE.
  #
  # output.label = The label used to identify hrv output; defaults to "hrv". Can be used to perform different HRV runs, possibly with different setting without overwriting earlier HRV output files.
  #
  # f.bands = A named list used to idenfity the frequency bands used by the band pass filter employed by the band-limited varance method; defaults to list(hf = c(.15,.4), lf = c(.04,.15)). Note that each list item is itself a two item vector specifying the lower and upper frequency bounds. A zero for either the lower or upper bound results in the application of a low or high pass filter, respectively. The names of the list items are used to name the variance variables in the resulting output hrv data file.
  #
  # frs = The (re)sampling rate used to convert the IBI event series to an evenly sampled time series for the band-limited varance method; defaults to 10.
  #
  # width = The width of the filter used in the band-limited varance method; defaults to 241.
  #
  #

  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])

    cat("\n\nProcessing: ",basename(in.file),"\n",sep=""); if(!interactive()) flush.console()

    out.file <- file.path(dirname(in.file), gsub(pattern, paste(".",output.label,".gz",sep=""), basename(in.file), fixed = TRUE))

    if(!overwrite & file.exists(out.file)){
      cat("Moving to next file.\n", sep=""); if(!interactive()) flush.console()
      next()
    }

    # 		in.file.info <- import.file.info()
    in.file.info <- import.file.info(in.file)

    ibi <- read.data(in.file, file.origin = in.file.info$file.origin)

    epoch.list <- import.epoch.list(in.file.list = gsub(".ibi.gz", ".epoch.txt", in.file, fixed = TRUE), file.origin = in.file.info$file.origin, event.times = NA, time.vector = NA, report = TRUE, print.epochs = FALSE)

    # 		if("file.origin" %in% colnames(in.file.info)){
    # 			ibi[,1] <- ibi[,1] + as.numeric(as.POSIXct(in.file.info$file.origin, tz="GMT"))
    # 		}

    ### BLV Routine ###

    rs.ibi <- seq(min(ibi[!ibi$artifact,"time"]), max(ibi[!ibi$artifact,"time"]), 1/frs)
    rs.ibi <- cbind(time=rs.ibi, ibi=approx(ibi[!ibi$artifact,"time"], ibi[!ibi$artifact,"ibi"], rs.ibi, rule=1, method="linear")$y)

    for(f in names(f.bands)){
      #(f <- names(f.bands)[1])
      if(f.bands[[f]][1] == 0){
        filter.coef <- lpcoef(width, frs, f.bands[[f]][2], "hamming")
      }else{
        filter.coef <- bpcoef(width, frs, f.bands[[f]][1], f.bands[[f]][2], "hamming")
      }
      rs.ibi <- cbind(rs.ibi, data.frame(f = as.vector(filter(rs.ibi[,"ibi"], filter.coef, circular = FALSE))))
      colnames(rs.ibi)[colnames(rs.ibi) == "f"] <- f
    }

    file.hrv <- data.frame()
    cat("Running HRV across epochs:\n", sep=""); if(!interactive()) flush.console()
    for(epoch in 1:nrow(epoch.list)){
      #(epoch <- 38)

      epoch.hrv <- data.frame()

      ibi.vector <- ibi[,"time"] >= epoch.list$start[epoch] & ibi[,"time"] < epoch.list$stop[epoch]
      rs.ibi.vector <- length(f.bands) & rs.ibi[,"time"] >= epoch.list$start[epoch] & rs.ibi[,"time"] < epoch.list$stop[epoch]

      if(any(ibi.vector)){
        #td.vars <- hrv.time(ibi[ibi.vector,], frs=0)
        epoch.hrv <- data.frame(epoch = epoch.list$name[epoch], hrv.time(ibi[ibi.vector,], frs=0))
        if(length(f.bands)){
          #blv.vars <- as.list(apply(rs.ibi[rs.ibi.vector,names(f.bands)], 2, var))
          epoch.hrv <- data.frame(epoch.hrv, as.list(apply(rs.ibi[rs.ibi.vector,names(f.bands)], 2, var, na.rm = TRUE)))
        }
        file.hrv <- rbind(file.hrv, epoch.hrv)
        cat("\tEpoch ",epoch.list$name[epoch],": ",round(epoch.hrv$length)," sec\n", sep=""); if(!interactive()) flush.console()
        rm(epoch.hrv)
      }else{
        cat("\tEpoch ",epoch.list$name[epoch],": Empty\n", sep=""); if(!interactive()) flush.console()
      }
      rm(ibi.vector,rs.ibi.vector)
    }

    write.data(file.hrv, out.file)

    rm(ibi,rs.ibi)

  }

}

extract.ibi <- function(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive",
                        path = file.path(), recursive = TRUE, overwrite = FALSE){


  #
  #
  # Description
  #
  # Extracts an interbeat interval event series from annotated R-spikes in submitted data file and outputs an ibi data file.
  #
  #
  # Usage
  #
  # extract.ibi(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, overwrite = FALSE)
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
  #


  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])

    cat("\n\nProcessing: ",basename(in.file),"\n",sep=""); if(!interactive()) flush.console()

    out.file <- unlist(strsplit(basename(in.file), "\\."))
    out.file <- file.path(dirname(in.file), gsub(paste(".",out.file[length(out.file)-1],".",out.file[length(out.file)],sep=""), ".ibi.gz", basename(in.file), fixed = TRUE))

    if(!overwrite & file.exists(out.file)){
      cat("Moving to next file.\n", sep=""); if(!interactive()) flush.console()
      next()
    }

    file.data <- read.data(in.file)

    in.file.info <- import.file.info(in.file)

    file.ibi <- data.frame(time = file.data[as.logical(file.data$r),"time"][-1], ibi = diff(file.data[as.logical(file.data$r),"time"])*1000)
    file.ibi <- file.ibi[!apply(file.ibi, 1, function(x){any(is.na(x))}),]

    write.data(file.ibi, out.file)

  }

}

extract.resp <- function(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive",
                         path = file.path(), recursive = TRUE, overwrite = FALSE,
                         output.label = "resp", resp.var = "resp", fl = .1, fh = .6){


  #
  #
  # Description
  #
  # Extracts an interbeat interval event series from annotated R-spikes in submitted data file and outputs an ibi data file.
  #
  #
  # Usage
  #
  # extract.resp(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, overwrite = FALSE, output.label = "resp", resp.var = "resp", fl = .1, fh = .6)
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
  # output.label = The label used to identify respiration output; defaults to "resp". Can be used to perform different extraction runs, possibly with different setting without overwriting earlier respiration output files.
  #
  # resp.var = The label used to identify respiration output; defaults to "resp". Can be used to perform different extraction runs, possibly with different setting without overwriting earlier respiration output files.
  #
  # fl = The lower bound (in Hertz) of the frequency band from which the peak respiration frequency is identified in the unsmoothed periodogram; defaults to .1.
  #
  # fh = The upper bound (in Hertz) of the frequency band from which the peak respiration frequency is identified in the unsmoothed periodogram; defaults to .6.
  #
  #


  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])

    cat("\n\nProcessing: ",basename(in.file),"\n",sep=""); if(!interactive()) flush.console()

    out.file <- file.path(dirname(in.file), gsub(pattern, paste(".",output.label,".gz",sep=""), basename(in.file), fixed = TRUE))

    if(!overwrite & file.exists(out.file)){
      cat("Moving to next file.\n", sep=""); if(!interactive()) flush.console()
      next()
    }

    file.data <- read.data(in.file)
    in.file.info <- import.file.info(in.file)

    if(all(c("time",resp.var,"inh") %in% colnames(file.data))){
      file.data <- file.data[,c("time", resp.var, "inh")]
    }else{
      cat("Input file does not contain the necessary variables; moving to next file.\n", sep=""); if(!interactive()) flush.console()
      next()
    }

    epoch.list <- import.epoch.list(in.file.list = file.path(dirname(in.file), gsub(".phys.gz", ".epoch.txt", basename(in.file), fixed = TRUE)), file.origin = in.file.info$file.origin, event.times = NA, time.vector = NA, report = TRUE, print.epochs = FALSE)


    file.resp <- data.frame()
    cat("Running Resp Summary across epochs:\n", sep=""); if(!interactive()) flush.console()
    for(epoch in 1:nrow(epoch.list)){
      #(epoch <- 1)

      select.vector <- file.data[,"time"] >= epoch.list$start[epoch] & file.data[,"time"] < epoch.list$stop[epoch]
      #select.vector <- rep(FALSE, nrow(file.data)); select.vector[555000:(555000+500*30)] <- TRUE

      if(any(select.vector)){

        resp.length <- diff(range(file.data[select.vector,"time"]))
        inh.period <- diff(which(as.logical(file.data[select.vector,"inh"]))) * 1/in.file.info$fs

        n.inh <- length(inh.period)
        mn.resp.rate <- 1/mean(inh.period)
        md.resp.rate <- 1/median(inh.period)

        resp.spec <- spec.pgram(ts(na.omit(file.data[select.vector,resp.var]), frequency=in.file.info$fs), taper=0, fast=FALSE, detrend=TRUE, plot = FALSE)
        resp.spec$spec <- resp.spec$spec[resp.spec$freq >= fl & resp.spec$freq <= fh]
        resp.spec$freq <- resp.spec$freq[resp.spec$freq >= fl & resp.spec$freq <= fh]
        resp.freq <- resp.spec$freq[which.max(resp.spec$spec)]

        epoch.resp <- data.frame(epoch = epoch.list$name[epoch], resp.length, n.inh, mn.resp.rate, md.resp.rate, resp.freq)

        file.resp <- rbind(file.resp, epoch.resp)

        cat("\tEpoch ",epoch.list$name[epoch],": ",round(resp.length)," sec\n", sep=""); if(!interactive()) flush.console()

        rm(epoch.resp, n.inh, mn.resp.rate, md.resp.rate, resp.freq)

      }else{
        cat("\tEpoch ",epoch.list$name[epoch],": Empty\n", sep=""); if(!interactive()) flush.console()
      }

      rm(select.vector)
    }

    write.data(file.resp, out.file)

    rm(file.data)

  }

}

merge.data <- function(pattern = "", path = file.path(), recursive = TRUE, merged.prefix = "Merged.Data.File", extract.id = TRUE, write.file = TRUE, output.path = getwd(), report = TRUE){

  #
  #
  # Description
  #
  # Merges data files of the specified type into a single csv file.
  #
  #
  # Usage
  #
  # merge.data(pattern, path = file.path(), recursive = TRUE, merged.prefix = "", extract.id = TRUE, write.file = TRUE, output.path = getwd(), report = TRUE)
  #
  #
  # Arguments
  #
  # pattern = The file format to be merged.
  #
  # path = The directory in which to (recursively) search for data files to be merged.
  #
  # merged.prefix = The file prefix for the merged data file (if written); defaults to "Merged.Data.File".
  #
  # extract.id = determines whether the subject identifier is extracted from the first field of the data files and saved in the merged file; defaults to TRUE.
  #
  # write.file = determines whether a merged data file is written; defaults to TRUE.
  #
  # output.path = sets the output directory for the merged data file; defaults to getwd().
  #
  # report = determines whether a data merge report is written to the console; defaults to TRUE.
  #
  #


  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!nchar(pattern)) stop("You must specify a pattern to identify data files.")

  if(!length(path)) path <- tclvalue(tkchooseDirectory(initialdir = getwd(), mustexist = TRUE, title = paste("Select directory in which to search for ",pattern," file(s)", sep="")))

  in.file.list <- list.files(path = path, pattern = pattern, full.names = TRUE, recursive = recursive)

  merged.data <- data.frame()

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])
    file.data <- read.data(in.file, report = report)
    if(extract.id) file.data <- data.frame(id = unlist(strsplit(basename(in.file), "\\."))[1], file.data)
    merged.data <- rbind(merged.data, file.data)
    rm(file.data)
  }

  if(report){cat("\n", length(in.file.list), " files merged.\n\n", sep=""); if(!interactive()) flush.console()}

  rownames(merged.data) <- 1:nrow(merged.data)

  if(write.file){
    out.file <- paste(merged.prefix,".", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv", sep = "")
    write.data(merged.data, out.file, compressed = FALSE)
  }else{
    return(merged.data)
  }

}

MSHeart.file.processing <- function(in.file.list = c(), pattern = ".5FS", processing.mode = "interactive", path = file.path(), recursive = TRUE, overwrite = FALSE){
  #
  #
  # Description
  #
  # Identifies inhalations (via file annotation) in submitted respiration waveforms.
  #
  #
  # Usage
  #
  # process.resp(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, overwrite = FALSE, save.fresp = FALSE, filter.width = 10, fl = .05, fh = .5, min.period = 2)
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


  # if(!interactive()) Sys.sleep(3)

  # rm(list=ls())

  #   in.file.list = c(); pattern = ".5FS"; processing.mode = "interactive"; path = file.path(); recursive = TRUE; overwrite = FALSE


  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])

    cat("\n\nProcessing: ",basename(in.file),"\n",sep=""); if(!interactive()) flush.console()

    #     cat("\nSelect the subejct's 5FS data file you copied from the memory card.\n", sep=""); if(!interactive()) flush.console()


    ###  Select .5FS file
    # in.file <- file.choose()
    # (in.file <- "/Users/christieic/Dropbox/MsHeart/MHP006/MHP006_0.5FS")


    file.check <- TRUE



    ### Create necessary in.files and check that they exist

    evt.in.file <- file.path(dirname(in.file), gsub(".5FS", "_EVT.txt", basename(in.file)))
    scl.in.file <- file.path(dirname(in.file), gsub(".5FS", "_SCL.txt", basename(in.file)))
    # actiwatch?


    if(!substr(basename(in.file), nchar(basename(in.file)) - 3, nchar(basename(in.file))) == ".5FS"){
      file.check <- FALSE
      stop("\nYou did not select a 5FS file.", sep=""); if(!interactive()) flush.console()
    }

    if(!file.exists(evt.in.file)){
      file.check <- FALSE
      cat("\nYou have not yet created the EVT file.", sep=""); if(!interactive()) flush.console()
    }

    if(!file.exists(scl.in.file)){
      file.check <- FALSE
      cat("\nYou have not yet created the SCL file.", sep=""); if(!interactive()) flush.console()
    }





    if(!file.check){

      cat("\n\nPlease remedy the problem listed above and then try running MSHeart.file.processing again.", sep=""); if(!interactive()) flush.console()

      cat("\n\nFile processing aborted.\n", sep=""); if(!interactive()) flush.console()

    }else{



      ### Read SCL file

      cat("\nReading file: ", basename(scl.in.file), sep=""); if(!interactive()) flush.console()
      scl.date <- read.table(scl.in.file, header = FALSE, sep = " ", skip = 1, nrows = 1)[,]

      if(read.table(scl.in.file, header = FALSE, sep = " ", skip = 2, nrows = 1)[1] == "Subject"){
        scl.data <- read.table(scl.in.file, header = FALSE, sep = " ", skip = 3)[,1:2]
      }else{
        scl.data <- read.table(scl.in.file, header = FALSE, sep = " ", skip = 2)[,1:2]
      }

      cat(" ... done\n", sep=""); if(!interactive()) flush.console()
      # scl.data <- scl.data[1:100,]


      ### Read EVT file

      cat("\nReading file: ", basename(evt.in.file), sep=""); if(!interactive()) flush.console()

      in.file.data <- read.table(evt.in.file, header = FALSE, sep = "\t", skip = 0)

      cat(" ... done\n", sep=""); if(!interactive()) flush.console()

      in.file.evt <- in.file.data[in.file.data[,1] == "Time" & grepl("/", in.file.data[,2]), ]
      in.file.msg <- in.file.data[in.file.data[,1] == "Message", ]
      in.file.evt <- in.file.evt[grepl("Button pressed", in.file.msg[,2]), ]



      ### Launch GUI to check (and possibly correct) end time
      end.date <- format(as.POSIXct(as.numeric(as.POSIXct(scl.date, origin = "1970-01-01", tz = "GMT", format = "%d-%m-%Y/%H:%M:%S")) + nrow(scl.data)*100/1000, origin = "1970-01-01", tz = "GMT"), format = "%d-%m-%Y/%H:%M:%S", tz = "GMT", usetz = FALSE)

      tt.date <- tktoplevel()
      tktitle(tt.date)  <- "End time correction"
      tcl.end.date <- tclVar(end.date)
      gui.done <- tclVar(0)
      entry.date <- tkentry(tt.date, width = "18", textvariable = tcl.end.date)
      tkgrid(tklabel(tt.date, text = "Make any necessary corrections to the end date & time (format DD-MM-YYYY/HH:MM:SS) and press okay."))
      tkgrid(entry.date)
      OnOK <- function() tclvalue(gui.done) <- 1
      OK.but <- tkbutton(tt.date, text = " OK ", command = OnOK)
      tkbind(entry.date, "<Return>", OnOK)
      tkgrid(OK.but)
      tkfocus(tt.date)
      tkwait.variable(gui.done)
      tkdestroy(tt.date)

      if(!tclvalue(tcl.end.date) == end.date){
        cat("Trimming to: ", end.date, "\n", sep=""); if(!interactive()) flush.console()
        end.date <- tclvalue(tcl.end.date)
        target.length <- as.numeric(as.POSIXct(end.date, tz = "GMT", format = "%d-%m-%Y/%H:%M:%S", origin = "1970-01-01")) - as.numeric(as.POSIXct(scl.date, tz = "GMT", format = "%d-%m-%Y/%H:%M:%S", origin = "1970-01-01"))
        scl.data <- scl.data[scl.data[,1] <= target.length*1000, ]
      }


      ### Write SCL_clean file
      scl.out.file <- file.path(dirname(scl.in.file), gsub("_SCL.txt", "_SCL_clean.txt", basename(scl.in.file)))

      cat("Writing file: ", basename(scl.out.file), sep=""); if(!interactive()) flush.console()

      write.table(scl.date, file = scl.out.file, append = FALSE, quote = FALSE, sep = " ",
                  eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                  col.names = FALSE, qmethod = c("escape", "double"))

      write.table(scl.data[,2], file = scl.out.file, append = TRUE, quote = FALSE, sep = " ",
                  eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                  col.names = FALSE, qmethod = c("escape", "double"))

      cat(" ... done\n", sep=""); if(!interactive()) flush.console()






      ### Write clean EVT file

      evt.out.file <- file.path(dirname(evt.in.file), gsub("_EVT.txt", "_EVT_clean.txt", basename(evt.in.file)))

      cat("Writing file: ", basename(evt.out.file), sep=""); if(!interactive()) flush.console()

      write.table(gsub(": ", "", in.file.evt[,2]), file = evt.out.file, append = FALSE, quote = FALSE, sep = " ",
                  eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                  col.names = FALSE, qmethod = c("escape", "double"))

      cat(" ... done\n", sep=""); if(!interactive()) flush.console()




      ### Summary File

      signal.list <- c("ECG","ICG","SCL")
      outage.list <- vector("list", length(signal.list)); names(outage.list) <- signal.list
      signal.summary <- c("number","minimum","maximum","total","percent","percent_24h")
      signal.summary <- as.data.frame(matrix(NA, nrow = length(signal.list), ncol = length(signal.summary), dimnames = list(signal.list, signal.summary)))

      for(signal in rownames(signal.summary)){
        # (signal <- rownames(signal.summary)[3])

        signal.out <- sapply(in.file.data[grep(paste(signal," out",sep=""), in.file.data[,2]) - 3, 2], function(x){as.numeric(as.POSIXct(sub(": ","",x), tz = "GMT", format = "%d-%m-%Y/%H:%M:%S"))})
        signal.in <- as.numeric(sapply(in.file.data[grep(paste(signal," in",sep=""), in.file.data[,2]) - 3, 2], function(x){as.numeric(as.POSIXct(sub(": ","",x), tz = "GMT", format = "%d-%m-%Y/%H:%M:%S"))}))
        # signal.out; signal.in

        if(length(signal.out) == 0){
          signal.summary[signal,] <- 0
          outage.list[[signal]] <- data.frame()
          next
        }

        #     if(length(signal.in) == 0){
        #       signal.in <- as.numeric(as.POSIXct(scl.date, tz = "GMT", format = "%d-%m-%Y/%H:%M:%S")) + nrow(scl.data)*100/1000
        #     }

        if(length(signal.in) < length(signal.out)) signal.in <- c(signal.in, as.numeric(as.POSIXct(scl.date, tz = "GMT", format = "%d-%m-%Y/%H:%M:%S")) + nrow(scl.data)*100/1000)

        # data.frame(signal.out, signal.in, duration = (signal.in - signal.out))

        signal.summary[signal,"number"] <- length(signal.out)
        signal.summary[signal,"minimum"] <- round(min(signal.in - signal.out) / 60, 1)
        signal.summary[signal,"maximum"] <- round(max(signal.in - signal.out) / 60, 1)
        signal.summary[signal,"total"] <- round(sum(signal.in - signal.out) /  60, 1)
        signal.summary[signal,"percent"] <- round(sum(signal.in - signal.out) / (nrow(scl.data)*100/1000) * 100, 1)
        signal.summary[signal,"percent_24h"] <- round(sum(signal.in - signal.out) / (24*60*60) * 100, 1)

        outage.list[[signal]] <- data.frame(time = sub(": ", "", names(signal.out)), duration = round((signal.in - signal.out) / 60, 1), row.names = 1:length(signal.out))

      }

      summary.out.file <- file.path(dirname(evt.in.file), gsub("_EVT.txt", "_Summary.txt", basename(evt.in.file)))

      cat("\nWriting file: ", basename(summary.out.file), sep=""); if(!interactive()) flush.console()

      cat("Total length of recording: ", round(nrow(scl.data)*100/1000/60/60, 2), " hours (", round(100*nrow(scl.data)*100/1000/60/60/24, 1), "% of 24 hours)\n\n\nSignal outage summary (in number of outages, duration in minutes,\npercent of recording time, and percent 24 hour period):\n\n", sep = "", file = summary.out.file, append = FALSE)
      capture.output(signal.summary, file = summary.out.file, append = TRUE)

      cat("\n\n\nAll recorded outages (duration in minutes):\n", sep = "", file = summary.out.file, append = TRUE)

      for(signal in rownames(signal.summary)){
        # (signal <- rownames(signal.summary)[3])
        cat("\n\n", signal, "\n", sep = "", file = summary.out.file, append = TRUE)
        if(nrow(outage.list[[signal]])){
          capture.output(outage.list[[signal]], file = summary.out.file, append = TRUE)
        }else{
          cat("no outages\n", sep = "", file = summary.out.file, append = TRUE)
        }
      }

      cat(" ... done\n", sep=""); if(!interactive()) flush.console()

      cat("\nFile processing completed.\n\n", sep=""); if(!interactive()) flush.console()

    }


    # rm(list=ls())

  }

}

process.ecg <- function(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, overwrite = FALSE, save.fecg = FALSE, check.ecg = FALSE, filter.width = .10, fl = 5, fh = 20, search = 10, threshold = 7, safe = 300){
  # process.ecg <- function(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, overwrite = FALSE, save.fecg = FALSE, check.ecg = FALSE, width = round(in.file.info$fs/10)+1, fl = 5, fh = 20, search = 10, threshold = 7, safe = 300){


  #
  #
  # Description
  #
  # Identifies R-Spikes (via file annotation) in submitted ECG.
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

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in in.file.list){
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

process.resp <- function(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, overwrite = FALSE, save.fresp = FALSE, filter.width = 10, fl = .05, fh = .5, min.period = 2){


  #
  #
  # Description
  #
  # Identifies inhalations (via file annotation) in submitted respiration waveforms.
  #
  #
  # Usage
  #
  # process.resp(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, overwrite = FALSE, save.fresp = FALSE, filter.width = 10, fl = .05, fh = .5, min.period = 2)
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
  # save.fresp = Determines whether the filtered ECG used by the QRS detection algorithm is saved in the data file along with the location of detected R-spikes; defaults to FALSE. This can considerably increas the size of data files and should only be necessary if the raw ECG is noisy.
  #
  # filter.width = the width (in seconds) of the window used to bandpass filter the raw respiration waveform; defaults to .10.
  #
  # fl = The lower cutoff frequency (in Hertz) used to bandpass filter the raw respiration waveform; defaults to .05.
  #
  # fh = The higher cutoff frequency (in Hertz) used to bandpass filter the raw respiration waveform; defaults to .5.
  #
  # min.period = QWERTY; defaults to 2.
  #
  # threshold = Determines the sensitivity of the R-spike detection. Larger numbers decrease sensitivity; defaults to 7.
  #
  # safe = Time (in milliseconds) R-spike detector is turned off following detection of an R-spike (prevents false detection of peaky T-waves); defaults to 300.
  #
  #


  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])

    cat("\n\nProcessing: ",basename(in.file),"\n",sep=""); if(!interactive()) flush.console()

    out.file <- in.file

    if(!overwrite && any(c("inh","exh") %in% read.vars(in.file))){
      cat("Processing halted to avoid overwrite.\n", sep=""); if(!interactive()) flush.console()
      next()
    }

    in.file.info <- import.file.info(in.file)

    file.data <- read.data(in.file)

    width <- filter.width * in.file.info$fs; if(as.integer(width %% 2 != 1)) width <- width + 1

    file.data$fresp <- as.vector(filter(file.data$resp, bpcoef(width, in.file.info$fs, fl, fh), circular = FALSE))

    # period = 1 / f = 1 / .6 = 1.6667
    span <- as.integer(min.period * in.file.info$fs / 2)

    inh <- peaks(file.data$fresp, span)
    exh <- peaks(-file.data$fresp, span)
    breath.check <- rbind(data.frame(i = inh, x = 1), data.frame(i = exh, x = -1))
    breath.check <- breath.check[order(breath.check$i),]
    while(any(rle(breath.check$x)$lengths > 1)){
      i <- min(which(rle(breath.check$x)$lengths > 1))
      if(breath.check$x[i] > 0) drop.i <- c(i,i+1)[which.min(file.data$fresp[breath.check$i[c(i,i+1)]])]
      if(breath.check$x[i] < 0) drop.i <- c(i,i+1)[which.max(file.data$fresp[breath.check$i[c(i,i+1)]])]
      breath.check <- breath.check[-drop.i,]
    }

    if(!save.fresp && any(which(colnames(file.data) == "fresp"))) file.data <- file.data[,-which(colnames(file.data) == "fresp")]

    file.data$inh <- 1:nrow(file.data) %in% inh
    file.data$exh <- 1:nrow(file.data) %in% exh

    write.data(file.data, out.file)

  }
}

review.ecg <- function(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, y.var = "ecg", id.var = "r", pch = NA, width = 10, edge.delta.1 = 10, edge.delta.2 = 2, width.delta = 10){

  #
  #
  # Description
  #
  # Loads ECG data into DataViewer for review and editting of annotated R-spikes.
  #
  #
  # Usage
  #
  # review.ecg(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, y.var = "ecg", id.var = "r", pch = NA, width = 10, edge.delta.1 = 10, edge.delta.2 = 2, width.delta = 10)
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
  # y.var = Identifies the the variable plotted on the y-axis in the data viewer (the x-axis is always "time"); defaults to "ecg". If you saved the filtered ECG in the process.ecg step and wish to view that instead, use "fecg" rather than "ecg".
  #
  # id.var = Identifies the annotation variable in the data file; defaults to "r" for R-spikes previously labelled by the process.ecg function.
  #
  # pch = Specifies an alternative plotting character; defaults to NA. Just ignore this. It isn't terribly useful.
  #
  # width = The starting width, in seconds, of the data viewer plot window; defaults to 10.
  #
  # edge.delta.1 = The starting value, in seconds, for the first of two data viewer plot window edge adjustment increments; defaults to 10.
  #
  # edge.delta.2 = The starting value, in seconds, for the second of two data viewer plot window edge adjustment increments; defaults to 2.
  #
  # width.delta = The starting value, in seconds, for the data viewer plot window width adjustment increment; defaults to 10.
  #
  #



  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])

    cat("\n\nProcessing: ",basename(gsub(".ecg.gz", "", in.file)),"\n",sep=""); if(!interactive()) flush.console()

    in.file.info <- import.file.info(in.file)

    data.viewer(in.file = in.file, x.var = "time", y.var = y.var, id.var = id.var, label.mode = c("all","max","min")[2], pch = pch, origin = in.file.info$file.origin, width = width, edge.delta.1 = edge.delta.1, edge.delta.2 = edge.delta.2, width.delta = width.delta)

    # 		file.data <- read.data(in.file)
    #
    # 		temp <- data.viewer(file.data, x.var = "time", y.var = y.var, id.var = id.var, label.mode = c("all","max","min")[2], pch = pch, origin = in.file.info$file.origin, width = width, edge.delta.1 = edge.delta.1, edge.delta.2 = edge.delta.2, width.delta = width.delta)
    #
    # 		if(!all(file.data == temp | is.na(temp))){
    # 			if(confirm.file.replace(in.file)){
    # 				file.data <- temp; rm(temp)
    # 				out.file <- in.file
    # 				write.data(file.data, out.file)
    #       }else{
    # 				rm(temp)
    # 			}
    # 		}

  }

}

review.ibi <- function(in.file.list = c(), pattern = ".ibi.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, y.var = "ibi", id.var = "artifact", pch = 20, id.pch = 20, ymin = 400, ymax = 1800, width = 180, edge.delta.1 = 180, edge.delta.2 = 60, width.delta = 60){


  #
  #
  # Description
  #
  # Loads IBI data into DataViewer for review and editting of annotated artifact.
  #
  #
  # Usage
  #
  # review.ibi(in.file.list = c(), pattern = ".ibi.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, pch = 20, width = 180, edge.delta.1 = 180, edge.delta.2 = 60, width.delta = 60)
  #
  #
  # Arguments
  #
  # in.file.list = List of files to be run through the function. The default, an empty vector "c()", triggers a graphical file or directory selector.
  #
  # pattern = The typical input file format; defaults to ".ibi.gz".
  #
  # processing.mode = Determines the mode of file selection if in.file.list is empty. Valid values are "interactive" and "batch"; defaults to "interactive".
  #
  # path = The starting directory for the graphical file or directory selector. Defaults to file.path(), an empty file path.
  #
  # recursive = When operating in batch mode, determines whether files nested in folders are searched for the pattern file type; defaults to TRUE.
  #
  # y.var = Identifies the the variable plotted on the y-axis in the data viewer (the x-axis is always "time"); defaults to "ibi".
  #
  # id.var = Identifies the annotation variable in the data file; defaults to "artifact" for IBI previously labelled by the artifact.ibi function.
  #
  # pch = Specifies an alternative plotting character; defaults to 20. Just ignore this. It isn't terribly useful.
  #
  # width = The starting width, in seconds, of the data viewer plot window; defaults to 10.
  #
  # edge.delta.1 = The starting value, in seconds, for the first of two data viewer plot window edge adjustment increments; defaults to 10.
  #
  # edge.delta.2 = The starting value, in seconds, for the second of two data viewer plot window edge adjustment increments; defaults to 2.
  #
  # width.delta = The starting value, in seconds, for the data viewer plot window width adjustment increment; defaults to 10.
  #
  #


  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])

    cat("\n\nProcessing: ",basename(gsub(".ibi.gz", "", in.file)),"\n",sep=""); if(!interactive()) flush.console()

    in.file.info <- import.file.info(in.file)

    data.viewer(in.file = in.file, x.var = "time", y.var = y.var, id.var = id.var, label.mode = c("all","max","min")[1], pch = pch, id.pch = id.pch, origin = in.file.info$file.origin, ymin = ymin, ymax = ymax, width = width, edge.delta.1 = edge.delta.1, edge.delta.2 = edge.delta.2, width.delta = width.delta)

  }


}

review.resp <- function(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, y.var = "resp", id.var = "inh", pch = NA, width = 180, edge.delta.1 = 180, edge.delta.2 = 60, width.delta = 10){


  #
  #
  # Description
  #
  # Loads Resp data into DataViewer for review and editting of annotated inhalations.
  #
  #
  # Usage
  #
  # review.resp(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, y.var = "resp", id.var = "inh", pch = NA, width = 180, edge.delta.1 = 180, edge.delta.2 = 60, width.delta = 10)
  #
  #
  # Arguments
  #
  # in.file.list = List of files to be run through the function. The default, an empty vector "c()", triggers a graphical file or directory selector.
  #
  # pattern = The typical input file format; defaults to ".phys.gz".
  #
  # processing.mode = Determines the mode of file selection if in.file.list is empty. Valid values are "interactive" and "batch"; defaults to "interactive".
  #
  # path = The starting directory for the graphical file or directory selector. Defaults to file.path(), an empty file path.
  #
  # recursive = When operating in batch mode, determines whether files nested in folders are searched for the pattern file type; defaults to TRUE.
  #
  # y.var = Identifies the the variable plotted on the y-axis in the data viewer (the x-axis is always "time"); defaults to "resp". If you saved the filtered respiration in the process.resp step and wish to view that instead, use "fresp" rather than "resp".
  #
  # id.var = Identifies the annotation variable in the data file; defaults to "inh" for inhalations previously labelled by the process.resp function.
  #
  # pch = Specifies an alternative plotting character; defaults to NA. Just ignore this. It isn't terribly useful.
  #
  # width = The starting width, in seconds, of the data viewer plot window; defaults to 180.
  #
  # edge.delta.1 = The starting value, in seconds, for the first of two data viewer plot window edge adjustment increments; defaults to 180.
  #
  # edge.delta.2 = The starting value, in seconds, for the second of two data viewer plot window edge adjustment increments; defaults to 60.
  #
  # width.delta = The starting value, in seconds, for the data viewer plot window width adjustment increment; defaults to 10.
  #
  #


  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])

    cat("\n\nProcessing: ",basename(in.file),"\n",sep=""); if(!interactive()) flush.console()

    in.file.info <- import.file.info(in.file)

    data.viewer(in.file = in.file, x.var = "time", y.var = y.var, id.var = id.var, label.mode = c("all","max","min")[2], pch = pch, origin = in.file.info$file.origin, width = width, edge.delta.1 = edge.delta.1, edge.delta.2 = edge.delta.2, width.delta = width.delta)

    # 		file.data <- read.data(in.file)
    #
    # 		temp <- data.viewer(file.data, x.var = "time", y.var = y.var, id.var = id.var, label.mode = c("all","max","min")[2], pch = pch, origin = in.file.info$file.origin, width = width, edge.delta.1 = edge.delta.1, edge.delta.2 = edge.delta.2, width.delta = width.delta)
    #
    # 		if(!all(file.data == temp | is.na(temp))){
    # 			if(confirm.file.replace(in.file)){
    # 				file.data <- temp; rm(temp)
    # 				out.file <- in.file
    # 				write.data(file.data, out.file)
    #       }else{
    # 				rm(temp)
    # 			}
    # 		}

  }

}

