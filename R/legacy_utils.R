amsibi.to.gz <- function(in.file.list = c(), pattern = ".IBI.txt",
                         processing.mode = "interactive", path = file.path(),
                         recursive = TRUE, overwrite = FALSE){

  #in.file.list = c(); pattern = ".IBI.txt"; processing.mode = "interactive"; path = file.path(); recursive = TRUE; overwrite = FALSE; separate.files = FALSE

  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])

    cat("\n\nProcessing: ",basename(in.file),"\n",sep=""); if(!interactive()) flush.console()

    # 		if(length(column.names)) { col.names <- column.names }else{ stop("You must specify column names when converting this file type.") }

    out.file <- file.path(dirname(in.file), gsub(pattern, ".info.txt", basename(in.file), fixed = TRUE))
    if(!overwrite & file.exists(out.file)){
      cat("Moving to next file.\n", sep=""); if(!interactive()) flush.console()
      next()
    }


    date.time <- read.table(in.file, nrows = 1, header = FALSE, skip = 0, sep = "\t", fill = TRUE, as.is = TRUE, strip.white = TRUE, blank.lines.skip = TRUE)[,1]
    #file.origin <- strptime(unlist(strsplit(date.time,"/"))[1], "%d-%m-%y")
    file.origin <- strptime(substr(date.time,1,8), "%d-%m-%y")

    file.data <- read.table(in.file, header = FALSE, skip = 1, sep = "\t", fill = TRUE, as.is = TRUE, strip.white = TRUE, blank.lines.skip = TRUE)[, c(2,3)]
    colnames(file.data) <- c("time","ibi")
    file.data[,"time"] <- file.data[,"time"] / 1000

    file.outage <- file.data[is.na(file.data[,"ibi"]),]
    file.data <- file.data[!is.na(file.data[,"ibi"]),]

    file.data[,1] <- file.data[,1] + as.numeric(ISOdatetime("1970","01","01", substr(date.time,10,11), substr(date.time,13,14), substr(date.time,16,21), tz = "GMT"))

    cat("Recording duration: ",round(diff(range(file.data[,"time"]))/60)," minutes (",round(diff(range(file.data[,"time"]))/60/60,2)," hours)\n",sep=""); if(!interactive()) flush.console()
    cat("Writing: ",basename(out.file),"\n",sep=""); if(!interactive()) flush.console()
    write.table(data.frame(file.origin), file = out.file, append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"))

    out.file <- file.path(dirname(in.file), gsub(pattern, ".ibi.gz", basename(in.file), fixed = TRUE))
    write.data(file.data, out.file)
  }

}

bpcoef <- function(m,fs,fl,fh,type="hamming") {
  h <- bscoef(m,fs,fl,fh,type=type)
  h <- -h
  h[m/2+1] <- h[m/2+1]+1
  h
}


bscoef <- function(m,fs,fl,fh,type="hamming") {
  hl <- lpcoef(m,fs,fl,type=type)
  hh <- hpcoef(m,fs,fh,type=type)
  h <- hl + hh
  h
}

build.in.file.list <- function(pattern, processing.mode = "interactive", multiple = TRUE,
                               path = file.path(), recursive = TRUE) {

  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  in.file.list <- c()

  if(processing.mode == "batch"){
    if(!length(path)) path <- tclvalue(tkchooseDirectory(initialdir = getwd(), mustexist = TRUE, title = paste("Select directory in which to search for ",pattern," file(s)", sep="")))
    if(!length(path)) stop("No search directory specified.")
    in.file.list <- list.files(path = path, pattern = pattern, full.names = TRUE, recursive = recursive)
  }


  if(processing.mode == "interactive"){
    in.file.list <- tclvalue(tkgetOpenFile(initialdir = ifelse(length(path),path,getwd()), multiple = multiple, title = paste("Select ",pattern," file(s)", sep=""), filetypes = paste("{{",pattern," files} {",pattern,"}} {{All files} {*}}", sep="")))
    in.file.list <- if(.Platform$OS.type == "unix"){ unlist(strsplit(in.file.list, " ", fixed = TRUE)) }else{ unlist(strsplit(in.file.list, "} {", fixed = TRUE)) }
    in.file.list <- gsub("{", "", in.file.list, fixed = TRUE); in.file.list <- gsub("}", "", in.file.list, fixed = TRUE)
  }

  if(!length(in.file.list)) stop("No input file(s) supplied")

  return(in.file.list)

}

check.inverted <- function(x, y) {
  require(tcltk)
  #   plot(x[1:(5*fs)], y[1:(s*fs)], type="l")
  plot(x, y, type="l")
  tt <- tktoplevel()
  tkwm.title(tt, "Check waveform direction.")
  done <- tclVar(0)
  yes.but <- tkbutton(tt, text="YES", command=function() tclvalue(done)<-1)
  no.but <- tkbutton(tt, text="NO", command=function() tclvalue(done)<-2)
  tkgrid(tklabel(tt, text="Is the waveform inverted?"), columnspan=2)
  tkgrid(tklabel(tt, text=""), columnspan=2)
  tkgrid(yes.but, row=3, column=0, sticky="e")
  tkgrid(no.but, row=3, column=1, sticky="w")
  tkgrid(tklabel(tt, text=""), columnspan=2)
  tkbind(tt,"<Destroy>", function() tclvalue(done)<-2)
  tkbind(tt,"<y>", function() tclvalue(done)<-1)
  tkbind(tt,"<n>", function() tclvalue(done)<-2)
  tkfocus(tt)
  tkwait.variable(done)
  done.val <- as.integer(tclvalue(done))
  tkdestroy(tt)
  dev.off()
  if(done.val == 1){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

confirm.file.replace <- function(filename){
  if(file.exists(filename)){
    require(tcltk)
    tt <- tktoplevel()
    tkwm.title(tt, "Confirm Possible File Replacement")
    done <- tclVar(0)
    OK.but <- tkbutton(tt, text="YES", command=function() tclvalue(done)<-1)
    Cancel.but <- tkbutton(tt, text="NO", command=function() tclvalue(done)<-2)
    tkgrid(tklabel(tt, text=paste(basename(filename), "\nalready exists and could be replaced.", sep="")), columnspan=2)
    tkgrid(tklabel(tt, text="Do you want to proceed?"), columnspan=2)
    tkgrid(tklabel(tt, text=""), columnspan=2)
    tkgrid(OK.but, row=3, column=0, sticky="e")
    tkgrid(Cancel.but, row=3, column=1, sticky="w")
    tkgrid(tklabel(tt, text=""), columnspan=2)
    tkbind(tt,"<Destroy>", function() tclvalue(done)<-2)
    tkbind(tt,"<y>", function() tclvalue(done)<-1)
    tkbind(tt,"<n>", function() tclvalue(done)<-2)
    tkfocus(tt)
    tkwait.variable(done)
    doneVal <- as.integer(tclvalue(done))
    tkdestroy(tt)
    if(doneVal==1){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }else{
    return(TRUE)
  }
}

dat.to.gz <- function(in.file.list = c(), pattern = ".DAT", processing.mode = "interactive",
                      path = file.path(), recursive = TRUE, overwrite = FALSE,
                      column.names = c(), fs = NA, length = NA){

  # in.file.list = c(); pattern = ".DAT"; processing.mode = "interactive"; path = file.path(); recursive = TRUE; overwrite = TRUE; column.names = c("time","ecg","resp"); fs = 1000; length = 600

  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])

    cat("\n\nProcessing: ",basename(in.file),"\n",sep=""); if(!interactive()) flush.console()

    if(is.na(fs)) stop("You must specify sampling frequency (fs) when converting this file type.")
    if(is.na(length)) stop("You must specify maximum record length when converting this file type.")

    if(length(column.names)) { col.names <- column.names }else{ stop("You must specify column names when converting this file type.") }

    in.con <- file(in.file, "rb")
    file.data <- readBin(con = in.con, what = integer(), size = 2, n = fs*length, signed = FALSE, endian = "little") / 16 - 2048
    close(in.con)

    file.data <- matrix(file.data, ncol = 2, byrow = TRUE, dimnames = NULL)
    file.data <- cbind(seq(0, by = 1/fs, length.out = nrow(file.data)), file.data)
    colnames(file.data) <- column.names

    # file.origin <- ISOdatetime(unlist(strsplit(date.time[1,1],"/"))[3], unlist(strsplit(date.time[1,1],"/"))[1], as.numeric(unlist(strsplit(date.time[1,1],"/"))[2]), 0, 0, 0, tz = "GMT")
    file.origin <- NA

    out.file <- file.path(dirname(in.file), gsub(pattern, ".info.txt", basename(in.file), fixed = TRUE))
    if(!overwrite && file.exists(out.file)){
      cat("Moving to next file.\n", sep=""); if(!interactive()) flush.console()
      next()
    }

    cat("Recording duration: ",round(diff(range(file.data[,"time"]))/60, 1)," minutes\n",sep=""); if(!interactive()) flush.console()
    cat("Writing: ",basename(out.file),"\n",sep=""); if(!interactive()) flush.console()
    write.table(data.frame(file.origin, fs), file = out.file, append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"))

    out.file <- file.path(dirname(in.file), gsub(pattern, ".phys.gz", basename(in.file), fixed = TRUE))
    write.data(file.data, out.file)

  }

}

derivative.artifact <- function(x, x.t = c(), x.art = rep(FALSE,length(x)), threshold = c(.20,.20), limits = c(400,1800), safe = c(50,50), width = 11, frs = 10, n.iterations = 1, artifact.plot = FALSE){

  options(stringsAsFactors = FALSE)

  if(!length(x.t)) x.t <- 1:length(x)
  if(length(threshold) == 1) threshold <- c(threshold,threshold)
  if(length(safe) == 1) safe <- c(safe,safe)

  d.x <- c(0,diff(x))

  for(i in 1:n.iterations){

    start.art <- x.art

    if(length(threshold)){

      if(!is.na(threshold[1])){
        if(threshold[1] < 1){
          t.l <- -c(0,(x*threshold[1])[-length(x)])
          x.art[d.x < 0] <- x.art[d.x < 0] | d.x[d.x < 0] < t.l[d.x < 0]
        }else{
          t.l <-  rep(-threshold[1],length(x))
          x.art[d.x < 0] <- x.art[d.x < 0] | d.x[d.x < 0] < t.l[d.x < 0]
        }
      }

      if(!is.na(threshold[2])){
        if(threshold[2] < 1){
          t.u <- c(0,(x*threshold[2])[-length(x)])
          x.art[d.x > 0] <- x.art[d.x > 0] | d.x[d.x > 0] > t.u[d.x > 0]
        }else{
          t.u <-  rep(threshold[2],length(x))
          x.art[d.x > 0] <- x.art[d.x > 0] | d.x[d.x > 0] > t.u[d.x > 0]
        }
      }

    }

    if(length(limits)){
      if(!is.na(limits[1])) x.art <- x.art | x < limits[1]
      if(!is.na(limits[2])) x.art <- x.art | x > limits[2]
    }

    if(length(safe)){
      x.safe <- rep(FALSE,length(x))
      rs.x.t <- seq(min(x.t[!x.art]), max(x.t[!x.art]), 1/frs)
      rs.x <- approx(x.t[!x.art], x[!x.art], rs.x.t, rule=2, method="linear")$y
      rs.width <- ifelse((width*frs)/2 == floor((width*frs)/2), (width*frs)+1, (width*frs))
      m.x <- approx(rs.x.t, as.vector(filter(rs.x, rep(1/rs.width, rs.width), circular = FALSE)), x.t, rule=2, method="linear")$y

      if(!is.na(safe[1])){
        if(safe[1] < 1){
          s.l <- m.x - abs(m.x*safe[1])
        }else{
          s.l <- m.x - safe[1]
        }
      }else{
        s.l <- m.x
      }

      if(!is.na(safe[2])){
        if(safe[1] < 1){
          s.u <- m.x + abs(m.x*safe[2])
        }else{
          s.u <- m.x + safe[2]
        }
      }else{
        s.u <- m.x
      }

      x.safe[x.art & x > s.l & x < s.u] <- TRUE
      x.art[x > s.l & x < s.u] <- FALSE
    }

    if(all(x.art == start.art)) break

  }

  if(artifact.plot){
    dev.new(width=600,height=400); split.screen(c(3,1))
    screen(1); par(mar=c(1,3,1,.25))
    plot(x.t, x, ylim=range(x), type="o", axes=FALSE, main="", xlab="", ylab=""); axis(2)
    points(x.t[x.art], x[x.art], pch=20, bg="red", col="red")
    if(length(safe)) { points(x.t[x.safe], x[x.safe], pch=20, bg="green", col="green"); lines(x.t, s.l, lty=1, lwd=1, col="black"); lines(x.t, s.u, lty=1, lwd=1, col="black") }
    screen(2); par(mar=c(1,3,1,.25))
    plot(x.t[!x.art], x[!x.art], ylim=range(x), type="o", axes=FALSE, main="", xlab="", ylab=""); axis(2)
    screen(3); par(mar=c(1,3,1,.25))
    plot(x.t, d.x, type="o", axes=FALSE, main="", xlab="", ylab=""); axis(2)
    points(x.t[x.art], d.x[x.art], pch=20, bg="red", col="red")
    if(length(threshold)) { points(x.t[x.safe], d.x[x.safe], pch=20, bg="green", col="green"); lines(x.t, t.l, lty=1, lwd=1, col="black"); lines(x.t, t.u, lty=1, lwd=1, col="black") }
    close.screen(all = TRUE)
  }

  return(x.art)

}

gui <- function(clear.prior = TRUE){

  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  make.fun.call <- function(fun.name){
    fun.call <- ""; for(x in deparse(args(fun.name))) fun.call <- paste(fun.call, x, sep = "")
    fun.call <- sub("function ",fun.name, fun.call)
    fun.call <- sub(" NULL","", fun.call)
    while(grepl("  ", fun.call)) fun.call <- sub("  ","", fun.call)
    return(fun.call)
  }

  insert.txt <- function(fun.name){
    if(clear.prior) tkdelete(txt, "0.0", "end")
    tkinsert(txt, "end", "\n")
    tkinsert(txt, "end", make.fun.call(fun.name))
    tkinsert(txt, "end", "\n")
  }

  eval.txt <- function() eval(parse(text = tclvalue(tkget(txt, "0.0", "end"))))

  tt  <- tktoplevel()
  tktitle(tt) <- "PhysioScripts"
  xscr <- tkscrollbar(tt, repeatinterval = 5, orient = "horizontal", command=function(...)tkxview(txt,...))
  yscr <- tkscrollbar(tt, repeatinterval = 5, command = function(...)tkyview(txt,...))
  txt <- tktext(tt, width=100, height=5, bg="white", font="courier", xscrollcommand = function(...)tkset(xscr,...), yscrollcommand = function(...)tkset(yscr,...), wrap = "none")
  tkgrid(txt, yscr, columnspan = 3); tkgrid(xscr, columnspan = 3)
  tkgrid.configure(yscr, sticky = "ns"); tkgrid.configure(xscr, sticky = "ew")

  submit.but <- tkbutton(tt, text="Submit", command=eval.txt)
  clear.but <- tkbutton(tt, text="Clear", command = function() tkdelete(txt, "0.0", "end"))
  quit.but <- tkbutton(tt, text="Quit", command = function() tkdestroy(tt))

  tkgrid(submit.but, clear.but, quit.but)
  tkgrid.configure(submit.but, sticky = "e"); tkgrid.configure(quit.but, sticky = "w")

  guiMenu <- tkmenu(tt)
  tkconfigure(tt, menu = guiMenu)

  convertMenu <- tkmenu(guiMenu, tearoff = FALSE)
  processMenu <- tkmenu(guiMenu, tearoff = FALSE)
  artifactMenu <- tkmenu(guiMenu, tearoff = FALSE)
  reviewMenu <- tkmenu(guiMenu, tearoff = FALSE)
  extractMenu <- tkmenu(guiMenu, tearoff = FALSE)
  mergeMenu <- tkmenu(guiMenu, tearoff = FALSE)
  miscMenu <- tkmenu(guiMenu, tearoff = FALSE)

  tkadd(convertMenu, "command", label = "amsibi.to.gz", command = function() insert.txt("amsibi.to.gz"))
  tkadd(convertMenu, "command", label = "vernier.to.gz", command = function() insert.txt("vernier.to.gz"))
  tkadd(convertMenu, "command", label = "mw.to.gz", command = function() insert.txt("mw.to.gz"))
  tkadd(convertMenu, "command", label = "dat.to.gz", command = function() insert.txt("dat.to.gz"))
  tkadd(convertMenu, "command", label = "kd.to.gz", command = function() insert.txt("kd.to.gz"))
  tkadd(guiMenu, "cascade", label = "Convert", menu = convertMenu)

  tkadd(processMenu, "command", label = "process.ecg", command = function() insert.txt("process.ecg"))
  tkadd(processMenu, "command", label = "process.resp", command = function() insert.txt("process.resp"))
  tkadd(guiMenu, "cascade", label = "Process", menu = processMenu)

  tkadd(artifactMenu, "command", label = "artifact.ibi", command = function() insert.txt("artifact.ibi"))
  tkadd(guiMenu, "cascade", label = "Artifact", menu = artifactMenu)

  tkadd(reviewMenu, "command", label = "review.ecg", command = function() insert.txt("review.ecg"))
  tkadd(reviewMenu, "command", label = "review.ibi", command = function() insert.txt("review.ibi"))
  tkadd(reviewMenu, "command", label = "review.resp", command = function() insert.txt("review.resp"))
  tkadd(guiMenu, "cascade", label = "Review", menu = reviewMenu)

  tkadd(extractMenu, "command", label = "extract.ibi", command = function() insert.txt("extract.ibi"))
  tkadd(extractMenu, "command", label = "extract.hrv", command = function() insert.txt("extract.hrv"))
  tkadd(extractMenu, "command", label = "extract.resp", command = function() insert.txt("extract.resp"))
  tkadd(guiMenu, "cascade", label = "Extract", menu = extractMenu)

  tkadd(mergeMenu, "command", label = "merge.hrv", command = function() insert.txt("merge.hrv"))
  tkadd(mergeMenu, "command", label = "merge.resp", command = function() insert.txt("merge.resp"))
  tkadd(guiMenu, "cascade", label = "Merge", menu = mergeMenu)

  tkadd(miscMenu, "command", label = "import.epoch.list", command = function() insert.txt("import.epoch.list"))
  tkadd(miscMenu, "command", label = "MSHeart.file.processing", command = function() insert.txt("MSHeart.file.processing"))
  tkadd(miscMenu, "command", label = "process.imt", command = function() insert.txt("process.imt"))
  tkadd(guiMenu, "cascade", label = "Misc", menu = miscMenu)

  tkfocus(tt)

}

hpcoef <- function(m,fs,fc,type="hamming"){
  h <- lpcoef(m,fs,fc,type=type)
  h <- -h
  h[m/2+1] <- h[m/2+1]+1
  h
}

hrv.time <- function(ibi, frs=10){
  #ibi <- ibi[ibi.vector,]; frs=0

  overlap <- which(as.logical(ibi$artifact))
  #overlap <- overlap[!is.na(overlap)]

  overlap.plus <- sort(unique(c(1, overlap, overlap + 1)))
  overlap.plus <- overlap.plus[overlap.plus <= nrow(ibi)]

  #overlap.plus <- sort(unique(c(1, overlap, overlap + 1, seq(1,nrow(ibi))[c(0,diff(ibi[,"time"]))>2])))
  #overlap.plus <- overlap.plus[overlap.plus <= nrow(ibi)]

  #####
  #####

  nibi <- nrow(ibi)
  nartifact <- length(which(as.logical(ibi$artifact)))

  tibi <- sum(ibi[,"ibi"])/1000
  tartifact <- sum(ibi[ibi$artifact,"ibi"])/1000

  length <- diff(range(ibi[,"time"]))


  #####


  if(length(overlap)>0){

    if(frs > 0){
      rsibi <- ibi[-overlap,"ibi"]
      rsibi <- as.matrix(seq(min(ibi[,"time"]),max(ibi[,"time"]),1/frs))
      rsibi <- cbind(rsibi,approx(ibi[,"time"], ibi[,"ibi"], rsibi[,"time"], method="linear")$y)
      rsibi <- rsibi[!is.na(rsibi[,"ibi"]),]
      mibi <- mean(rsibi[,"ibi"])
    }else{
      mibi <- mean(ibi[-overlap,"ibi"])
    }

  }else{

    if(frs > 0){
      rsibi <- as.matrix(seq(min(ibi[,"time"]),max(ibi[,"time"]),1/frs))
      rsibi <- cbind(rsibi,approx(ibi[,"time"], ibi[,"ibi"], rsibi[,"time"], method="linear")$y)
      rsibi <- rsibi[!is.na(rsibi[,"ibi"]),]
      mibi <- mean(rsibi[,"ibi"])
    }else{
      mibi <- mean(ibi[,"ibi"])
    }

  }


  #####


  if(length(overlap)>0){

    sdnn <- sd(ibi[-overlap,"ibi"])

    diff.ibi <- cbind(ibi[,"time"], c(0,diff(ibi[,"ibi"])))
    msd <- mean(abs(diff.ibi[-overlap.plus,2]))

    pnn50 <- abs(diff.ibi[-overlap.plus,2])
    pnn50 <- length((1:length(pnn50))[pnn50 > 50]) / length(pnn50)

  }else{

    sdnn <- sd(ibi[,"ibi"])

    diff.ibi <- cbind(ibi[,"time"], c(0,diff(ibi[,"ibi"])))
    msd <- mean(abs(diff.ibi[,2]))

    pnn50 <- abs(diff.ibi[,2])
    pnn50 <- length((1:length(pnn50))[pnn50 > 50]) / length(pnn50)

  }

  return(list(length=length, nibi=nibi, nartifact=nartifact, tibi=tibi, tartifact=tartifact, mibi=mibi, sdnn=sdnn, msd=msd, pnn50=pnn50))

}

import.epoch.list <- function(in.file.list = c(), file.origin = NA, event.times = NA, time.vector = NA, report = TRUE, print.epochs = FALSE){

  options(stringsAsFactors = FALSE)

  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = ".epoch.txt", processing.mode = "interactive", path = file.path(), recursive = FALSE)

  if(is.na(file.origin)){
    origin <- "1970-01-01"
  }else{
    origin <- file.origin
  }

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])

    if(report) cat("Reading: ", basename(in.file), "\n", sep="")

    epoch.times <- read.table(in.file, header = TRUE, sep = ",", quote = "\"'",
                              dec = ".", fill = TRUE, strip.white = TRUE, blank.lines.skip = TRUE, comment.char = "#")

    raw.epoch.times <- epoch.times

    for(epoch in 1:nrow(epoch.times)){
      #(epoch <- 1)
      if(all(!is.na(time.vector))){
        if (epoch.times[epoch,"time"]=="0"){
          epoch.times[epoch,"time"] <- min(time.vector)
          next
        }
        if (epoch.times[epoch,"time"]=="00"){
          epoch.times[epoch,"time"] <- max(time.vector)
          next
        }
      }

      stamp <- epoch.times[epoch,"time"]

      if(all(!is.na(event.times))){
        if(any(grep("e", stamp))){
          epoch.times[epoch,"time"] <- event.times[as.numeric(sub("e", "", stamp))]
          next
        }
      }

      if(any(grep("-",stamp))){
        epoch.times[epoch,"time"] <- as.numeric(ISOdatetime(substr(stamp,1,4), substr(stamp,6,7), substr(stamp,9,10), substr(stamp,12,13),substr(stamp,15,16),substr(stamp,18,19), tz = "GMT"))
        if(nchar(stamp) > 19) epoch.times[epoch,"time"] <- as.numeric(epoch.times[epoch,"time"]) + as.numeric(substr(stamp,19,nchar(stamp)))
        next
      }

      if(any(grep(":",stamp))){
        epoch.times[epoch,"time"] <- as.numeric(ISOdatetime(substr(origin,1,4), substr(origin,6,7), substr(origin,9,10), substr(stamp,1,2), substr(stamp,4,5), substr(stamp,7,8), tz = "GMT"))
        if(nchar(stamp) > 8) epoch.times[epoch,"time"] <- as.numeric(epoch.times[epoch,"time"]) + as.numeric(substr(stamp,9,nchar(stamp)))
        next
      }

      # 		epoch.times[epoch,"time"] <- as.numeric(epoch.times[epoch,"time"]) + as.numeric(as.POSIXct(origin, tz="GMT"))

    }



    for(epoch in 1:nrow(epoch.times)){
      #(epoch <- 1)

      stamp <- epoch.times[epoch,"length"]

      if(any(grep("-",stamp))){
        epoch.times[epoch,"length"] <- as.numeric(ISOdatetime(substr(stamp,1,4), substr(stamp,6,7), substr(stamp,9,10), substr(stamp,12,13),substr(stamp,15,16),substr(stamp,18,19), tz = "GMT"))
        if(nchar(stamp) > 19) epoch.times[epoch,"length"] <- as.numeric(epoch.times[epoch,"length"]) + as.numeric(substr(stamp,19,nchar(stamp)))
        next
      }

      if(any(grep(":",stamp))){
        #   		epoch.times[epoch,"length"] <- as.numeric(ISOdatetime(substr(origin,1,4), substr(origin,6,7), substr(origin,9,10), substr(stamp,1,2), substr(stamp,4,5), substr(stamp,7,8), tz = "GMT"))
        epoch.times[epoch,"length"] <- as.numeric(ISOdatetime("1970", "01", "01", substr(stamp,1,2), substr(stamp,4,5), substr(stamp,7,8), tz = "GMT"))
        if(nchar(stamp) > 8) epoch.times[epoch,"length"] <- as.numeric(epoch.times[epoch,"length"]) + as.numeric(substr(stamp,9,nchar(stamp)))
        next
      }

    }

    epoch.times[,"time"] <- as.numeric(epoch.times[,"time"])
    epoch.times[,"length"] <- as.numeric(epoch.times[,"length"])

    epoch.list <- data.frame(name = NA, start = NA, stop = NA)[-1,]
    for (epoch in 1:nrow(epoch.times)){
      #(epoch <- 1)
      if(any(epoch.times[epoch,c("before","after")] != 0)){
        epoch.starts <- epoch.times[epoch,"time"] + seq(-epoch.times[epoch,"before"]*epoch.times[epoch,"length"], epoch.times[epoch,"after"]*epoch.times[epoch,"length"], epoch.times[epoch,"length"])
        epoch.labels <- -epoch.times[epoch,"before"]:epoch.times[epoch,"after"]
        for(i in 1:length(epoch.starts)){
          #(i <- 1)
          epoch.list <- rbind(epoch.list, data.frame(name = paste(epoch.times[epoch,"name"], epoch.labels[i], sep="."), start = epoch.starts[i], stop = epoch.starts[i] + epoch.times[epoch,"length"]))
        }
      }else{
        epoch.list <- rbind(epoch.list, data.frame(name = epoch.times[epoch,"name"], start = epoch.times[epoch,"time"], stop = epoch.times[epoch,"time"] + epoch.times[epoch,"length"]))
      }
    }

    #epoch.list; raw.epoch.times

    #  %Y-%m-%d

    if(print.epochs){
      cat("\n")
      print(raw.epoch.times)
      cat("\n")
      # 		pretty.epoch.list <- data.frame(name = epoch.list$name,
      # 			start = format(as.POSIXct(epoch.list$start, tz = "GMT", origin = "1970-01-01"), format = ifelse(any(grep("-", raw.epoch.times[,"time"])), "%Y-%m-%d %H:%M:%S", "%H:%M:%S"), tz = "GMT", usetz = FALSE),
      # 			stop = format(as.POSIXct(epoch.list$stop, tz = "GMT", origin = "1970-01-01"), format = ifelse(any(grep("-", raw.epoch.times[,"time"])), "%Y-%m-%d %H:%M:%S", "%H:%M:%S"), tz = "GMT", usetz = FALSE),
      # 			duration = epoch.list$stop - epoch.list$start)
      if(is.na(file.origin)){
        pretty.epoch.list <- data.frame(name = epoch.list$name,
                                        start = format(as.POSIXct(epoch.list$start, tz = "GMT", origin = "1970-01-01"), format = "%H:%M:%S"),
                                        stop = format(as.POSIXct(epoch.list$stop, tz = "GMT", origin = "1970-01-01"), format = "%H:%M:%S"),
                                        duration = epoch.list$stop - epoch.list$start)
      }else{
        pretty.epoch.list <- data.frame(name = epoch.list$name,
                                        start = format(as.POSIXct(epoch.list$start, tz = "GMT", origin = "1970-01-01"), format = "%Y-%m-%d %H:%M:%S"),
                                        stop = format(as.POSIXct(epoch.list$stop, tz = "GMT", origin = "1970-01-01"), format = "%Y-%m-%d %H:%M:%S"),
                                        duration = epoch.list$stop - epoch.list$start)
      }
      print(pretty.epoch.list)
      cat("\n")
    }

  }

  if(!print.epochs) return(epoch.list)

}

import.file.info <- function(in.file = file.choose(), report = TRUE){
  #if(report) cat("Reading Info: ", basename(in.file), "\n", sep="")
  if(report) cat("Reading Info File\n", sep="")
  in.file.parts <- unlist(strsplit(basename(in.file), "\\."))
  in.file.info <- read.data(file.path(dirname(in.file), gsub(paste(".",in.file.parts[length(in.file.parts)-1],".",in.file.parts[length(in.file.parts)],sep=""), ".info.txt", basename(in.file), fixed = TRUE)), report = FALSE)
  return(in.file.info)
}

kd.to.gz <- function(in.file.list = c(), pattern = "", path = file.path(), overwrite = FALSE, column.names = c()){
  #in.file.list = c(); pattern = ".asc.txt"; path = file.path("Files"); overwrite = TRUE; column.names = c("time","ibi")

  options(stringsAsFactors = FALSE)

  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  if(!length(in.file.list) & nchar(pattern) & length(path)){
    in.file.list <- list.files(path = path, pattern = pattern, full.names = TRUE, recursive = TRUE)
  }

  if(!length(in.file.list)){
    in.file.list <- tclvalue(tkgetOpenFile(multiple = TRUE, title = "Select Exported ASCII Data File(s)", filetypes="{{data file} {.asc.txt}} {{All files} {*}}"))
    in.file.list <- if(.Platform$OS.type == "unix"){ unlist(strsplit(in.file.list, " ", fixed = TRUE)) }else{ unlist(strsplit(in.file.list, "} {", fixed = TRUE)) }
    in.file.list <- gsub("{", "", in.file.list, fixed = TRUE)
    in.file.list <- gsub("}", "", in.file.list, fixed = TRUE)
  }

  if(!length(in.file.list)) stop("No input file supplied")

  for(in.file in in.file.list){
    #(in.file <- in.file.list[2])
    cat("\n\nReading file: ",in.file,"\n",sep=""); if(!interactive()) flush.console()

    date.time <- read.table(in.file, header = FALSE, sep = " ", nrows = 1, skip = 1, comment.char = "")[1,3:4]
    file.origin <- ISOdatetime(unlist(strsplit(date.time[1,1],"/"))[1], unlist(strsplit(date.time[1,1],"/"))[2], as.numeric(unlist(strsplit(date.time[1,1],"/"))[3]), as.numeric(unlist(strsplit(date.time[1,2],":"))[1]), as.numeric(unlist(strsplit(date.time[1,2],":"))[2]), as.numeric(unlist(strsplit(date.time[1,2],":"))[3]), tz = "GMT")

    if(length(column.names)) { col.names <- column.names }else{ stop("You must specify column names when converting this file type.") }

    file.data <- read.table(in.file, header = FALSE, skip = 5, sep = ",", fill = TRUE, as.is = TRUE, strip.white = TRUE, blank.lines.skip = TRUE, col.names = col.names)

    out.file <- gsub(".asc.txt",paste(".nfo.txt",sep=""), in.file, fixed = TRUE)
    if(!overwrite & file.exists(out.file)){
      cat("\tFile exists: ",basename(out.file),"\nMoving to next file\n", sep=""); if(!interactive()) flush.console()
      next()
    }

    cat("\tRecording duration: ",round(diff(range(file.data[,"time"]))/60)," minutes\n\tSaving: ", basename(out.file),"\n",sep=""); if(!interactive()) flush.console()
    write.table(data.frame(file.origin, fs = NA), file = out.file, append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"))

    for(i in 2:ncol(file.data)){
      #(i <- 2)
      out.file <- gsub(".asc.txt",paste(".",col.names[i],".gz",sep=""), in.file, fixed = TRUE)
      cat("\tSaving: ", basename(out.file),"\n",sep=""); if(!interactive()) flush.console()
      if(col.names[i] == "ibi") file.data[,"ibi"] <- file.data[,"ibi"] * 1000
      write.table(file.data[,c(1,i)], file = gzfile(out.file), append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"))
    }

  }

}

lpcoef <- function(m,fs,fc,type="hamming"){
  h <- vector(mode = "numeric", length = m+1)
  for (i in c(0:m)){
    if((i-m/2)==0){
      h[i+1] <- 2*pi*(fc/fs)
    }else{
      h[i+1] <- sin(2*pi*(fc/fs)*(i-m/2))/(i-m/2)
    }
  }
  h <- switch(type,
              hamming = h*(.54-.46*cos(2*pi*c(0:m)/m)),
              blackman = h*(.42 - .5*cos(2*pi*c(0:m)/m) + .08*cos(4*pi*c(0:m)/m)) )
  h <- h/sum(h)
  h
}

make.epoch.list <- function(in.file.list = c(), pattern = ".phys.gz", processing.mode = "interactive", path = file.path(), recursive = TRUE, overwrite = FALSE, name, time, length, before, after){

  # 	in.file.list = c(); pattern = ".phys.gz"; processing.mode = "interactive"; path = file.path(); recursive = TRUE; overwrite = FALSE; name = "rest"; time = 12; length = 90; before = 0; after = 3
  # 	(300-24)/90

  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])

    cat("\n\nProcessing: ",basename(in.file),"\n",sep=""); if(!interactive()) flush.console()

    out.file <- unlist(strsplit(basename(in.file), "\\."))
    out.file <- file.path(dirname(in.file), gsub(paste(".",out.file[length(out.file)-1],".",out.file[length(out.file)],sep=""), ".epoch.txt", basename(in.file), fixed = TRUE))

    if(!overwrite & file.exists(out.file)){
      cat("Moving to next file.\n", sep=""); if(!interactive()) flush.console()
      next()
    }

    # 		file.data <- read.data(in.file)

    file.epoch <- data.frame(name, time, length, before, after)

    # 		in.file.info <- import.file.info(in.file)
    #
    # 		file.ibi <- data.frame(time = file.data[as.logical(file.data$r),"time"][-1], ibi = diff(file.data[as.logical(file.data$r),"time"])*1000)
    # 		file.ibi <- file.ibi[!apply(file.ibi, 1, function(x){any(is.na(x))}),]

    write.data(file.epoch, out.file, compressed = FALSE)

  }

}

merge.hrv <- function(pattern = ".hrv.gz", path = file.path(), recursive = TRUE, merged.prefix = "Merged.HRV.Data", extract.id = TRUE, write.file = TRUE, output.path = getwd(), report = TRUE){
  merge.data(pattern = pattern, path = path, recursive = recursive, merged.prefix = merged.prefix, extract.id = extract.id, write.file = write.file, output.path = output.path, report = report)
}

merge.resp <- function(pattern = ".resp.gz", path = file.path(), recursive = TRUE, merged.prefix = "Merged.Resp.Data", extract.id = TRUE, write.file = TRUE, output.path = getwd(), report = TRUE){
  merge.data(pattern = pattern, path = path, recursive = recursive, merged.prefix = merged.prefix, extract.id = extract.id, write.file = write.file, output.path = output.path, report = report)
}

mw.to.gz <- function(in.file.list = c(), pattern = ".mw", processing.mode = "interactive", path = file.path(), recursive = TRUE, overwrite = FALSE, column.names = c(), separate.files = FALSE){

  # in.file.list = c(); pattern = ".mw"; processing.mode = "interactive"; path = file.path(); recursive = TRUE; overwrite = TRUE; column.names = c("time","resp","ecg"); separate.files = FALSE

  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])

    cat("\n\nProcessing: ",basename(in.file),"\n",sep=""); if(!interactive()) flush.console()

    if(length(column.names)) { col.names <- column.names }else{ stop("You must specify column names when converting this file type.") }

    file.data <- read.mw(in.file)
    file.header <- file.data$header
    file.data <- file.data$body

    fs <- file.header$scan.rate

    file.data <- cbind(seq(0, by = 1/fs, length.out = nrow(file.data)), file.data)
    colnames(file.data) <- column.names

    #   	file.origin <- ISOdatetime(unlist(strsplit(date.time[1,1],"/"))[3], unlist(strsplit(date.time[1,1],"/"))[1], as.numeric(unlist(strsplit(date.time[1,1],"/"))[2]), 0, 0, 0, tz = "GMT")
    file.origin <- NA

    out.file <- file.path(dirname(in.file), gsub(pattern, ".info.txt", basename(in.file), fixed = TRUE))
    if(!overwrite && file.exists(out.file)){
      cat("Moving to next file.\n", sep=""); if(!interactive()) flush.console()
      next()
    }
    cat("Recording duration: ",round(diff(range(file.data[,"time"]))/60, 1)," minutes\n",sep=""); if(!interactive()) flush.console()
    cat("Writing: ",basename(out.file),"\n",sep=""); if(!interactive()) flush.console()
    write.table(data.frame(file.origin, fs), file = out.file, append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"))

    if(separate.files){
      for(i in 2:ncol(file.data)){
        #(i <- 2)
        out.file <- file.path(dirname(in.file), gsub(pattern, paste(".",col.names[i],".gz",sep=""), basename(in.file), fixed = TRUE))
        write.data(file.data[,c(1,i)], out.file)
      }
    }else{
      out.file <- file.path(dirname(in.file), gsub(pattern, ".phys.gz", basename(in.file), fixed = TRUE))
      write.data(file.data, out.file)
    }

  }

}

peaks <- function(x, span = 1, na.rm = TRUE){
  sdx <- sign(c(NA,diff(x)))
  runs <- rle(sdx)
  px <- cumsum(runs$lengths)[!is.na(runs$values)]
  px <- px[sdx[px] > 0]
  px <- px[sapply(px, function(i){x[i] == max(x[(i-span):(i+span)], na.rm = na.rm)})]
  return(px)
}

process.imt <- function(in.file.list = c(), pattern = ".sav", processing.mode = "interactive", path = file.path(), recursive = TRUE, output.label = "PIP.imt.", write.flagged = TRUE, write.summary = TRUE, write.plots = TRUE){

  #   in.file.list = c(); pattern = ".sav"; processing.mode = "interactive"; path = file.path(); recursive = TRUE; output.label = "PIP.imt."; write.flagged = TRUE; write.summary = TRUE

  if(!is.element("foreign", installed.packages()[,1])) install.packages("foreign", repos = "http://cran.r-project.org", dependencies = TRUE)

  require(tcltk, quietly = TRUE) || stop("tcltk library not available")
  require(foreign, quietly = TRUE) || stop("foreign library not available")

  options(stringsAsFactors = FALSE)

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  file.data <- data.frame()
  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])
    cat("\n\nReading file: ",basename(in.file),"\n",sep=""); if(!interactive()) flush.console()
    file.data <- rbind(file.data, read.spss(in.file, use.value.labels=TRUE, to.data.frame = TRUE))
    variable.labels <- data.frame(label=attr(file.data,"variable.labels"))
  }


  #   Change IDs in URL file to all upper case and remove spaces
  file.data$urlid <- file.data$id
  file.data$id <- toupper(gsub(" ", "", file.data$patid))


  tt.idrecode <- tktoplevel()
  tkwm.title(tt.idrecode, "Recode IDs?")
  done <- tclVar(0)
  yes.but <- tkbutton(tt.idrecode, text="YES", command=function() tclvalue(done)<-1)
  no.but <- tkbutton(tt.idrecode, text="NO", command=function() tclvalue(done)<-2)
  tkgrid(tklabel(tt.idrecode, text = "Do you wish to select recode list(s)?\n\nIf yes, you will be asked to select one or more two-column, space or tab delimited text files listing the old and new subject IDs."), columnspan = 2)
  tkgrid(tklabel(tt.idrecode, text=""), columnspan=2)
  tkgrid(yes.but, row=3, column=0, sticky="e")
  tkgrid(no.but, row=3, column=1, sticky="w")
  tkgrid(tklabel(tt.idrecode, text=""), columnspan=2)
  tkbind(tt.idrecode,"<Destroy>", function() tclvalue(done)<-2)
  tkbind(tt.idrecode,"<y>", function() tclvalue(done)<-1)
  tkbind(tt.idrecode,"<n>", function() tclvalue(done)<-2)
  tkfocus(tt.idrecode)
  tkwait.variable(done)
  done.val <- as.integer(tclvalue(done))
  tkdestroy(tt.idrecode)
  if(done.val == 1){
    recode.file.list <- build.in.file.list(pattern = ".recode.txt", processing.mode = "interactive")
    recode.list <- data.frame()
    cat("\nImporting recode file(s):\n",sep=""); if(!interactive()) flush.console()
    for(recode.file in recode.file.list){
      cat("\nReading file: ",basename(recode.file),"\n",sep=""); if(!interactive()) flush.console()
      recode.list <- rbind(recode.list, read.table(recode.file, sep = "", as.is = TRUE, strip.white = TRUE))
    }
    #     Change IDs in recode file to all upper case and remove spaces
    recode.list <- apply(recode.list, 2, function(x) toupper(gsub(" ", "", x)))
    for(i in 1:nrow(recode.list)) file.data$id[file.data$id == recode.list[i,1]] <- recode.list[i,2]
  }else{
    cat("\nNo recode list(s) selected.\n",sep=""); if(!interactive()) flush.console()
  }




  file.imt <- data.frame(id = file.data$id, urlid = file.data$urlid, dos = file.data$dos)
  cat("\nCalculating IMT and AD variables\n", sep=""); if(!interactive()) flush.console()

  file.imt$mavgimt <- apply(file.data[,c("tcafr","tcanr","tbafr","tiafr","tcafl","tcanl","tbafl","tiafl")], 1, mean, na.rm = TRUE)
  file.imt$mmaximt <- apply(file.data[,c("tcxfl","tcxnl","tbxfl","tixfl","tcxfr","tcxnr","tbxfr","tixfr")], 1, mean, na.rm = TRUE)

  file.imt$mavgcca <- apply(file.data[,c("tcafl","tcanl","tcafr","tcanr")], 1, mean, na.rm = TRUE)
  file.imt$mmaxcca <- apply(file.data[,c("tcxfr","tcxnr","tcxfl","tcxnl")], 1, mean, na.rm = TRUE)

  file.imt$mavgccaf <- apply(file.data[,c("tcafl","tcafr")], 1, mean, na.rm = TRUE)
  file.imt$mmaxccaf <- apply(file.data[,c("tcxfr","tcxfl")], 1, mean, na.rm = TRUE)

  file.imt$mavgica <- apply(file.data[,c("tiafl","tiafr")], 1, mean, na.rm = TRUE)
  file.imt$mmaxica <- apply(file.data[,c("tixfr","tixfl")], 1, mean, na.rm = TRUE)

  file.imt$mavgbulb <- apply(file.data[,c("tbafr","tbafl")], 1, mean, na.rm = TRUE)
  file.imt$mmaxbulb <- apply(file.data[,c("tbxfl","tbxfr")], 1, mean, na.rm = TRUE)

  #adventdiam <- apply(file.data[,c("ADAVGr","ADAVGl")], 1, mean, na.rm = TRUE)

  file.imt$adventdiam <- apply(cbind(apply(file.data[,c("l41ar","l42ar")], 1, mean, na.rm = FALSE), apply(file.data[,c("l41al","l42al")], 1, mean, na.rm = FALSE)), 1, mean, na.rm = FALSE) + 2 * apply(file.data[,c("tcafr","tcafl")], 1, mean, na.rm = FALSE)



  cat("\nChecking raw IMT data for duplicate readings\n", sep=""); if(!interactive()) flush.console()

  r.imt.vars <- matrix(tolower(c("TCANR1","TCMNR1","TCXNR1",
                                 "TCAFR1","TCMFR1","TCXFR1",
                                 "L41AR","L41MR","L41XR",
                                 "TCANR2","TCMNR2","TCXNR2",
                                 "TCAFR2","TCMFR2","TCXFR2",
                                 "L42AR","L42MR","L42XR",
                                 "TBAFR","TBMFR","TBXFR",
                                 "TIAFR","TIMFR","TIXFR")), ncol = 3, byrow = TRUE)

  l.imt.vars <-  matrix(tolower(c("TCANL1","TCMNL1","TCXNL1",
                                  "TCAFL1","TCMFL1","TCXFL1",
                                  "L41AL","L41ML","L41XL",
                                  "TCANL2","TCMNL2","TCXNL2",
                                  "TCAFL2","TCMFL2","TCXFL2",
                                  "L42AL","L42ML","L42XL",
                                  "TBAFL","TBMFL","TBXFL",
                                  "TIAFL","TIMFL","TIXFL")), ncol = 3, byrow = TRUE)

  r.sdlen.vars <- c("TCLENNr1","TCSDNr1","TCLENNr2","TCSDNr2",
                    "TCLENFr1","TCSDFr1","TCLENFr2","TCSDFr2",
                    "L4LENr1","L4SDr1","L4LENr2","L4SDr2")

  l.sdlen.vars <- c("TCLENNl1","TCSDNl1","TCLENNl2","TCSDNl2",
                    "TCLENFl1","TCSDFl1","TCLENFl2","TCSDFl2",
                    "L4LENl1","L4SDl1","L4LENl2","L4SDl2")

  label.list <- c("CCA.IMTn.image.1",
                  "CCA.IMTf.image.1",
                  "CCA.LD45.image.1",
                  "CCA.IMTn.image.2",
                  "CCA.IMTf.image.2",
                  "CCA.LD45.image.2",
                  "Bulb.IMTf.image.3",
                  "ICA.IMTf.image.4")

  file.imt$flag.count <- 0
  file.imt$flag.comment <- ""

  for(i in 1:16){
    #   (i <- 1)
    loop.vec <- apply(file.data[,rbind(r.imt.vars, l.imt.vars)[i,]], 1, function(x){length(unique(x))}) < 2
    file.imt$flag.count[loop.vec] <- file.imt$flag.count[loop.vec] + 1
    file.imt$flag.comment[loop.vec] <- paste(file.imt$flag.comment[loop.vec], "; ", c(paste("R",label.list,sep="."), paste("L",label.list,sep="."))[i], sep="")
    rm(loop.vec)
  }
  file.imt$flag.comment <- substring(file.imt$flag.comment, 3, nchar(file.imt$flag.comment))

  cat(length(which(file.imt$flag.count > 0))," subjects with one or more duplicate readings\n\n", sep=""); if(!interactive()) flush.console()


  if(FALSE){
    cat("LABELS FOR CAROTID IMT AND AD VARIABLES:

        Label mavg.imt = 'MEAN IMT OF AVERAGE'; *This variable represents average IMT across all carotid artery segments measured.*
        Label mmax.imt = 'MEAN OF MAX MEASUREMENTS';

        Label mavg.cca.imt = 'MEAN IMT OF AVERAGE CCA IMT';
        Label mmax.cca.imt = 'Maxmum IMT of Average in CCA';

        Label mavg.ica.imt = 'MEAN IMT OF AVERAGE ICA IMT';
        Label mmax.ica.imt = 'Max IMT of Average in ICA';

        Label mavg.bulb.imt = 'MEAN IMT OF AVERAGE BULB IMT';
        Label mmax.bulb.imt = 'Maxmum IMT of Average in Bulb';

        Label avg.ad.imt = 'MEAN AVERAGE CCA ADVENTITIAL DIAMETER'"); if(!interactive()) flush.console()

    # For the CCA IMT variables (tcafl, tcanl, tcafr and tcanr) and CCA AD variables (adavgl and adavgr), these represent averages of image 1 and image 2 values. Refer to carotid worksheet with corresponding variable names.
  }

  #
  #       Carotid Scan Data:
  #
  #         Calculation of average Carotid IMT and CCA adventitial (AD) variables
  #       mavg=(tcafr+tcanr+tbafr+tiafr+tcafl+tcanl+tbafl+tiafl)/8;
  #       mavg_cca=MEAN(TCAFL,TCANL,TCAFR,TCANR); (for SWAN this is calculated as sum/4)
  #       mavg_ica=MEAN(TIAFL, TIAFR);
  #       mavg_bulb=MEAN(TBAFR, TBAFL);
  #
  #       mmax= mmax=(tcxfl+tcxnl+tbxfl+tixfl+tcxfr+tcxnr+tbxfr+tixfr)/8;
  #
  #       ADAVG= mean(ADAVGr,ADAVGl);
  #
  #       Labels for Carotid IMT and AD variables
  #       Label mavg='MEAN IMT OF AVERAGE'; *This variable represents average IMT across all carotid artery segments measured.*
  #
  #       LABEL mavg_cca ='MEAN IMT OF AVERAGE CCA IMT';
  #       LABEL mavg_ica='MEAN IMT OF AVERAGE ICA IMT';
  #       LABEL mavg_bulb='MEAN IMT OF AVERAGE BULB IMT';
  #
  #       Label mmax='MEAN OF MAX MEASUREMENTS';
  #
  #       Label ADAVG='MEAN AVERAGE CCA ADVENTITIAL DIAMETER"
  #
  #       For the CCA IMT variables (tcafl, tcanl, tcafr and tcanr) and CCA AD variables (adavgl and adavgr), these represent averages of image 1 and image 2 values. Refer to carotid worksheet with corresponding variable names.
  #
  #       Additional variables that can be calculated
  #       max_cca=mean(tcxfr, tcxnr,tcxfl ,tcxnl;
  #       Mmax_ica= (tixfr + tixfl)/2;
  #       Mmax_bulb=(tbxfl+tbxfr)/2;
  #
  #       Label mmax_cca='Maxmum IMT of Average in CCA';
  #       Label mmax_ica=' Max IMT of Average in ICA';
  #       Label mmax_bulb='Maxmum IMT of Average in Bulb';
  #


  td.stamp <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
  write.data(file.imt, paste(output.label,td.stamp,".N",length(unique(file.imt$id)),".csv",sep=""), compressed = FALSE)
  if(write.flagged) write.data(file.data[file.imt$flag.count > 0, c("urlid","id","dos","reader","patid", r.imt.vars, r.sdlen.vars, l.imt.vars, l.sdlen.vars)], paste(output.label, "Flagged.",td.stamp,".N",length(unique(file.data$id[file.imt$flag.count > 0])),".csv",sep=""), compressed = FALSE)

  if(write.summary){
    cat("\nCreating summary file\n", sep=""); if(!interactive()) flush.console()
    summary.file <- paste(output.label, "Summary.",td.stamp,".N",length(unique(file.imt$id)),".txt",sep="")
    capture.output(summary(file.imt[,c("mavgimt","mmaximt","mavgcca","mmaxcca","mavgccaf","mmaxccaf","mavgica","mmaxica","mavgbulb","mmaxbulb","adventdiam","flag.count")]), file = summary.file, append = FALSE)

    cat("Creating boxplots and identifying outliers\n", sep=""); if(!interactive()) flush.console()
    cat("\n\n\nOutliers:\n", file = summary.file, append = TRUE)
    var.list <- c("mavgimt","mmaximt","mavgcca","mmaxcca","mavgccaf","mmaxccaf","mavgica","mmaxica","mavgbulb","mmaxbulb","adventdiam","flag.count")
    for(i in 1:length(var.list)){
      plot.dir <- paste("IMT_Plots_",td.stamp,sep="")
      dir.create(plot.dir, showWarnings = FALSE, recursive = TRUE)
      png(filename = file.path(plot.dir,paste(var.list[i],".png",sep="")), width = 480, height = 480)
      x <- boxplot(file.imt[,var.list[i]], main = var.list[i])
      if(!length(x$out)) next
      text(1, x$out, labels = file.imt$id[which(file.imt[,var.list[i]] %in% x$out)], pos = c(2,4))
      dev.off()
      cat("\n",var.list[i],": ",sep="", file = summary.file, append = TRUE)
      cat(file.imt$id[which(file.imt[,var.list[i]] %in% x$out)], file = summary.file, append = TRUE)
      cat("\n", file = summary.file, append = TRUE)
    }
  }

  cat("\nDone\n\n", sep=""); if(!interactive()) flush.console()

}

qrs <- function(ecg, fs, return.fecg = FALSE, width = round(fs/10)+1, fl = 5, fh = 20, search = 10, threshold = 7, safe = 300){


  #ecg <- scale(ecg)

  fecg <- as.vector(filter(ecg, bpcoef(width, fs, fl, fh), circular = FALSE))

  decg <- c(NA, diff(fecg))

  decg[decg > 0 & !is.na(decg)] <- fecg[decg > 0 & !is.na(decg)]^2
  decg[decg <= 0 & !is.na(decg)] <- 0

  mean.decg <- mean(decg, na.rm = TRUE)

  safe <- round(safe/(1000/fs))
  search <- round(search/(1000/fs))
  ref.col <- search + 1

  r.fun <- function(x){ all(fecg[x[ref.col]] >= fecg[x]) & max(decg[x]) > threshold * mean.decg }

  r <- c(rep(NA, search), apply(embed(seq(fecg), 2*search + 1), 1, r.fun), rep(NA, search))
  r[r & !is.na(r)] <- c(safe,diff(which(r))) >= safe

  if(return.fecg){
    data.frame(fecg, r)
  }else{
    data.frame(r)
  }

}

read.data <- function(data.file = file.choose(), file.type = NA, file.origin = NA, report = TRUE){
  #read.data <- function(data.file, file.type = NA, aslogical = TRUE, report = TRUE){
  #data.file = "/Users/christieic/Dropbox/Mezick/Files/062_1.ecg.gz"; file.type = NA; aslogical = TRUE; report = TRUE

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

  if(!is.na(file.origin) && "time" %in% colnames(in.data)) in.data[,"time"] <- in.data[,"time"] + as.numeric(as.POSIXct(file.origin, tz="GMT"))

  #if(aslogical) for(i in 1:ncol(in.data)) if(all(in.data[,i] %in% c(0,1,NA))) in.data[,i] <- as.logical(in.data[,i])

  return(in.data)

}

read.mw <- function(file.name){

  ### Open file

  fid <- file(file.name, "rb")

  ### begin header
  header.length <- readBin(fid, "integer", n = 1, size = 4, signed = FALSE, endian = "big")
  channel.list.length <- readBin(fid, "integer", n = 1, size = 4, signed = FALSE, endian = "big")
  channel.list <- rawToChar(readBin(fid, "raw", n = channel.list.length, size = 1, endian = "big")) # csv of text, one for each channel

  ### begin hardware config
  hardware.config.length <- readBin(fid, "integer", n = 1, size = 4, signed = FALSE, endian = "big")
  num.channels <- readBin(fid, "integer", n = 1, size = 4, signed = FALSE, endian = "big")

  #remaining_in_header = header_length-4-channel_list_length-4-4;  % (bytes left in header)
  remaining.in.header = header.length - 4 - channel.list.length - 4 - 4 # (bytes left in header)

  channel <- vector("list", num.channels)

  for(i in 1:num.channels){
    name.length <- readBin(fid, "integer", n = 1, size = 4, signed = FALSE, endian = "big")
    channel[[i]]$name <- rawToChar(readBin(fid, "raw", n = name.length, size = 1, endian = "big"))
    channel[[i]]$upper <- readBin(fid, "double", n = 1, size = 4, endian = "big")
    channel[[i]]$lower <- readBin(fid, "double", n = 1, size = 4, endian = "big")
    channel[[i]]$range <- readBin(fid, "double", n = 1, size = 4, endian = "big")
    channel[[i]]$polarity <- readBin(fid, "integer", n = 1, size = 2, signed = FALSE, endian = "big")
    channel[[i]]$gain <- readBin(fid, "double", n = 1, size = 4, endian = "big")
    channel[[i]]$coupling <- readBin(fid, "integer", n = 1, size = 2, signed = FALSE, endian = "big")
    channel[[i]]$config <- readBin(fid, "integer", n = 1, size = 2, signed = FALSE, endian = "big")
    channel[[i]]$scale.multiplier <- readBin(fid, "double", n = 1, size = 4, endian = "big")
    channel[[i]]$scale.offset <- readBin(fid, "double", n = 1, size = 4, endian = "big")
    remaining.in.header <- remaining.in.header - 4 - name.length - 4 - 4 - 4 - 2 - 4 - 2 - 2 - 4 - 4
  }

  scan.rate <- readBin(fid, "double", n = 1, size = 4, endian = "big")
  channel.clock <- readBin(fid, "double", n = 1, size = 4, endian = "big")
  remaining.in.header <- remaining.in.header - 4 - 4

  channel$channel.list <- channel.list
  channel$scan.rate <- scan.rate
  channel$channel.clock <- channel.clock

  remaining.in.header
  test <- rawToChar(readBin(fid, "raw", n = remaining.in.header, size = 1, endian = "big"))

  ###  begin data (2d array of 16bit signed ints)

  all.data <- readBin(fid, "integer", n = file.info(file.name)$size/2, size = 2, signed = TRUE, endian = "big")

  close(fid)

  #reshaped.data <- matrix(all.data, ncol = num.channels, byrow = TRUE)

  list(header = channel, body = matrix(all.data, ncol = num.channels, byrow = TRUE))

}

read.vars <- function(data.file = file.choose(), file.type = NA, report = TRUE){
  #data.file = file.choose(); file.type = NA; report = TRUE

  if(report) cat("Reading Variables: ", basename(data.file), "\n", sep="")

  if(is.na(file.type)){
    file.type <- unlist(strsplit(basename(data.file), "\\."))
    file.type <- file.type[length(file.type)]
  }

  in.file.vars <- colnames(read.table(file=if(file.type == "gz"){gzfile(data.file)}else{data.file},
                                      header = TRUE, sep = ",", quote = "\"'", dec = ".", as.is = TRUE,
                                      na.strings = "NA", colClasses = NA, nrows = 1,
                                      skip = 0, check.names = TRUE, fill = TRUE,
                                      strip.white = TRUE, blank.lines.skip = TRUE,
                                      comment.char = "#", allowEscapes = FALSE, flush = FALSE,
                                      stringsAsFactors = FALSE, encoding = "unknown"))

  return(in.file.vars)

}

vernier.to.gz <- function(in.file.list = c(), pattern = ".asc.txt", processing.mode = "interactive", path = file.path(), recursive = TRUE, overwrite = FALSE, column.names = c(), separate.files = FALSE){

  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  options(stringsAsFactors = FALSE)

  if(!length(in.file.list)) in.file.list <- build.in.file.list(pattern = pattern, processing.mode = processing.mode, path = path, recursive = recursive)

  for(in.file in in.file.list){
    #(in.file <- in.file.list[1])

    cat("\n\nProcessing: ",basename(in.file),"\n",sep=""); if(!interactive()) flush.console()

    date.time <- read.table(in.file, header = FALSE, sep = " ", nrows = 1, skip = 1)[1,2:3]
    file.origin <- ISOdatetime(unlist(strsplit(date.time[1,1],"/"))[3], unlist(strsplit(date.time[1,1],"/"))[1], as.numeric(unlist(strsplit(date.time[1,1],"/"))[2]), 0, 0, 0, tz = "GMT")

    if(length(column.names)) { col.names <- column.names }else{ stop("You must specify column names when converting this file type.") }

    out.file <- gsub(".asc.txt",paste(".info.txt",sep=""), in.file, fixed = TRUE)
    if(!overwrite & file.exists(out.file)){
      cat("Moving to next file.\n", sep=""); if(!interactive()) flush.console()
      next()
    }

    file.data <- read.table(in.file, header = FALSE, skip = 6, sep = "", fill = TRUE, as.is = TRUE, strip.white = TRUE, blank.lines.skip = TRUE, col.names = col.names)
    file.data[,1] <- file.data[,1]*60
    fs <- as.integer(1/diff(file.data[1:2,1]))

    cat("Recording duration: ",round(diff(range(file.data[,"time"]))/60)," minutes\n",sep=""); if(!interactive()) flush.console()
    cat("Writing: ",out.file,"\n",sep=""); if(!interactive()) flush.console()
    write.table(data.frame(file.origin, fs), file = out.file, append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"))

    if(separate.files){
      for(i in 2:ncol(file.data)){
        #(i <- 2)
        out.file <- gsub(".asc.txt",paste(".",col.names[i],".gz",sep=""), in.file, fixed = TRUE)
        write.data(file.data[,c(1,i)], out.file)
      }
    }else{
      out.file <- gsub(".asc.txt",".phys.gz", in.file, fixed = TRUE)
      write.data(file.data, out.file)
    }

  }

}

write.data <- function(out.data, data.file, compressed = TRUE, quote = FALSE, report = TRUE){

  if(report) cat("Writing: ", basename(data.file), "\n", sep="")

  for(i in 1:ncol(out.data)) if(is.logical(out.data[,i])) out.data[,i] <- as.numeric(out.data[,i])

  if(!file.exists(dirname(data.file))) dir.create(dirname(data.file), showWarnings = TRUE, recursive = TRUE)

  write.table(out.data, file = if(compressed){gzfile(data.file)}else{data.file},
              append = FALSE, quote = quote, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"))

}
