data.viewer <- function(file.data = NA, in.file = "", pattern = ".gz", out.file = in.file,
                        x.var, y.var, id.var = NA, origin = NA,
                        label.mode = c("all","max","min")[1], pch = 20, id.pch = 20,
                        ymin = NA, ymax = NA, width, edge.delta.1, edge.delta.2,
                        width.delta, ...){

  require(tcltk, quietly = TRUE) || stop("tcl/tk library not available")

  if(all(is.na(file.data)) && !nchar(in.file)) in.file <- build.in.file.list(pattern = pattern, multiple = FALSE)
  if(all(is.na(file.data)) && !nchar(in.file)) stop("You must select a file.")

  if(all(is.na(file.data))) file.data <- read.data(in.file)

  if(!id.var %in% colnames(file.data)) file.data[,id.var] <- 0

  file.data.backup <- file.data
  file.data.copy <- file.data

  dev.new(width = 12, height = 6, xpos = 200, ypos = 0, title = paste("Data Viewer Plot: ",y.var," by ",x.var,sep=""))
  if(nchar(in.file)){
    par(mar=c(3.25,3.25,2,.25))
  }else{
    par(mar=c(3.25,3.25,.25,.25))
  }

  draw.data <- function(...){
    edge <- eval(parse(text=as.character(tclvalue(edge))))
    width <- eval(parse(text=as.character(tclvalue(width))))
    fit.window <- eval(parse(text=as.character(tclvalue(fit.window))))
    hide.id.var <- eval(parse(text=as.character(tclvalue(hide.id.var))))
    ymax <- eval(parse(text=as.character(tclvalue(ymax))))
    ymin <- eval(parse(text=as.character(tclvalue(ymin))))

    data.slice <<- file.data[file.data[,x.var] >= edge & file.data[,x.var] <= (edge+width),]

    if(nrow(data.slice) && !all(is.na(data.slice[,y.var]))){
      if(hide.id.var) data.slice <- data.slice[!as.logical(data.slice[,id.var]),]

      if(fit.window){
        plot(data.slice[,x.var], data.slice[,y.var], xlab="", ylab="", type="l", axes=FALSE);box()
      }else{
        plot(data.slice[,x.var], data.slice[,y.var], xlab="", ylab="", type="l", ylim=c(ymin,ymax), axes=FALSE);box()
      }

      title(, xlab=x.var, ylab=y.var, line = 2.25)
      if(nchar(in.file)) title(main = basename(in.file), line = .5, font.main = 1)

      if(!is.na(pch)) points(data.slice[,x.var], data.slice[,y.var], pch=pch, bg="white")

      if(!is.na(id.pch) && !hide.id.var) points(data.slice[as.logical(data.slice[,id.var]),x.var], data.slice[as.logical(data.slice[,id.var]),y.var], pch=id.pch, bg="red", col="red")

      if(!is.na(origin)){
        axis(1, at = pretty(data.slice[,x.var],4), labels = format(as.POSIXct(pretty(data.slice[,x.var],4), origin=origin, tz="GMT"), "%H:%M:%S"))
      }else{
        axis(1)
      }
      axis(2)
    }

    # 		tkfocus(tt)
    # 		tkwm.deiconify(tt)
    # 		tkgrab(tt)
    # 		tkraise(tt)
    # 		tcl("wm", "attributes", tt, topmost=TRUE)
  }



  add.label <- function(){

    file.data.copy <<- file.data

    click <- locator(n=2); click$x <- sort(click$x); click$y <- sort(click$y)
    selected.points <- which(file.data[,x.var] >= click$x[1] & file.data[,x.var] <= click$x[2] & file.data[,y.var] >= click$y[1] & file.data[,y.var] <=  click$y[2])

    if(label.mode == "all"){
      #file.data[file.data[,x.var] >= click$x[1] & file.data[,x.var] <= click$x[2] & file.data[,y.var] >= click$y[1] & file.data[,y.var] <=  click$y[2], id.var] <- TRUE
      file.data[selected.points, id.var] <- 1
    }

    if(label.mode == "max"){
      file.data[selected.points[which.max(file.data[selected.points, y.var])], id.var] <- 1
    }

    if(label.mode == "min"){
      file.data[selected.points[which.min(file.data[selected.points, y.var])], id.var] <- 1
    }

    file.data <<- file.data

    draw.data()
  }


  remove.label <- function(){

    file.data.copy <<- file.data

    click <- locator(n=2); click$x <- sort(click$x); click$y <- sort(click$y)
    selected.points <- which(file.data[,x.var] >= click$x[1] & file.data[,x.var] <= click$x[2] & file.data[,y.var] >= click$y[1] & file.data[,y.var] <=  click$y[2])

    file.data[selected.points, id.var] <- 0

    file.data <<- file.data

    draw.data()
  }



  undo.label <- function(){
    file.data <<- file.data.copy
    draw.data()
  }



  zoom.fun <- function(){
    #eval(parse(text=as.character(tclvalue())))
    pre.zoom <<- list(edge=eval(parse(text=as.character(tclvalue(edge)))),
                      width=eval(parse(text=as.character(tclvalue(width)))),
                      ymin=eval(parse(text=as.character(tclvalue(ymin)))),
                      ymax=eval(parse(text=as.character(tclvalue(ymax)))))
    click <- locator(n=2)
    click$x <- sort(click$x)
    click$y <- sort(click$y)
    tclvalue(edge) <- click$x[1]
    tclvalue(width) <- click$x[2] - click$x[1]
    tclvalue(ymin) <- click$y[1]
    tclvalue(ymax) <- click$y[2]
    draw.data()
  }

  unzoom.fun <- function(){
    tclvalue(edge) <- pre.zoom$edge
    tclvalue(width) <- pre.zoom$width
    tclvalue(ymin) <- pre.zoom$ymin
    tclvalue(ymax) <- pre.zoom$ymax
    draw.data()
  }



  edge.home <- function(){
    tclvalue(edge) <- min(data.slice[,x.var])
    draw.data()
  }

  edge.end <- function(){
    #eval(parse(text=as.character(tclvalue())))
    tclvalue(edge) <- max(data.slice[,x.var]) - eval(parse(text=as.character(tclvalue(width))))
    draw.data()
  }

  edge.add.one <- function(){
    #eval(parse(text=as.character(tclvalue())))
    tclvalue(edge) <- eval(parse(text=as.character(tclvalue(edge)))) + 1
    draw.data()
  }

  edge.sub.one <- function(){
    tclvalue(edge) <- eval(parse(text=as.character(tclvalue(edge)))) - 1
    draw.data()
  }

  sub.edge.1.fun <- function(){
    tclvalue(edge) <- eval(parse(text=as.character(tclvalue(edge)))) - eval(parse(text=as.character(tclvalue(edge.delta.1))))
    draw.data()
  }

  add.edge.1.fun <- function(){
    tclvalue(edge) <- eval(parse(text=as.character(tclvalue(edge)))) + eval(parse(text=as.character(tclvalue(edge.delta.1))))
    draw.data()
  }

  sub.edge.2.fun <- function(){
    tclvalue(edge) <- eval(parse(text=as.character(tclvalue(edge)))) - eval(parse(text=as.character(tclvalue(edge.delta.2))))
    draw.data()
  }

  add.edge.2.fun <- function(){
    tclvalue(edge) <- eval(parse(text=as.character(tclvalue(edge)))) + eval(parse(text=as.character(tclvalue(edge.delta.2))))
    draw.data()
  }

  sub.width.fun <- function(){
    tclvalue(width) <- eval(parse(text=as.character(tclvalue(width)))) - eval(parse(text=as.character(tclvalue(width.delta))))
    draw.data()
  }

  add.width.fun <- function(){
    tclvalue(width) <- eval(parse(text=as.character(tclvalue(width)))) + eval(parse(text=as.character(tclvalue(width.delta))))
    draw.data()
  }

  width.add.one <- function(){
    tclvalue(width) <- eval(parse(text=as.character(tclvalue(width)))) + 1
    draw.data()
  }

  width.sub.one <- function(){
    tclvalue(width) <- eval(parse(text=as.character(tclvalue(width)))) - 1
    draw.data()
  }

  ymax.add <- function(){
    tclvalue(ymax) <- eval(parse(text=as.character(tclvalue(ymax)))) + eval(parse(text=as.character(tclvalue(ymax.delta))))
    draw.data()
  }

  ymax.sub <- function(){
    tclvalue(ymax) <- eval(parse(text=as.character(tclvalue(ymax)))) - eval(parse(text=as.character(tclvalue(ymax.delta))))
    draw.data()
  }

  ymin.add <- function(){
    tclvalue(ymin) <- eval(parse(text=as.character(tclvalue(ymin)))) + eval(parse(text=as.character(tclvalue(ymin.delta))))
    draw.data()
  }

  ymin.sub <- function(){
    tclvalue(ymin) <- eval(parse(text=as.character(tclvalue(ymin)))) - eval(parse(text=as.character(tclvalue(ymin.delta))))
    draw.data()
  }

  yshift.up <- function(){
    tclvalue(ymax) <- eval(parse(text=as.character(tclvalue(ymax)))) + eval(parse(text=as.character(tclvalue(ymax.delta))))
    tclvalue(ymin) <- eval(parse(text=as.character(tclvalue(ymin)))) + eval(parse(text=as.character(tclvalue(ymin.delta))))
    draw.data()
  }

  yshift.down <- function(){
    tclvalue(ymax) <- eval(parse(text=as.character(tclvalue(ymax)))) - eval(parse(text=as.character(tclvalue(ymax.delta))))
    tclvalue(ymin) <- eval(parse(text=as.character(tclvalue(ymin)))) - eval(parse(text=as.character(tclvalue(ymin.delta))))
    draw.data()
  }

  toggle.fit.window <- function(){
    if(as.logical(tclvalue(fit.window))){
      tclvalue(fit.window) <- "FALSE"
    }else{
      tclvalue(fit.window) <- "TRUE"
    }
    draw.data()
  }

  toggle.hide.id.var <- function(){
    if(as.logical(tclvalue(hide.id.var))){
      tclvalue(hide.id.var) <- "FALSE"
    }else{
      tclvalue(hide.id.var) <- "TRUE"
    }
    draw.data()
  }

  draw.fun <- function(){
    click <- locator(n=2)
    click$x <- sort(click$x)
    click$y <- sort(click$y)
    polygon(c(click$x[1],click$x[1],click$x[2],click$x[2]), c(click$y[1],click$y[2],click$y[2],click$y[1]), lwd=2, border="blue")
  }

  clear.fun <- function(){
    draw.data()
  }

  save.fun <- function(){
    if(!length(out.file)){
      cat("You must specify out.file in order to save changes.\n",sep=""); if(!interactive()) flush.console()
    }else{
      if(!identical(file.data, file.data.backup)){
        if(confirm.file.replace(out.file)){
          write.data(file.data, out.file)
        }
      }
    }
  }

  exit.fun <- function(){
    if(!length(out.file)){
      cat("You must specify out.file in order to save changes.\n",sep=""); if(!interactive()) flush.console()
    }else{
      if(!identical(file.data, file.data.backup)){
        if(confirm.file.replace(out.file)){
          write.data(file.data, out.file)
        }
      }
    }

    #       if(!all(dim(file.ibi) == dim(temp)) || !all(file.ibi == temp)){
    #         if(confirm.file.replace(in.file)){
    # 				  write.data(file.ibi, out.file)
    #         }else{
    # 				  rm(temp)
    # 			  }
    #
    # 		  }

    tkdestroy(tt)
  }

  cancel.fun <- function(){
    tkdestroy(tt)
  }


  tt <- tktoplevel()
  tkwm.title(tt,"Data Viewer Controller")

  guiMenu <- tkmenu(tt)
  tkconfigure(tt, menu = guiMenu)

  fileMenu <- tkmenu(guiMenu, tearoff = FALSE)

  tkadd(fileMenu, "command", label = "Save", command = function() save.fun())
  tkadd(fileMenu, "command", label = "Exit w/Save", command = function() exit.fun())
  tkadd(fileMenu, "command", label = "Exit w/out Save", command = function() cancel.fun())
  tkadd(guiMenu, "cascade", label = "File", menu = fileMenu)

  tt.frame <- tkframe(tt)
  label.frame <- tkframe(tt.frame)
  zoom.frame <- tkframe(tt.frame)
  view.frame <- tkframe(tt.frame)
  draw.frame <- tkframe(tt.frame)
  close.frame <- tkframe(tt.frame)

  edge <- tclVar(min(file.data[,x.var]))
  width <- tclVar(width)
  edge.delta.1 <- tclVar(edge.delta.1)
  edge.delta.2 <- tclVar(edge.delta.2)
  width.delta <- tclVar(width.delta)
  goto.edge <- tclVar()
  goto.edge <- tclVar()
  goto.width <- tclVar()
  fit.window <- tclVar("FALSE")
  hide.id.var <- tclVar("FALSE")
  ymin <- tclVar(min(c(ymin, file.data[,y.var]), na.rm = TRUE))
  ymax <- tclVar(max(c(ymax, file.data[,y.var]), na.rm = TRUE))
  ymin.delta <- tclVar(.1*diff(range(file.data[,y.var], na.rm = TRUE)))
  ymax.delta <- tclVar(.1*diff(range(file.data[,y.var], na.rm = TRUE)))
  goto.ymin <- tclVar()
  goto.ymax <- tclVar()
  # 	done <- tclVar(0)


  #b.change.mode <- tkbutton(view.frame, text="Change", command=change.mode)

  s.edge <- tkscale(tt.frame, from=min(file.data[,x.var]), to=max(file.data[,x.var]), resolution=1, length=300, sliderlength=10, showvalue=FALSE, orient="horiz", variable=edge, command=draw.data)
  e.edge <- tkentry(tt.frame, textvariable=edge, width=12)
  e.goto.edge <- tkentry(tt.frame, textvariable=goto.edge, width=12)
  b.goto.edge <- tkbutton(tt.frame, text="Go to:", command=function(){tclvalue(edge) <- as.numeric(tclvalue(goto.edge)); draw.data()})

  b.sub.edge.1 <- tkbutton(tt.frame, text="<", command=sub.edge.1.fun)
  b.add.edge.1 <- tkbutton(tt.frame, text=">", command=add.edge.1.fun)
  e.edge.delta.1 <- tkentry(tt.frame, textvariable=edge.delta.1, width=8)

  b.sub.edge.2 <- tkbutton(tt.frame, text="<", command=sub.edge.2.fun)
  b.add.edge.2 <- tkbutton(tt.frame, text=">", command=add.edge.2.fun)
  e.edge.delta.2 <- tkentry(tt.frame, textvariable=edge.delta.2, width=8)

  e.width <- tkentry(tt.frame, textvariable=width, width=12)
  e.goto.width <- tkentry(tt.frame, textvariable=goto.width, width=12)
  b.goto.width <- tkbutton(tt.frame, text="Go to:", command=function(){tclvalue(width) <- as.numeric(tclvalue(goto.width)); draw.data()})
  b.sub.width <- tkbutton(tt.frame, text="<", command=sub.width.fun)
  b.add.width <- tkbutton(tt.frame, text=">", command=add.width.fun)
  e.width.delta <- tkentry(tt.frame, textvariable=width.delta, width=8)

  c.fit.window <- tkcheckbutton(view.frame,text="Fit Y-Axis", offvalue="FALSE", onvalue="TRUE", variable=fit.window, command=draw.data)
  c.hide.id.var <- tkcheckbutton(view.frame,text="Hide Labeled Points", offvalue="FALSE", onvalue="TRUE", variable=hide.id.var, command=draw.data)

  b.zoom <- tkbutton(zoom.frame, text="Zoom", command=zoom.fun)
  b.unzoom <- tkbutton(zoom.frame, text="UnZoom", command=unzoom.fun)

  b.add <- tkbutton(label.frame, text="Add", command=add.label)
  b.remove <- tkbutton(label.frame, text="Remove", command=remove.label)
  b.undo <- tkbutton(label.frame, text="Undo", command=undo.label)

  e.ymax <- tkentry(tt.frame, textvariable=ymax, width=12)
  e.ymax.delta <- tkentry(tt.frame, textvariable=ymax.delta, width=12)
  b.sub.ymax <- tkbutton(tt.frame, text="<", command=ymax.sub)
  b.add.ymax <- tkbutton(tt.frame, text=">", command=ymax.add)
  e.goto.ymax <- tkentry(tt.frame, textvariable=goto.ymax, width=12)
  b.goto.ymax <- tkbutton(tt.frame, text="Go to:", command=function(){tclvalue(ymax) <- as.numeric(tclvalue(goto.ymax)); draw.data()})

  e.ymin <- tkentry(tt.frame, textvariable=ymin, width=12)
  e.ymin.delta <- tkentry(tt.frame, textvariable=ymin.delta, width=12)
  b.sub.ymin <- tkbutton(tt.frame, text="<", command=ymin.sub)
  b.add.ymin <- tkbutton(tt.frame, text=">", command=ymin.add)
  e.goto.ymin <- tkentry(tt.frame, textvariable=goto.ymin, width=12)
  b.goto.ymin <- tkbutton(tt.frame, text="Go to:", command=function(){tclvalue(ymin) <- as.numeric(tclvalue(goto.ymin)); draw.data()})

  b.draw <- tkbutton(draw.frame, text="Draw", command=draw.fun)
  b.clear <- tkbutton(draw.frame, text="Clear", command=clear.fun)

  b.exit <- tkbutton(close.frame, text=" Save & Exit ", command=exit.fun)
  b.cancel <- tkbutton(close.frame, text=" Cancel ", command=cancel.fun)


  tkgrid(tt.frame)

  tkgrid(tklabel(tt.frame, text=""), columnspan=3)

  tkgrid(tklabel(tt.frame, text="Window Edge"), columnspan=3)
  tkgrid(e.edge, column=1, sticky="ew")
  tkgrid(s.edge, columnspan=3)
  tkgrid(b.sub.edge.1, e.edge.delta.1, b.add.edge.1)
  tkgrid(b.sub.edge.2, e.edge.delta.2, b.add.edge.2)
  tkgrid(b.goto.edge, column=1)
  tkgrid(e.goto.edge, column=1, sticky="ew")

  tkgrid.configure(b.sub.edge.1, b.sub.edge.2, sticky="e")
  tkgrid.configure(e.edge.delta.1, e.edge.delta.2, sticky="ew")
  tkgrid.configure(b.add.edge.1, b.add.edge.2, sticky="w")

  tkgrid(tklabel(tt.frame, text=""), columnspan=3)

  tkgrid(tklabel(tt.frame, text="Window Width"), columnspan=3)
  tkgrid(e.width, column=1, sticky="ew")
  tkgrid(b.sub.width, e.width.delta, b.add.width)

  tkgrid.configure(b.sub.width, sticky="e")
  tkgrid.configure(e.edge.delta.1, e.edge.delta.2, e.width.delta, sticky="ew")
  tkgrid.configure(b.add.width, sticky="w")

  tkgrid(b.goto.width, column=1)
  tkgrid(e.goto.width, column=1, sticky="ew")

  tkgrid(tklabel(tt.frame,text=""), columnspan=3)

  tkgrid(view.frame, zoom.frame, label.frame)

  tkgrid(c.fit.window)
  tkgrid(c.hide.id.var)

  tkgrid(b.zoom)
  tkgrid(b.unzoom)

  tkgrid(b.add)
  tkgrid(b.remove)
  tkgrid(b.undo)

  tkgrid(tklabel(tt.frame, text=""), columnspan=3)

  tkgrid(tklabel(tt.frame, text="Y-Max"), column=1)
  tkgrid(e.ymax, column=1, sticky="ew")
  tkgrid(b.sub.ymax, e.ymax.delta, b.add.ymax)
  tkgrid.configure(e.ymax.delta, sticky="ew")
  tkgrid.configure(b.sub.ymax, sticky="e")
  tkgrid.configure(b.add.ymax, sticky="w")
  tkgrid(b.goto.ymax, column=1)
  tkgrid(e.goto.ymax, column=1, sticky="ew")

  tkgrid(tklabel(tt.frame, text=""), columnspan=3)

  tkgrid(tklabel(tt.frame, text="Y-Min"), column=1)
  tkgrid(e.ymin, column=1, sticky="ew")
  tkgrid(b.sub.ymin, e.ymin.delta, b.add.ymin)
  tkgrid.configure(e.ymin.delta, sticky="ew")
  tkgrid.configure(b.sub.ymin, sticky="e")
  tkgrid.configure(b.add.ymin, sticky="w")
  tkgrid(b.goto.ymin, column=1)
  tkgrid(e.goto.ymin, column=1, sticky="ew")

  tkgrid(draw.frame, close.frame)
  tkgrid.configure(draw.frame, column=0)
  tkgrid.configure(close.frame, column=2)

  tkgrid(b.draw)
  tkgrid(b.clear)

  tkgrid(b.exit)
  tkgrid(b.cancel)


  ##### KEY BINDINGS #####

  tkbind(tt, "<Home>", edge.home)
  tkbind(tt, "<End>", edge.end)

  tkbind(tt, "<Left>", sub.edge.1.fun)
  tkbind(tt, "<Right>", add.edge.1.fun)
  tkbind(tt, "<Control-Left>", sub.edge.2.fun)
  tkbind(tt, "<Control-Right>", add.edge.2.fun)
  tkbind(tt, "<Shift-Left>", edge.sub.one)
  tkbind(tt, "<Shift-Right>", edge.add.one)

  tkbind(tt, "<Down>", sub.width.fun)
  tkbind(tt, "<Up>", add.width.fun)
  tkbind(tt, "<Shift-Up>", width.sub.one)
  tkbind(tt, "<Shift-Down>", width.add.one)

  tkbind(tt, "<Insert>", add.label)
  tkbind(tt, "<Delete>", remove.label)
  tkbind(tt, "<u>", undo.label)

  tkbind(tt, "<z>", zoom.fun)
  tkbind(tt, "<Control-z>", unzoom.fun)

  tkbind(tt, "<f>", toggle.fit.window)
  tkbind(tt, "<h>", toggle.hide.id.var)

  tkbind(tt, "<Shift-Prior>", ymax.add)
  tkbind(tt, "<Shift-Next>", ymax.sub)
  tkbind(tt, "<Control-Prior>", ymin.add)
  tkbind(tt, "<Control-Next>", ymin.sub)
  tkbind(tt, "<Prior>", yshift.up)
  tkbind(tt, "<Next>", yshift.down)

  tkbind(tt, "<d>", draw.fun)
  tkbind(tt, "<c>", clear.fun)

  #########################


  tkfocus(tt)


  # 	tkwait.variable(done)
  #
  # 	tkdestroy(tt)
  #
  # 	if (tclvalue(done)==2){
  # 		file.data <- file.data.backup
  # 	}
  #
  # return(file.data)

}
