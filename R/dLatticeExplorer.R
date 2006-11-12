## create a droppable, dyamic widget for exploratory graphs
## TODO: make panel functions better
##       * add more lattice graphs?
##       * means to edit formulas
## when we close window -- clear out handlers on clear button
## add refressh button or refresh on click?
## for drawing panel on graphs
panel.mean = function(x,y) {
  panel.abline(v=mean(x))
}
panel.median = function(x,y) {
  panel.abline(v=median(x))
}

dLatticeExplorer = function(
  container = NULL,
  ...) {
  
  ## graphics device
  gdGroup = ggroup(horizontal=FALSE, container = container, raise.on.dragmotion = TRUE)
  gd = ggraphics()

  
#  obj = list(ref=gdGroup, device = gd)
#  class(obj) = c("dLatticeExplorer","gComponent","gWidget")

  obj = gdGroup

  
  ## initialize
  tag(obj,"varlist") <- list()
  tag(obj,"function") <- "densityplot"
  tag(obj, "dropHandlers") <- list()
  
  ## add buttons
  gdButtons = ggroup(container = gdGroup)
  add(gdGroup, gd, expand=FALSE)

  ## what to do with dropped values.
  ## can't put obj in here without some machinations, as the
  dropHandler = handler = function(h,...) {
    ## we have the following:
    ## add to varlist by appending
    ## obj is found by closure
    varlist = tag(obj,"varlist")
    tag(obj,"varlist") <- c(varlist, h$dropdata)
    updatedLatticeExplorer(obj)

    ## now bind to be dynamic *if* a treeviewcolumn
#    if(class(h$dropdata)[1] == "GtkTreeViewColumn") {
    if(is.gdataframecolumn(h$dropdata)) {
      view.col = h$dropdata
      id = addhandlerchanged(view.col, handler=function(h,...) updatedLatticeExplorer(obj))
      dropHandlers = tag(obj,"dropHandlers")
      dropHandlers[[length(dropHandlers)+1]] = list(
                    view.col = view.col,
                    id = id
                    )
      tag(obj,"dropHandlers") <- dropHandlers
     }
    return(TRUE)
  }
  adddroptarget(gd,targetType="object",handler = dropHandler )

   allGraphs = c("dotplot","xyplot","barchart","stripplot","bwplot","qq",
     "--------",
     "histogram","densityplot","qqmath")
   univariateGraphs = c("histogram","densityplot","qqmath")
   availPanelFuns = list(
     dotplot = c("mean","median"),
     xyplot = c("lmline","loess","lines","rug"),
     barchart = c(),
     stripplot = c("mean","median"),
     bwplot = c("violin","mean"),
     qq = c("lmline"),
     "--------" = c(),
     histogram = c(),
     densityplot = c("rug","mean","median"),
     qqmath = c("qqmathline")
     )
  ## now store these into the object
  tag(obj,"allGraphs")  <- allGraphs
  tag(obj,"univariateGraphs")  <- univariateGraphs
  tag(obj,"availPanelFuns")  <- availPanelFuns

  clearButton = gbutton("clear",action = obj,
    handler = function(h,...) {
      clearPlot(obj)
      svalue(plotChooser) <- "densityplot"
    })
  add(gdButtons,clearButton)
  add(gdButtons,gbutton("refresh",handler = function(h,...) {
    updatedLatticeExplorer(obj)
  }))
  addSpring(gdButtons)

  plotChooser = gdroplist(c(allGraphs), container=gdButtons,
    action = obj,
    handler = function(h,...) {
      FUN = svalue(h$obj)
      if(!is.empty(FUN) || FUN != "--------") {
        tag(h$action,"function") <- FUN
        clearPanel(obj)
        updatePanel(obj)
        updatedLatticeExplorer(obj)
      }
    })
  svalue(plotChooser) <- "densityplot"

  panelChooser = gdroplist(c("panel="), container = gdButtons,
     action = obj,
     handler = function(h,...) {
       panel.FUN = svalue(h$obj)
       if(!is.empty(panel.FUN) && panel.FUN != "panel=")
         tag(obj, "panel") <- panel.FUN
       else
         tag(obj, "panel") <- NA
       updatedLatticeExplorer(obj)
     })
  tag(obj,"plotChooser") <-  plotChooser
  tag(obj,"panelChooser") <-  panelChooser
  updatePanel(obj)

#  makeEmptyPlot(obj)

  ## clean up when closed or unrealized
  addhandlerunrealize(obj, handler = function(h,...) {
    clearPlot(obj)                      # also clears out handlers
  })
  return(obj)
}
##################################################
## evaluate plot
as.dLatticeExplorer = function(da,...) {
  warning("as.dLatticeExplorer Needs writing")
  return(da)
  if(class(da)[1] == "GtkDrawingArea" &&
     !is.empty(tag(da,"device"))) {
    da = list(ref=da, device = tag(da,"device"))
    class(da) <- c("iGD", "iComponent")
  }
  if(class(da)[1] == "iGd") {
    obj = list(ref=NULL, device=da)
    class(obj) = c("dLatticeExplorer","gComponent","gWidget")
    return(obj)
  } else {
    return(da)
  }
}

##getGTKwidget.dLatticeExplorer = function(obj,...) obj$ref ##obj$device$ref #$ref??
##visible.dLatticeExplorer = function(obj,...) visible(obj$device)

## Main workhorse
updatedLatticeExplorer = function(object,...) {
  obj = object                          # for s3 consistency
  require(lattice)

  
  vars = tag(obj,"varlist")
  FUN = tag(obj,"function")
  doUnivariate = FUN %in% tag(obj,"univariateGraphs")
  
  ## make formula
  nvars = length(vars)
  ## where to evaluate
  env = environment()
    
  if(nvars == 0) {
    cat("can't draw plot, add variables\n")
    makeEmptyPlot(obj)
    return()
  }
  varNames = sapply(1:nvars, function(i) id(vars[[i]]))
  sapply(1:nvars, function(i)
         assign(id(vars[[i]]),svalue(vars[[i]]), envir=env)
         )

  command = NA
  lst = list()
  if(nvars == 1) {
    if(doUnivariate) {
      command = Paste(FUN,"( ~ ", varNames[1],")")
      xlab = varNames[1]; ylab=NULL
      x = ~ svalue(vars[[1]])
    }
    else {
      command = Paste(FUN,"(", varNames[1],")")
      xlab = varNames[1]; ylab=NULL
      x = svalue(vars[[1]])
    }
  }

  if(nvars == 2) {
    ## check that not too many levels
    if(nlevels(shingle(svalue(vars[[2]]))) > 40) {
      cat("Too many levels for the conditioning variable\n")
      return()
    }
    if(doUnivariate) {
      command = Paste(FUN,"( ~ ", varNames[1],"|", varNames[2],")")
      xlab = varNames[1]; ylab = NULL
      x = ~ svalue(vars[[1]]) | svalue(vars[[2]])
    } else {
      command = Paste(FUN,"(", varNames[2],"~", varNames[1],")")
      xlab = varNames[1]; ylab = varNames[2]
      x = svalue(vars[[2]]) ~ svalue(vars[[1]])
    }
  }

  if(nvars >= 3) {
    if(doUnivariate) {
      cat("univariate only has two possible variables\n")
      command = NULL; xlab = NULL: ylab=NULL
      x = NULL
    } else {
      m1 = nlevels(shingle(svalue(vars[[2]])))
      m2 = nlevels(shingle(svalue(vars[[3]])))
      if(m1*m2 > 40) {
        cat("Too many levels for the conditioning variables\n")
        return()
      }


      command = Paste(FUN,"(", varNames[1],"~", varNames[2],"|", varNames[3],")")
      xlab = varNames[1]; ylab = varNames[2]
      x = svalue(vars[[2]]) ~ svalue(vars[[1]]) | svalue(vars[[3]])
    }
  }
  
  lst$x = x
  lst$main = command
  lst$xlab = xlab
  lst$ylab = ylab
  ## add panel
  panel.FUN = tag(obj,"panel")
  if(is(panel.FUN,"guiWidget")) panel.FUN = svalue(panel.FUN)
  if(!is.empty(panel.FUN) && panel.FUN != "panel=") {
    lst$panel = function(x,y,...) {
      arglist = list()
      if(missing(x)) {
        return()
      } else if(missing(y)) {
        arglist$x = x
      } else {
        arglist$x = x; arglist$y = y
      }
      if(panel.FUN != "panel=" && is.character(panel.FUN)) {
        do.call(Paste("panel.",FUN),arglist)
        do.call(Paste("panel.",panel.FUN),arglist)
      }
    }
  } else {
    lst$panel = NULL
  }

  ## plot
  if(!is.null(lst$x)) {
    x = try(do.call(FUN, lst), silent=TRUE)
    if(!inherits(x,"try-error"))
      print(x)            # don't forget print()
    else
      cat("Not ready to plot. Drop another variable?\n")
  }     
}


clearPlot = function(obj) {
  tag(obj,"varlist") <- list()
  dropHandlers = tag(obj,"dropHandlers")
  if(length(dropHandlers) > 0) {
    for(i in 1:length(dropHandlers)) {
      removehandler(dropHandlers[[i]]$view.col,dropHandlers[[i]]$id)
    }
    tag(obj,"dropHandlers") <- list()
  }
  updatePanel(obj)
  panelChooser = tag(obj,"panelChooser"); svalue(panelChooser, index=TRUE)<-1  
  updatedLatticeExplorer(obj)
}

makeEmptyPlot = function(obj) {
#  visible(obj) <- TRUE
  plot.new()
  plot.window(xlim=c(0,1),ylim=c(0,1))
  text(1/2, 1/2, "Drop variables here")
}

clearPanel = function(obj) {
  panelChooser = tag(obj,"panelChooser") # keep in object
  if(!is.null(panelChooser)) {
    panelChooser[]<- c("panel=")
    tag(obj,"panel") <-  NA
  }
}

## available panel functions depends on graph
updatePanel = function(obj) {
  FUN = tag(obj,"function")
  funs = tag(obj,"availPanelFuns")[[FUN]]
  panelChooser = tag(obj,"panelChooser") # kept in object
  if(!is.null(funs))
    panelChooser[] <- c("panel=",funs)
  else
    panelChooser[] <- c("panel=")
}



### testing -- delete me
##gp = ggroup(container=gwindow(v=T))
##add(gp, (vb <- gvarbrowser()))
##add(gp, (adf <- ieditdataframe(mtcars, filter.column=3)),expand=TRUE)
##add(gp, (ile <- ilatticeexplorer()))
