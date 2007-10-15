## NOT WORKING!!! -- can't get eval command to work. Gives
## error about scale

qplotGUI = function(container = NULL, ...) {
  do.call("require",list("ggplot2"))
  
  ## globals
  widgets = list()
  extraArgsWidgets = list()
  geomVals1d = data.frame(geoms=c("histogram","density"),stringsAsFactors=FALSE)
  geomVals2d = data.frame(geoms=c("point","smooth","boxplot","quantile","line","path","density2d","jitter"), stringsAsFactors=FALSE)
  
  
  ## handler for getting variable using either data or globalenv
  getValueFromString = function(str) {
    if(str == "") return(NA)
    
    dfName = svalue(widgets[['data']])
    if(dfName != "") {
      ## there is a data frame
      df = get(dfName, envir=.GlobalEnv)
      ret = with(df, try(eval(parse(text=str)), silent=TRUE))
    } else {
      ## get from global environment
      try(eval(parse(text=str), envir=.GlobalEnv), silent=TRUE)
    }
    return(ret)
  }
  

  nb = gnotebook(cont=container, tab.pos = 1, ...)

  ## main group
  qpg = ggroup(horizontal=FALSE, cont=nb, label="qplot")
  parg =ggroup(horizontal=FALSE, cont=nb, label="plot args")

  ## work on qplot group
  tbl = glayout(cont=qpg)
  
  ## x, y
  tbl[1,1, anchor=c(1,0)] <- "x"
  tbl[1,2] <- (widgets[['x']] <- gdroplist(c(),editable=TRUE, cont=tbl))
  
  tbl[1,3, anchor=c(1,0)] <- "y"
  tbl[1,4] <- (widgets[['y']] <- gdroplist(c(),editable=TRUE, cont=tbl))
  
  ## data
  tbl[2,1, anchor=c(1,0)] <- "data"
  tbl[2,2] <- (widgets[['data']] <- gedit("", cont=tbl))
  
  ## weigths
  tbl[2,3, anchor=c(1,0)] <- "weights"
  tbl[2,4] <- (widgets[['weights']] <- gdroplist(c(),editable=TRUE, cont=tbl))
  
  
  tbl[3,1:4] <- gseparator(cont=tbl)
  
  ## geom
  tbl[4,1, anchor=c(1,0)] <- "geom"
  tbl[4:8,2] <- (widgets[['geom']] <- gtable(geomVals1d,multiple=TRUE,cont=tbl))
  size(widgets[['geom']]) <- c(150,125)
  
  ## stat
  tbl[4,3, anchor = c(1,0)] <- "args"
  tbl[4:8, 4] <- (widgets[['args']] <- gnotebook(tab.pos=3,cont=tbl))
  
  tbl[9,1:4] <- gseparator(cont=tbl)
  
  ## facet
  tbl[10,1, anchor = c(1,0)] <- "facets"
  tbl[10,2:4] <- (fgp <- ggroup(horizontal=TRUE, cont=tbl))
  widgets[['fresp']] <- gdroplist(".",editable=TRUE, cont=fgp)
  glabel("~", cont=fgp)
  widgets[['fpred']] <- gdroplist(".",editable=TRUE, cont=fgp)
  gbutton("edit",cont=fgp, handler = function(h,...) {
    if(svalue(widgets[['data']]) != "") {
      gWidgets:::editFormulaDialog(data = widgets[['data']],
                                   responsewidget = widgets[['fresp']],
                                   predictorwidget = widgets[['fpred']])
    } else {
      cat("Data is empty, facet needs to have  a data frame set\n")
    }
  })
  
  
  ## add
#  tbl[11,1:4] <- gseparator(cont=tbl)
#  tbl[15,1,anchor=c(1,0)] <- "add"
#  tbl[15,2] <- (widgets[['add']] <- gedit("",cont=tbl))
  
  ## for RGtk2
  visible(tbl) <- TRUE
  
  
  ## plot args tab
  tbl = glayout(cont=parg)
  
  tbl[1,1,anchor=c(1,0)] <- "xlim"
  tbl[1,2] <- (widgets[['xlim']] <- gedit("", cont=tbl))
  
  tbl[1,3,anchor=c(1,0)] <- "ylim"
  tbl[1,4] <- (widgets[['ylim']] <- gedit("", cont=tbl))
  
  tbl[2,1,anchor=c(1,0)] <- "log"
  tbl[2,2] <- (widgets[['log']] <- gdroplist(c("","x","y","xy"),cont=tbl))
  
  tbl[3,1:4] <- gseparator(cont=tbl)
  
  tbl[4,1,anchor=c(1,0)] <- "main"
  tbl[4,2:4] <- (widgets[['main']] <- gedit("",cont=tbl))
  
  tbl[5,1,anchor=c(1,0)] <- "xlab"
  tbl[5,2] <- (widgets[['xlab']] <- gedit("", cont=tbl))
  
  tbl[5,3,anchor=c(1,0)] <- "ylab"
  tbl[5,4] <- (widgets[['ylab']] <- gedit("", cont=tbl))
  
  tbl[6,1:4] <- gseparator(cont=tbl)
  
  tbl[7,1, anchor=c(1,0)] <- "margins"
  tbl[7,2] <- (widgets[['margins']] <-
               gdroplist(c("", TRUE, "'grand_row'","'grand_col'"),
                         editable=TRUE, cont=tbl))
  
  tbl[8,1:4] <- gseparator(cont=tbl)
  ## ## colour size shape linetype
  ## tbl[9,1,anchor=c(1,0)] <- "colour"
  ## tbl[9,2] <- (widgets[['colour']] <- gdroplist(c(), editable=TRUE, cont=tbl))
  
  ## tbl[9,3,anchor=c(1,0)] <- "size"
  ## tbl[9,4] <- (widgets[['size']] <- gdroplist(c(), editable=TRUE, cont=tbl))
  
  ## tbl[10,1,anchor=c(1,0)] <- "shape"
  ## tbl[10,2] <- (widgets[['shape']] <- gdroplist(c(), editable=TRUE, cont=tbl))
  
  ## tbl[10,3,anchor=c(1,0)] <- "linetype"
  ## tbl[10,4] <- (widgets[['linetype']] <- gdroplist(c(), editable=TRUE, cont=tbl))
  
  ## RGtk2
  visible(tbl) <- TRUE

  ## set tab
  svalue(nb) <- 1

  ##
############ end layout ####################
  
  ## useful functions

  ## update variables based on value of data
  updateVarNames = function(dataVal) {
    theNames = c()
    if(length(dataVal) == 0 || dataVal == "") {
      ## .globalEnv -- list all of them
      is.variable = function(i) is.numeric(i) || is.factor(i) 
      tmp = sapply(ls(envir=.GlobalEnv),function(i) {
        is.variable(get(i))})
      theNames = names(tmp)[tmp]
    } else {
      ## try to see if data has names
      theData = try(get(dataVal,envir=.GlobalEnv), silent=TRUE)
      if(!inherits(theData,"try-error")) {
        tmp = try(names(theData), silent=TRUE)
        if(!inherits(tmp,"try-error")) theNames = names(theData)
      }
    }

    ## add to x, y, fresp, fpred, colour, size, linetype
    if(length(theNames) > 0) 
      sapply(c("x","y","weights"), ##,"colour","size", "shape","linetype"),
             function(i) 
             widgets[[i]][] <- c("",theNames))
    sapply(c("fresp","fpred"), function(i) # . is default
           widgets[[i]][] <- c(".",theNames))
    
    invisible()
  }
  
  
  updateGeoms = function(...) {
    yVal = svalue(widgets[['y']])
    cat("DEBUG: what is yval:",yVal,"\n")
    if(yVal == "")
      widgets[['geom']][,] <- geomVals1d
    else
      widgets[['geom']][,] <- geomVals2d
  }
  
  
  ## update the args to match values selected by geom
  extraArgConfig = list(
    histogram = c("binwidth"),
    density = c(),
    point = c(),
    smooth = c("method"),
    boxplot = c(),
    quantile = c("formula", "quantile"),
    line = c(),
    path = c(),
    density2d = c()
    )
  
  
  
  updateExtraArgs = function(...) {
    
    selGeoms = svalue(widgets[['geom']])
    anb = widgets[['args']]
    if(length(selGeoms) == 0) {
      if( (n <- length(anb)) > 0)
        for(i in n:1) {svalue(anb) <- i; dispose(anb)}
    }
    
    doThese = unique(unlist(extraArgConfig[selGeoms]))
    ## delete those not there
    currentOnes = names(anb)
    if(length(doThese) == 0) {
      ## delete notebook pages, adn all of extraArgsWidgets
      extraArgsWidgets <<- list()
      n = length(anb)
      if(n > 0) {
        for(i in n:1) {
          svalue(anb) <- i; dispose(anb)
        }
      }
    } else {
      deleteThese = setdiff(currentOnes, doThese)
      addThese = setdiff(doThese, currentOnes)
      
      ## delete
      if(length(deleteThese) >0) {
        for(i in rev(which(deleteThese == currentOnes))) {
          svalue(anb) <- i; dispose(anb)
        }
        for(i in deleteThese) {
          extraArgsWidgets[[i]] <<- NULL
        }
      }
      
      
      ## add new ones
      for(i in addThese) {
        eg = ggroup(horizontal=FALSE, cont=anb, label=i)
        ## different based on thing.
        if(i == "method") {
          ## just do lm
          glabel("Using lm method", cont=eg)
          tmp =  ggroup(cont=eg)
          glabel("formula", cont=tmp)
          extraArgsWidgets[['formula']] <<-
            gdroplist(c("",
                        "y ~ ns(x,1)",
                        "y ~ ns(x,2)",
                        "y ~ ns(x,3)"),
                      coerce.with = function(str) {
                        if(str == "")
                          return(str)
                        else
                          return(formula(str))
                      },
                      handler = updateGraphic,
                      editable=TRUE, cont=tmp)
        } else if(i ==  "quantile") {
          tmp = ggroup(cont=eg)
          glabel("quantile", cont=tmp)
          extraArgsWidgets[["quantile"]] <<-
            gdroplist(
                      c("",
                        "seq(.05,.95,by=.05)",
                        "seq(.10,.90,by=.10)",
                        "seq(.25,.75,by=.25)"),
                      coerce.with = function(str) {
                        ifelse(str=="","",try(eval(parse(text=str)),silent=TRUE))
                      },
                      handler = updateGraphic,
                      editable=TRUE, cont=tmp)
        } else if(i == "binwidth") {
          tmp = ggroup(cont=eg)
          glabel("binwidth", cont=tmp)
          extraArgsWidgets[["binwidth"]] <<- gedit("", cont=tmp,
                                                   coerce.with=as.numeric,
                                                   handler=updateGraphic)
        } else if(i == "formula") {
          tmp = ggroup(cont=eg)
          glabel("formula", cont=tmp)
          extraArgsWidgets[["quantile"]] <<-
            gdroplist(
                      c("",
                        "y ~ ns(x,1)",
                        "y ~ ns(x,2)",
                        "y ~ ns(x,3)"),
                      coerce.with = function(str) {
                        ifelse(str == "", "", formula(str))
                      },
                      handler = updateGraphic,
                      editable=TRUE, cont=tmp)
          
        }
      }
    }
  }
  
  ## get values for args notebook. Return a list with values
  getExtraArgValues = function() {
    tmp = list()
    
    if(length(extraArgsWidgets) == 0) return(tmp)
    
    for(i in names(extraArgsWidgets)) {
      val = svalue(extraArgsWidgets[[i]])

      if(is.na(val) ||
         is.null(val) ||
         (is.character(val) && val[1] == "")) {
        ## what?
      } else {
        tmp[[i]] <- val
      }
    }
    
    ## fix up method
    if("smooth" %in% svalue(widgets[['geom']]) &&
       !is.null(tmp$formula) &&
       tmp$formula != "") {
      tmp$method <- "lm"
    }
    
    return(tmp)
  }
  
  ## Key to this is handling the different geoms and the new infor we add to them
  ## smooth loess(span (in [0,1]), lm( y ~ poly(x,1) default)
  ## quantile (formula y ~ poly(x,1)


  ## Grab values and make a plot
  updateGraphic = function(...) {
    ## first make sure this is current
    updateExtraArgs()


    
    tmp = lapply(widgets, svalue)

    ## updateVarNames
    updateVarNames(tmp$data)
    
    ## check if we can
    if(length(tmp$geom) == 0 || tmp$x == "") {
      cat("need to specify a variable or geom\n")
      return()
    }


    l = list()                      # store args here
    if(tmp$data != "") {
      ## there is a data frame
      df = get(tmp$data, envir=.GlobalEnv)
      l$data <- df
      l$x = with(df, try(eval(parse(text=tmp$x)), silent=TRUE))
      ## same for y
      if(tmp$y != "")
        l$y = with(df, try(eval(parse(text=tmp$y)),silent=TRUE))
    } else {
      ## get names from global environment
      l$x <- try(eval(parse(text=tmp$x), envir=.GlobalEnv), silent=TRUE)
      if(tmp$y != "")
        l$y <- try(eval(parse(text=tmp$x), envir=.GlobalEnv), silent=TRUE)
    }



    ## fix x, y labels
    l$xlab = ifelse(tmp$xlab == "", tmp$x, tmp$xlab)
    l$ylab = ifelse(tmp$ylab == "", tmp$y, tmp$ylab)
    
    ## done with x, y, data, xlab, ylab
    ## now handle facets
    if(tmp$fresp != "" && tmp$fpred != "")
      l$facets = formula(paste(tmp$fresp, "~", tmp$fpred, sep="  "))
    
    ## deal with extra arguments to geoms
    if(length(tmp$args) > 0) {
      ## we have extra arguments
      
      res = getExtraArgValues()
      for(i in names(res))
        l[[i]] <- res[[i]]
    }
    
    
    ## now add in the rest of the arguments
    trimIt = function(i) is.null(i) || (length(i) == 1 && i == "")
    
    tmp$x <- tmp$y <- tmp$data <- tmp$xlab <- tmp$ylab <- NULL
    tmp$fresp <- tmp$fpred <- NULL
    tmp$args <- NULL
    
    ## How to deal with these and do.call? They label doesn't work correctly
    
    ##   ## These numeric guys we do separately and in the calling environment
    ##   for(i in c("colour","size","shape","linetype")) {
    ##     if(tmp[[i]] != "") {
    ##       l[[i]] <- getValueFromString(tmp[[i]])
    ##       tmp[[i]] <- NULL
    ##     }
    ##   }
    
    ## the rest
    tmp = tmp[!sapply(tmp,trimIt)]
    for(i in names(tmp)) {
      l[[i]] <- tmp[[i]]
    }
    
    ## make graphic
    ret = try(print(do.call("qplot",l)), silent=TRUE)
    if(inherits(ret,"try-error")) {
      cat("Error with qplot:",ret,"\n")
      return()
    }
  }
   
  updateGraphic.paste = function(...) {
    tmp = lapply(widgets, svalue)
 
    ## check if we can
    if(length(tmp$geom) == 0 || tmp$x == "") {
      cat("need to specify a variable or geom\n")
      return()
    }
 
    ## try to paste together all the arguments
    ## then call within with
 
    cmd = paste("print(qplot(x=",tmp$x, sep="")
    tmp$x <- NULL
 
    pasteIfNotNull = function(lab,val) {
      if(!is.null(val) && val != "")
        cmd <<- paste(cmd,", ",lab,"=",val, sep="")
    }
 
    ## handle y, data, geoms, facets separately
    pasteIfNotNull("y", tmp$y)
    tmp$y <- NULL
 
    if(tmp$data != "") {
      df = get(tmp$data, envir=.GlobalEnv)
    } else {
      df = .GlobalEnv
    }
    tmp$data = NULL
 
    ## geoms
    pasteIfNotNull("geom",
                   paste("c('",paste(tmp$geom, collapse="', '"),"')", sep="")
                   )
    tmp$geom <- NULL
    tmp$args <- NULL
 
    if(tmp$fresp != "" && tmp$fpred != "") 
      pasteIfNotNull("facets",paste(tmp$fresp, "~", tmp$fpred, sep="  "))
    tmp$fresp <- tmp$fpred <- NULL
 
    ## add the rest
    for(i in names(tmp)) {
      pasteIfNotNull(i, tmp[[i]])
    }
 
    cmd = paste(cmd, "))", sep="")
 
    print(cmd)
    with(df, eval(parse(text=cmd)))
 
  }


  ## add handlers
  
  
  
  ## update graphic -- error checking inside updateGraphic
  sapply(names(widgets), function(i) {
    ## treat "y", "data" separately for tcltk
    if(i != "y" || i != "data")
      addHandlerChanged(widgets[[i]], handler = function(h,...) {
        updateGraphic()
      })
  })
  
  
  addHandlerClicked(widgets[['geom']], handler = function(h,...) {
    updateGraphic()
  })
  
  
  ## y --> change from 1d to 2d geoms
  addHandlerChanged(widgets[['y']], handler = function(h,...) {
    updateGeoms()
  })
  
  ## data --> update variable names
  addHandlerChanged(widgets[['data']], handler=function(h,...) {
    updateVarNames(svalue(h$obj))
    updateGraphic()
  })
  addDropTarget(widgets[['data']], handler = function(h,...) {
    updateVarNames(h$dropdata)
    updateGraphic()
  })


  ## all done, return top container
  return(nb)
  
}



## ## partial GUI for qplot function
## require(ggplot2)
## options("guiToolkit"="RGtk2")
## require(gWidgets)

## ##Usage:
## ##
## ## qplot(
## ## x, y = NULL, z=NULL, ..., data,
## ## facets = . ~ .,
## ## margins=FALSE,
## ## geom = "point",
## ## stat=list(NULL),
## ## position=list(NULL),

## ## -- par tab 
## ##xlim = c(NA, NA), ylim = c(NA, NA), log = "",
## ## main = NULL, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)),

## ##add=NULL)






## ## set up top level
## win = gwindow("qplot GUI", width=700, height=400)
## g = ggroup(horizontal=FALSE, cont=win, expand=TRUE)  # main group


## tb = list()
## tb$Quit$handler = function(h,...) dispose(win)
## tb$Quit$icon = "quit"
## tb = gtoolbar(tb, cont=g)


## pg = gpanedgroup(cont=g, exand=TRUE)
## vb = gvarbrowser(cont=pg)                    # left varbrowser

## nb = qplotGUI(cont = pg)
