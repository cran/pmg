## pmg interface to iplots
## main functino pmg.iplots

## deal with a data frame
.newSet = function(df, name,container=NULL,...) {

  ## globals ###########################
  plotList = list()                     # list of made plots, for getting names

  setISetByName = function(name) {
    if(name %in% names(iset.list())){
      iset.set(which(name == names(iset.list())))
    }
  }

  ## set up a new iset if not there
  if(name %in% names(iset.list())){
    iset.set(which(name == names(iset.list())))
  } else {
    iset.new(name=name)
    iset.df(df)
  }
  
  ################################################
  ## objects
  dfnames = names(df)
  namesDF = data.frame(name=dfnames,type=sapply(df,class),
    stringsAsFactors = FALSE)

  varNames = gtable(namesDF, multiple=TRUE)
  adddropsource(varNames)
    
  ## plot selector
  availPlots = c("select plot","ibar","ihist","ibox","imosaic","ipcp","ihammock","iplot")
  newPlotSelector = gdroplist(availPlots)

  ## showMadePlots
  showCurPlotIcon = gimage("symbol_none",dirname="stock")
  showCurPlotName = glabel("click to update")
  nextPlot = gimage("rarrow",dir="stock")
  curPlot  = gimage("uarrow",dir="stock")
  prevPlot = gimage("larrow",dir="stock")

  
  ## add objects
  availObjects = c("","ilines","iabline","itext","remove last added")
  addObjectsSelector = gdroplist(availObjects)

  ## list objects
  showCurObject = glabel("")
  nextObject = gimage("rarrow",dir="stock")
  curObject  = gimage("uarrow",dir="stock")
  prevObject = gimage("larrow",dir="stock")
  
  ## selections
  getSelectedButton = gbutton("get selected")
  setSelectedButton = gbutton("set selected")
  showSelected = gtext("")


  ## layout #########################
  lgroup = ggroup(horizontal=FALSE)
  rgroup = ggroup(horizontal=FALSE)
  gp = gpanedgroup(lgroup, rgroup, cont=container) # return this
  
  ## left group shows variable names
  add(lgroup,varNames,expand=TRUE)
  size(varNames) <- c(200,300)
  size(gp) <- c(450,350)
  ## right group shows options

  ## non layout for now
  ## new plot
  tmp = gframe("New plot", container=rgroup)
  add(tmp, newPlotSelector)

  ## show plots
  tmp = gframe("Current plot", container=rgroup)
  plotGroup = ggroup(container=tmp)
  add(plotGroup, prevPlot)
  add(plotGroup, curPlot)
  add(plotGroup, nextPlot)
  hg = ggroup(horizontal=FALSE, cont=plotGroup)
  add(hg, showCurPlotIcon)
  add(hg, showCurPlotName)

  
  ## add object
  tmp = gframe("add object to plot", container=rgroup)
  add(tmp,addObjectsSelector)

  ## list objects
##   tmp = gframe("Current object", container=rgroup)
##   objGroup = ggroup(container=tmp)
## #  add(objGroup, prevObject)
##   add(objGroup, curObject)
## #  add(objGroup, nextObject)
##   add(objGroup, showCurObject)

  

  tmp = gframe("Selections", container=rgroup)
  selGroup = ggroup(horizontal=FALSE,container=tmp)
  selButtonGroup = ggroup(cont=selGroup)
  add(selButtonGroup, getSelectedButton)
  add(selButtonGroup, setSelectedButton)
  add(selGroup, showSelected, expand=TRUE)


  #############################################
  ## actions
  ## plotselection
  addhandlerchanged(newPlotSelector,handler=function(h,...) {
    typeOfPlot = svalue(newPlotSelector)
    if(typeOfPlot == availPlots[1]) {
      return()                          # select plot choice
    }

    ## search through names of isets to set the proper iset


    
    varsIndex = svalue(varNames, index=TRUE)

    ## we have to fuss around to actually get the proper names into
    ## the plots.

    setISetByName(name)


    tmpEnvir = environment()
    
    x.name = ""; y.name = ""
    
    
    if(typeOfPlot == "ibox") {
      ## check for a factor
      if(length(varsIndex) == 2) {
        if(namesDF[varsIndex[1],2] == "factor") {
          x.name = dfnames[varsIndex[2]]
          assign(x.name,df[,varsIndex[2]], envir=tmpEnvir)
          y.name = dfnames[varsIndex[1]]
          assign(y.name,df[,varsIndex[1]], envir=tmpEnvir)
          theNewPlot = eval(parse(text=
            paste(typeOfPlot,"(",x.name,",",y.name,")")),
            envir=tmpEnvir)
        } else if(namesDF[varsIndex[2],2] == "factor") {
          x.name = dfnames[varsIndex[1]]
          assign(x.name,df[,varsIndex[1]], envir=tmpEnvir)
          y.name = dfnames[varsIndex[2]]
          assign(y.name,df[,varsIndex[2]], envir=tmpEnvir)
          theNewPlot = eval(parse(text=
            paste(typeOfPlot,"(",x.name,",",y.name,")")),
            envir=tmpEnvir)
        }
      } else {
        ## build up a data frame with proper names. do.call isn't
        ## doing this correctly
        lst = list()
        for(i in varsIndex) {
          lst[[dfnames[i]]] = df[,i]
        }
        lst = as.data.frame(lst)
        theNewPlot = ibox(lst)
      }
    } else {
      if(typeOfPlot %in% c("ibar","ihist")) {
        x.name = dfnames[varsIndex[1]]
        assign(x.name,df[,varsIndex[1]], envir=tmpEnvir)
        theNewPlot = eval(parse(text=
          paste(typeOfPlot,"(",x.name,")")),
          envir=tmpEnvir)
      } else if(typeOfPlot == "iplot") {
        x.name = dfnames[varsIndex[1]]
        assign(x.name,df[,varsIndex[1]], envir=tmpEnvir)
        y.name = dfnames[varsIndex[2]]
        assign(y.name,df[,varsIndex[2]], envir=tmpEnvir)
        theNewPlot = eval(parse(text=
          paste(typeOfPlot,"(",x.name,",",y.name,")")),
          envir=tmpEnvir)
      } else {
        ## imosaic, ihammock or ipcp
        ## TODO should check that pcp has only continuous vars

        lst = list()
        for(i in varsIndex) {
          lst[[dfnames[i]]] = df[,i]
        }
        lst = as.data.frame(lst)
        theNewPlot = switch(typeOfPlot,
          "imosaic"=imosaic(lst),
          "ipcp" = ipcp(lst)
          )
      }
    }

    ## record newPlot
    plotList = c(plotList, theNewPlot)

    svalue(newPlotSelector, index=TRUE) <- 1
  })


  ## selection
  addhandlerclicked(getSelectedButton,handler=function(h,...) {
    ## show selected in text box
    ## show wrapped in c()

    setISetByName(name)
    theSelected = try(iset.selected(),silent=TRUE)
    if(inherits(theSelected,"try-error"))
      theSelected = "c()"
    else
      theSelected = paste("c(",paste(theSelected,collapse=", "),")",sep="",collapse="")

    svalue(showSelected) <- theSelected

                    
  })

  addhandlerclicked(setSelectedButton, handler=function(h,...) {

    setISetByName(name)

    ## set selected from text box
    tmp = svalue(showSelected)
    tmp = gsub("\n","",tmp)
    val = try(eval(parse(text=tmp),envir=df),silent=TRUE)
    if(inherits(val,"try-error"))  {
      cat("Error with selection")
    } else {
      iset.select(val)
    }
  })

  ## plot selections
  addhandlerclicked(prevPlot,handler= function(h,...) {
    setISetByName(name)
    current = iplot.cur()
    if(current == 1)
      current = length(iplot.list())
    else
      current = current - 1
    iplot.set(current)
    updateCurrentPlotDescription()
  })
  addhandlerclicked(curPlot,handler= function(h,...) {
    setISetByName(name)
    updateCurrentPlotDescription()
  })
  addhandlerclicked(nextPlot,handler= function(h,...) {
    setISetByName(name)
    current = iplot.cur()
    if(current == length(iplot.list()))
      current = 1
    else
      current = current + 1
    iplot.set(current)
    updateCurrentPlotDescription()
    updateCurrentPlotDescription()
  })

  ## add objects
  addhandlerchanged(addObjectsSelector,handler=function(h,...) {
    setISetByName(name)

    ## we do different things based on request here
    addThis = svalue(addObjectsSelector)

    if(addThis == "") return()

    curPlotType = getCurrentPlotNameType()[2]
    if(addThis == "ilines") {
      ## give option to add to scatterplot, or provide x, y
      win = pmgWC$new("Add lines to current iplot")
      gp = ggroup(horizontal = FALSE, cont=win, raise.on.dragmotion = TRUE)
      if(curPlotType == "ixyplot") {
        spframe = gframe("Add trend line",horizontal=FALSE,cont=gp)
        varGroup = ggroup(container=spframe)
        glabel("x=",cont=varGroup)
        xVar = glabel("x-variable", cont=varGroup,editable=TRUE)
        adddroptarget(xVar)
        font(xVar) <-  list(style="bold")
        glabel(", ", cont=varGroup)
        glabel("y=",cont=varGroup)
        yVar = glabel("y-variable", cont=varGroup,editable=TRUE)
        adddroptarget(yVar)
        font(yVar) <-  list(style="bold")
        useSelected = gcheckbox("consider selected values only",cont=spframe)
        bgroup = ggroup(cont=gp)
        gbutton("Add regression line",cont=bgroup,
                handler=function(h,...) {
                  addTrendLine("lm")
                })
        gbutton("Add lowess line",cont=bgroup,
                handler=function(h,...) {
                  addTrendLine("lowess")
                })
        addTrendLine = function(type) {
          xvariable = svalue(xVar)
          yvariable = svalue(yVar)
          if(xvariable == "x-variable" ||
             yvariable == "y-variable") {
            cat("need to have two variables to add trend line\n")
            return(TRUE)
          }
          if(svalue(useSelected)) {
            theSelected = try(iset.selected(),silent=TRUE)
            if(inherits(theSelected,"try-error"))
              theSelected = 1:nrow(df)
            restDF = subset(df,select=c(xvariable,yvariable),
              subset=rep(TRUE,nrow(df))[theSelected])
          } else {
            restDF = subset(df,select=c(xvariable,yvariable))
          }
          if(type == "lm") {
            res = lm(restDF[,2] ~ restDF[,1])
            iabline(res)
          } else if(type == "lowess") {
            ilines(lowess(restDF[,1], restDF[,2]))
          }
        }
      }
      ## now offer to add lines
      slgp = gframe("Specify line to add",cont=gp)
      addThese = gedit("",cont=slgp)
      gbutton("add",cont=slgp,handler=function(h,...) {
        eval(parse(text=paste("ilines(",svalue(addThese),")")),envir=df)
      })
      delGroup = ggroup(cont=gp)
      addSpring(delGroup)
      gbutton("cancel",cont=delGroup, handler=function(h,...) dispose(win))
    } else if(addThis == "iabline") {
      ## FIXME
      cat("That action needs to be programmed.")
    } else if(addThis == "itext") {
      ## FIXME
      cat("That action needs to be programmed.")
    } else if(addThis == "remove last added") {
      iobj.rm()
    } else {
      cat("That action needs to be programmed.")
    }
    svalue(addObjectsSelector, index=1) <- 1
  })


  ## current objects
##   addhandlerclicked(prevObject,handler=function(h,...) {
##     ## now way to set
##     updateCurrentObjectType()    
##   })
  addhandlerclicked(curObject,handler=function(h,...) {
    x = iobj.cur()
    cur = .jstrVal(x$obj)                 # from print.iobj
    svalue(showCurObject) <- cur
  })
##   addhandlerclicked(prevObject,handler=function(h,...) {
##     current = iobj.cur()
##     current = ifelse(current ==  length(iobj.list()),1,current+1)
##     updateCurrentObjectType()
##   })

  
  ##################################################
  ## helpers
  getCurrentPlotNameType = function() {
    curDetails = iplot.list()[[iplot.cur()]]
    theType = class(curDetails)[2]
    theName = attr(curDetails,"iname")
    return(c(theName,theType))
  }
  updateCurrentPlotDescription = function() {
    ## update label and icon to match current
    curDetails = getCurrentPlotNameType()
    ## do icon
    switch(curDetails[2],
           "ibar"=svalue(showCurPlotIcon) <- "barplot",
           "ihist"=svalue(showCurPlotIcon) <- "hist",
           "ibox"=svalue(showCurPlotIcon) <- "boxplot",
           "imosaic"=svalue(showCurPlotIcon) <- "plot1",
           "ipcp"=svalue(showCurPlotIcon) <- "plot1",
           "iplot"=svalue(showCurPlotIcon) <- "points" 
           )
    ## do label
    svalue(showCurPlotName) <- curDetails[1]
    return(TRUE)                        # for handlers
  }

  ## obj
  updateCurrentObjectType = function() {
  }

  
  ## return container
  return(gp)
}
  

pmg.iplots = function(container = pmgWC$new("PMG: iplots interface"), envir=.GlobalEnv) {

  do.call("require",list("iplots"))                       # load if not loaded
  

  ## we have name, data. Now set up a new iset for this.
  
  group = ggroup(horizontal = FALSE, container=container)
  ## simple toolbar
  tbl = list()
  tbl$Add$icon="add"
  tbl$Add$handler= function(...) addNewDF()
  tbl$Quit$icon = "quit"
  tbl$Quit$handler = function(...) dispose(group)
    
  gtoolbar(tbl, cont=group)

  ## notebook
  nb = gnotebook()
  add(group, nb, expand=TRUE)
  ## add instructions
  theInstructions = pmgIplotInstructions()
  size(theInstructions) <- c(450,350)
  add(nb,theInstructions,label="help")
  

  addNewDF = function() {

    ## grab data set names
    objs = ls(envir=.GlobalEnv)
    addDfs = objs[sapply(objs, function(i)
      is.data.frame(get(i, envir=envir)))]
    
    if(length(addDfs) > 0) {
      win = pmgWC$new("Make a new iset around this data frame")
      gp = ggroup(horizontal=FALSE, cont=win)
      add(gp, glabel("Double click on a data frame"))
      
      dfSelector = gtable(addDfs)
      add(gp, dfSelector, expand=TRUE)
      addhandlerdoubleclick(dfSelector,handler = function(h,...) {
        theDF = svalue(dfSelector) ## its name
        df = get(theDF, envir=envir)    # its values
        add(nb,.newSet(df,theDF), label=theDF)
        dispose(win)
      })
    } else {
      gmessage("No data frames available.")
    }
  }
}
  
  
pmgIplotInstructions = function() {
  instructs = gtext()
  add(instructs,"iplots usage", font.attr=c("bold","blue"))
  add(instructs,
      paste("The iplots website says this about the the package:",
            "",
            "\"(it) offers a wide variety of plots, including histograms,",
            "barcharts, scatterplots, boxplots, fluctuation diagrams,",
            "parallel coordinates plots and spineplots. All plots support",
            "interactive features, such as querying, linked highlighting,",
            "color brushing, and interactive changing of parameters.\"",
            "",
            "The pmg interface alows one to create new plots by selecting",
            "from a popup box. After choosing a data frame, and selecting the",
            "variables for the new graphic, simplify changing the ",
            "popup box will produce a new graphic.",
            "There is some functionality for adding a line or text to ",
            "the current graphic. Like other R devices, there is a",
            "current plot device. These may be cycled through by",
            "clicking the buttons.",
            sep="\n",collapse="")
      )
                       

    return(instructs)
}
