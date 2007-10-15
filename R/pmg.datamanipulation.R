## Dialogs for data manipulation


## dialog for finding subsets
## return a group object
pmg.subset.dialog = function(container=NULL) {

  group  = ggroup(horizontal=FALSE, container=container)

  frame = gframe("<b>Data</b>",markup=TRUE, container=group)

  table = glayout()
  table[1,1] = glabel("x=")
  dataEntry = gedit("",width=30)
  table[1,2] = dataEntry
  ## subset
  table[2,1] = glabel("subset=")
  subsetEntry = gedit("",width=30)
  table[2,2] = subsetEntry
  subsetButton = gbutton("edit",handler = function(h,...) {
    editSubsetDialog(data=svalue(dataEntry),
                     widget=subsetEntry)})
  table[2,3] = subsetButton
  ## select
  table[3,1] = glabel("select=")
  selectEntry = gedit("", width=30)
  table[3,2] = selectEntry
  selectButton = gbutton("edit",handler = function(h,..) {
    editSelectDialog(data=svalue(dataEntry),
                     widget=selectEntry
                     )})
  table[3,3] = selectButton

  table[4,1] = glabel("drop=")
  dropEntry = gradio(c("TRUE","FALSE"),index=FALSE,selected=2)
  table[4,2] = dropEntry

  table[5,1] = glabel("assign to:")
  assignEntry = gedit("",width=30)
  table[5,2] = assignEntry
  
  submitButton = gbutton("submit",handler=function(h,...) {
    dataValue = svalue(dataEntry)
    subsetValue = svalue(subsetEntry)
    selectValue = svalue(selectEntry)
    dropValue = svalue(dropEntry)
    assignValue = svalue(assignEntry)
    if(assignValue == "")
      assignValue = NULL
    ## use pmg.cli to evaluate
    if(dataValue == "") {
      warning("No dataset chosen")
      return(NULL)
    }
    string = Paste("subset(", dataValue)
    if(nchar(subsetValue)>0) 
      string = Paste(string,", subset=",subsetValue)
    if(nchar(selectValue)>0)
      string = Paste(string, ", select=",selectValue)
    string = Paste(string,",drop=",dropValue,")")

    names(string) = assignValue
    svalue(pmg.cli) <- string

    ## close dialog?
    ##     if(!is.null(win))
    ##       dispose(win)
  })
  
  table[6,3] = submitButton

  add(frame, table,expand=TRUE)
  visible(table) <-  TRUE

  return(group)
}
  


##################################################

## edit data frame properties

pmg.edit.dataframe.properties.dialog = function(container=NULL) {

  ## in gWIdgetsRGtkw, but not exported?
  lsType = function(type, envir=.GlobalEnv) {
    x = with(.GlobalEnv, sapply(ls(), function(i) class(get(i))))
    objects = names(x)[sapply(x, function(i) any(i %in% type))]
  return(objects)
  }
  
  

  ## need means to select the data frame, popup this for editing

  g = ggroup(horizontal=FALSE, cont=container)

  add(g, glabel("This dialog allows you to change the\n name, and data type for the columns of a data frame."))
  add(g, gseparator())
  
  tbl = glayout(cont=g)

  allDFs = lsType("data.frame")
  selHandler = function(h,...) {
    newDFName = svalue(selectDF)
    .editDFProperties(newDFName)
  }
  selectDF = gdroplist(c("",allDFs), editable=TRUE, handler = selHandler)

  tbl[1,1] = glabel("Select a data frame:")
  tbl[2,1] = selectDF
  tbl[2,2] = gbutton("edit",handler=selHandler)
  
  visible(tbl) <- TRUE
  
  return(g)
  
}

.editDFProperties = function(dfName) {
  
  df = try(get(dfName, envir=.GlobalEnv))

  ## check that df is there
  if(inherits(df,"try-error")) {
    gmessage("Can't find a data frame with that name in the global environment.")
    return()
  }
    
  allTypes = c("","numeric","integer","character","factor","logical")


  win = pmgWC$new("Edit data frame properties")
  g = ggroup(horizontal=FALSE, cont = win)

  lg = ggroup(cont=g)

  
  allNames = gdroplist(names(df), selected = 0)
  newName = gedit("")
  newType = gdroplist(allTypes, selected = 1)

  
  tbl = glayout(cont=g)
  tbl[1,1] = glabel("Which column?")
  tbl[1,2] = allNames

  tbl[2,1] = glabel("new type:")
  tbl[2,2] = newType

  tbl[3,1] = glabel("new name:")
  tbl[3,2] = newName

  visible(tbl) <- TRUE


  add(g, gseparator())
  sg = ggroup(cont=g)
  saveButton = gbutton("save", cont=sg)
  glabel("data frame as:", cont=sg)
  newDfName = glabel(dfName, editable=TRUE, cont=sg)
  font(newDfName)<-list(style="bold")

  ## now add handlers
  addhandlerchanged(allNames, handler = function(h,...) {
    theColName = svalue(allNames)
    svalue(newName) <- theColName
    theData = df[,theColName,drop=TRUE]
    theClass = class(theData)[1]
    if(any(theClass == allTypes)) 
      svalue(newType) <- theClass
    else
      svalue(newType, index=TRUE) <- 1
    return(FALSE)
  })

  addhandlerchanged(newName, handler = function(h,...) {
    theColName = svalue(allNames)
    theNewName = svalue(newName)
    theColNo = which(theColName == names(df))
    names(df)[theColNo] <<- theNewName  

    allNames[] <- names(df)
    svalue(newName) <- ""
    svalue(newType, index=T) <- 1
    return(FALSE)
  })

  addhandlerchanged(newType, handler = function(h,...) {
    theNewType = svalue(newType)
    theColName = svalue(allNames)
    theColNo = which(theColName == names(df))
    n = ncol(df)
    
    theData = df[,theColNo]
    oldType = class(theData)[1]
    if(theNewType != "" && theNewType != oldType) {
      theData = do.call(paste("as.",theNewType,sep="",collapse=""),list(theData))

      if(theColNo > 1 && theColNo < n) {
        newDF = data.frame(df[,1:(theColNo-1),drop=FALSE], theData, df[,(theColNo+1):n, drop=FALSE], stringsAsFactors = FALSE)
      } else if(theColNoNo == 1) {
        newDF = data.frame(theData, df[,(theColNo+1):n, drop=FALSE], stringsAsFactors = FALSE)
      } else {
        newDF = data.frame(df[,1:(theColNo-1), drop=FALSE], theData, stringsAsFactors = FALSE)
      }
      names(newDF)[theColNo] <- theColName
      df <<- newDF
    }
  })

  addhandlerchanged(saveButton, handler = function(h,...) {

    ## change the name, in may not have been
    theNewName = svalue(newName)
    if(theNewName != "") {
      theColName = svalue(allNames)
      theColNo = which(theColName == names(df))
      names(df)[theColNo] <<- theNewName  
    }
    
    assign(svalue(newDfName), df, envir=.GlobalEnv)
    dispose(win)
  })


}
  
