## dialog for finding subsets
## return a group object
pmg.subset.dialog = function(container=NULL) {

  group  = ggroup(horizontal=FALSE, container=container)

  frame = gframe("<b>Data</b>",markup=TRUE, container=group)

  table = glayout()
  table[1,1] = glabel("x=")
  dataEntry = gedit("",width=30)
  table[1,2] = dataEntry

  table[2,1] = glabel("subset=")
  subsetEntry = gedit("",width=30)
  table[2,2] = subsetEntry
  subsetButton = gbutton("edit",handler = function(h,...) {
    editSubsetDialog(data=h$action$dataEntry,
                     widget=h$action$subsetEntry)
  }, action=list(dataEntry = dataEntry, subsetEntry = subsetEntry))
  table[2,3] = subsetButton

  table[3,1] = glabel("select=")
  selectEntry = gedit("", width=30)
  table[3,2] = selectEntry
  selectButton = gbutton("edit",handler = function(h,..) {
    editSelectDialog(data=h$action$dataEntry,
                     widget=h$action$selectEntry
                     )
  }, action=list(dataEntry = dataEntry, selectEntry = selectEntry))
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

    if(!is.null(window))
      dispose(window)
  })
  
  table[6,3] = submitButton

  add(frame, table,expand=TRUE)
  visible(table) <-  TRUE

  return(group)
}
  
