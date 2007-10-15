##file import Wizard
##Uses BasicWidgets
##call pmg.specifyFileForImport to start it off
fileExtensions =  list(
  "text files" = c("csv","txt","fwf"),
  "ARFF files" = c("arff"),
  "DBF files" = c("dbf"),
  "Stata Binary files" = c("dta"),
  "EPI info files" = c("epi"),
  "Minitab Portable files" = c("mtp"),
  "Octave text data files" = c("octave"),
  "SPSS files" = c("sav"),
  "SAS XPORT files" = c("xport"),
  "Systat files" = c("sys","syd"),
  "Excel files" = c("xls"),
  "DIF files" = c("DIF","dif"),
  "Open office files" = c("odt"),
  "gnumeric files" = c("gnumeric")
  )
## strip last character
pop = function(x) x[-length(x)]
popchar = function(str) paste(pop(unlist(strsplit(str,""))),collapse="")

selectFile = function(initialFile = NULL) {

  filterList = lapply(fileExtensions, function(i) list(patterns = paste("*.",i,sep="")))
  filterList$"All files" = list(patterns=c("*"))
  gfile(text = "Select a file for import",
        initialfilename = initialFile,
        filter = filterList
        )
}


## specify with a URL or a filebrowse
pmg.specifyFileForImport = function(...) {

  filterList = lapply(fileExtensions, function(i) list(patterns = paste("*.",i,sep="")))
  filterList$"All files" = list(patterns=c("*"))

  GUI = BasicGUI$new(message="Select a file to import")
  GUI$filterList = filterList
  GUI$useDefaultText = "<use file extension to determine>"
  GUI$fileSelectDefaultText = "Specify a file or url..."
  GUI$makeBody = function(., container) {
    g = ggroup(horizontal=FALSE, cont=container)
    f = gframe("File", cont=g)
    tbl = glayout(cont=f)
    tbl[1,1] <- "local file"
    tbl[1,2] <- (.$filebrowse = gfilebrowse(text=.$fileSelectDefaultText,
                   action=invisible,
                   container=tbl, filter=.$filterList, quote=FALSE))
    tbl[2,1] <- (l <- glabel("or",cont=tbl))
    font(l) <- c(style="italic")
    tbl[2,2] <- gseparator(cont=tbl)
    tbl[3,1] <- "url"
    tbl[3,2] <- (.$url = gedit("", container=tbl))

    tbl[4,1:2] <- gseparator(cont=tbl)
    tbl[5,1] = "File type is"
    tbl[5,2] <- (.$filetype = gdroplist(c(
      .$useDefaultText,
      sapply(names(filterList),popchar)
      ), cont=tbl))

    visible(tbl) <- TRUE
  }
  GUI$okButtonHandler = function(.,h,...) {
    ## what to do? need *local* filename and type
    ## if url, but no file, then we download file name it, go
    ## if file then go to next

    .$theFile = svalue(.$filebrowse)
    theURL = svalue(.$url)
    .$ext = NULL ## the extension, figure out


    if(.$theFile == .$fileSelectDefaultText || !file.exists(.$theFile)) {
      ## try to get the URL
      .$theFile= tempfile()
      out = try(download.file(theURL, destfile = .$theFile))
      if(inherits(out,"try-error")) {
        cat("Error downloading file:",out,"\n")
        return(TRUE)
      }
      ## we saved to out
      ## guess extension from $url
      tmp = unlist(strsplit(basename(theURL), split="\\."))
      .$ext = tmp[length(tmp)]
    }
    ##  file is now theFile
    ## get extension type from droplist

    fileType = svalue(.$filetype)

    if(fileType != .$useDefaultText) {
      ## use filterList to get
      fileType = paste(fileType,"s", sep="", collapse="") ## append s back
      .$ext = fileExtensions[[fileType]][1]
      cat("Set extension to",.$ext,"\n")
    } else if(is.null(.$ext)) {
      tmp = unlist(strsplit(basename(.$theFile), split="\\."))
      .$ext = tmp[length(tmp)]
    } 
    ## now we have .$theFile and .$ext move on
    dispose(.$window)

    importFile(.$theFile, .$ext)
  }

  ## now draw GUI
  GUI$show()
}


importFile = function(filename, ext=NULL) {

  if(missing(filename))
    filename = selectFile()



  GUI = BasicGUI$new(message=paste("import", filename,collapse=" "))
  GUI$filename = filename
  GUI$ext = ext
  GUI$AssignToText = "Assign to:"
  GUI$okButtonHandler = function(.,h,...) {
    ## the functions below define FUN, args, and varName
    out = try(do.call(.$FUN,lapply(args,svalue)), silent=TRUE)
    if(inherits(out,"try-error")) {
      cat("Error:",out,"\n")
    } else {
      varName = make.names(svalue(.$varName))
      ## can't have empty names due to make.names

      ## check if there already
      curVars = ls(envir=.GlobalEnv)
      if(varName %in% curVars) {
        override = gconfirm(paste("A variable",varName,"already exists. Overwrite?",collpse=""))
        if(override == FALSE)
          return(TRUE)
      }
      assign(make.names(varName),out,envir=.GlobalEnv)
      dispose(.$window) ## clean up
    }
  }
  GUI$makeBody = function(.,container) {
    .$container = container             # store
    ## dispatch various functions depending on type of filename
    if(is.null(.$ext)) {
      tmp = unlist(strsplit(basename(.$filename), split="\\."))
      .$ext = tmp[length(tmp)]
    }
    ## now what is the ext
    switch(.$ext,
           "csv" = .$read_text(sep=","),
           "txt" = .$read_text(sep=""),
           "fwf" = .$read_fwf(sep=","),
           "arff" = .$read_foreign(type="arff"),
           "dbf"= .$read_foreign(type="dbf"),
           "DIF" = .$read_DIF(),
           "dta"= .$read_foreign(type="dta"),
           "epi"= .$read_foreign(type="epi"),
           "mtp"= .$read_foreign(type="mtp"),
           "octave"= .$read_foreign(type="octave"),
           "sav"= .$read_foreign(type="spss"),
           "ssd"= .$read_foreign(type="ssd"),
           "xport"= .$read_foreign(type="xport"),
           "systat"= .$read_foreign(type="systat"),
           "xls"= .$read_spreadsheet(type="xls"),
           "odt" = .$read_spreadsheet(type="odt"),
           "gnumeric" = .$read_spreadsheet(type="gnumeric"),
           .$read_text(sep=""))         # default
  }
  ## each of these has FUN="string", args=list(), varName
  ## will do do.call(FUN,lapply(args,svalue)) to get answer

  ## ITS ONE OF THESE?
  GUI$read_text = function(.,sep) {
    .$FUN = "read.table"
    .$args = list(file = gedit(.$filename))

    ## see ?read.table for numerous arguments

    g = ggroup(horizontal=FALSE, cont=.$container)
    glabel(paste("Read",basename(.$filename),collapse=" "), cont=g)

    tbl <- glayout(cont=g)
    tbl[1,1] <- .$AssignToText
    tbl[1,2] <- (.$varName <- gedit("X", cont=tbl))
    .$varName[] <- ls(envir=.GlobalEnv)
    visible(tbl) <- TRUE

         
    f= gframe("Import", cont=g)
    tbl <- glayout(cont=f)
    tbl[1,1] <- "header"
    tbl[1,2] <- (.$args[['header']] <- gdroplist(c(TRUE,FALSE), cont=tbl))
    tbl[1,3] <- "Skip lines"
    tbl[1,4] <- (.$args[["skip"]] <- gspinbutton(0,1000, cont=tbl))
    tbl[2,1] <- "Strip whitespace"
    tbl[2,2] <- (.$args[['strip.white']] <- gdroplist(c(TRUE,FALSE), cont=tbl))
    tbl[2,3] <- "Skip blank lines"
    tbl[2,4] <- (.$args[['blank.lines.skip']] <- gdroplist(c(TRUE,FALSE), cont=tbl))

    visible(tbl) <- TRUE
    f = gframe("Attributes", cont=g)
    tbl <- glayout(cont=f)
    tbl[1,1] <- "Separator"
    tbl[1,2] <- (.$args[['sep']] <- gedit(sep, cont=tbl))
    tbl[1,3] <- "quote"
    tbl[1,4] <- (.$args[['quote']] <- gedit("\"", cont=tbl))
    tbl[2,1] <- "Decimal point"
    tbl[2,2] <- (.$args[["dec"]] <- gdroplist(c(".",","), cont=tbl))
    tbl[2,3] <- "Comment char."
    tbl[2,4] <- (.$args[['comment.char']] <- gedit("#", cont=tbl))
    tbl[3,1] <- "NA string"
    tbl[3,2] <- (.$args[['na.strings']] <- gedit("NA", cont=tbl))

    visible(tbl) <- TRUE

    makePreview = function(...) {
      ## read in
      l <- lapply(.$args, svalue)
      l$nrows = 10
      df= try(do.call(.$FUN,l), silent=TRUE)
      if(!inherits(df,"try-error")) {
        delete(.$og,.$ig)
        .$ig <- ggroup(horizontal=FALSE, cont=.$og, expand=TRUE)
        tmp <- gdf(df,cont=.$ig) ## get rownames
##         enabled(tmp) <- FALSE ## too faint
      }
    }

    ## do names?
    f = gframe("preview", cont=g, expand=TRUE)
    .$og = ggroup(cont=f, expand=TRUE)
    .$ig = ggroup(cont=.$og, expand=TRUE)                # to be deleted
    makePreview()

    ## now add handler
    sapply(.$args, function(i) addHandlerChanged(i,handler = makePreview))
  }         
  GUI$read_fwf = function(.,sep) {
    .$FUN = "read.fwf"
    .$args = list(file = gedit(.$filename))

    g = ggroup(horizontal=FALSE, cont=.$container)
    glabel(paste("Read",basename(.$filename),collapse=" "), cont=g)

    tbl <- glayout(cont=g)
    tbl[1,1] <- .$AssignToText
    tbl[1,2] <- (.$varName <- gedit("X", cont=tbl))
    .$varName[] <- ls(envir=.GlobalEnv)
    visible(tbl) <- TRUE

         
    f= gframe("Import", cont=g)
    tbl <- glayout(cont=f)
    tbl[1,1] <- "Header"
    tbl[1,2] <- (.$args[['header']] <- gdroplist(c(FALSE,TRUE), cont=tbl))
    tbl[1,3] <- "Separator"
    tbl[1,4] <- (.$args[['sep']] <- gedit(sep, cont=tbl))
    tbl[2,1] <- "Skip lines"
    tbl[2,2] <- (.$args[["skip"]] <- gspinbutton(0,1000, cont=tbl))
    tbl[2,3] <- "Skip blank lines"
    tbl[2,4] <- (.$args[['blank.lines.skip']] <- gdroplist(c(TRUE,FALSE), cont=tbl))

    visible(tbl) <- TRUE
    f = gframe("Attributes", cont=g)
    tbl <- glayout(cont=f)
#    tbl[1,3] <- "quote"
#    tbl[1,4] <- (.$args[['quote']] <- gedit("\"", cont=tbl))
#    tbl[2,1] <- "Decimal point"
#    tbl[2,2] <- (.$args[["dec"]] <- gdroplist(c(".",","), cont=tbl))
    tbl[1,1] <- "Comment char."
    tbl[1,2] <- (.$args[['comment.char']] <- gedit("#", cont=tbl))
#    tbl[3,1] <- "NA string"
#    tbl[3,2] <- (.$args[['na.strings']] <- gedit("NA", cont=tbl))

    visible(tbl) <- TRUE

    ## widths is key here

    f = gframe("Field widths", cont=g)
    tbl <- glayout(cont=f)
    tbl[1,1] <- "widths"
    tbl[1,2] <- (.$args[["widths"]] <- gedit(paste("c(",nchar(readLines(.$filename,n=1)),")",collapse=""), coerce.with=svalue,cont=tbl))
    visible(tbl) <- TRUE


    makePreview = function(...) {
      ## read in
      l <- lapply(.$args, svalue)
      l$nrows = 10
      df= try(do.call(.$FUN,l), silent=TRUE)
      if(!inherits(df,"try-error")) {
        delete(.$og,.$ig)
        .$ig <- ggroup(horizontal=FALSE, cont=.$og, expand=TRUE)
        tmp <- gdf(df,cont=.$ig) ## get rownames
##         enabled(tmp) <- FALSE ## too faint
      } else {
        cat("Error:",df,"\n")
      }
    }

    ## do names?
    f = gframe("preview", cont=g,expand=TRUE)
    .$og = ggroup(cont=f, expand=TRUE)
    .$ig = ggroup(cont=.$og, expand=TRUE)                # to be deleted
    makePreview()

    ## now add handler
    sapply(.$args, function(i) addHandlerChanged(i,handler = makePreview))

  }
  GUI$read_DIF = function(.) {
    .$FUN = "read.DIF"
    .$args = list(file = gedit(.$filename))

    g = ggroup(horizontal=FALSE, cont=.$container)
    glabel(paste("Read",basename(.$filename),collapse=" "), cont=g)

    tbl <- glayout(cont=g)
    tbl[1,1] <- .$AssignToText
    tbl[1,2] <- (.$varName <- gedit("X", cont=tbl))
    .$varName[] <- ls(envir=.GlobalEnv)
    visible(tbl) <- TRUE

         
    f= gframe("Import", cont=g)
    tbl <- glayout(cont=f)
    tbl[1,1] <- "Header"
    tbl[1,2] <- (.$args[['header']] <- gdroplist(c(FALSE,TRUE), cont=tbl))
    tbl[2,1] <- "Skip lines"
    tbl[2,2] <- (.$args[["skip"]] <- gspinbutton(0,1000, cont=tbl))
    tbl[2,3] <- "Skip blank lines"
    tbl[2,4] <- (.$args[['blank.lines.skip']] <- gdroplist(c(TRUE,FALSE), cont=tbl))
    tbl[3,1] <- "NA string"
    tbl[3,2] <- (.$args[['na.strings']] <- gedit("NA", cont=tbl))
    tbl[3,3] <- "Strings as factors"
    tbl[3,4] <- (.$args[['stringsAsFactors']] <- gdroplist(c(TRUE,FALSE),cont=tbl))
    visible(tbl) <- TRUE


    makePreview = function(...) {
      ## read in
      l <- lapply(.$args, svalue)
      l$nrows = 10
      df= try(do.call(.$FUN,l), silent=TRUE)
      if(!inherits(df,"try-error")) {
        delete(.$og,.$ig)
        .$ig <- ggroup(horizontal=FALSE, cont=.$og, expand=TRUE)
        tmp <- gdf(df,cont=.$ig) ## get rownames
##         enabled(tmp) <- FALSE ## too faint
      } else {
        cat("Error:",df,"\n")
      }
    }

    ## do names?
    f = gframe("preview", cont=g, expand=TRUE)
    .$og = ggroup(cont=f, expand=TRUE)
    .$ig = ggroup(cont=.$og, expand=TRUE)                # to be deleted
    makePreview()

    ## now add handler
    sapply(.$args, function(i) addHandlerChanged(i,handler = makePreview))

  }    

  GUI$read_foreign = function(.,type) {
    .$FUN = paste("read.",type,sep="",collapse="")
    .$args = list(file=gedit(.$filename)) # all have file as first arg


    fileType = names(fileExtensions)[sapply(fileExtensions,function(i) .$ext %in% i)]
        ## strip s
    g = ggroup(horizontal=FALSE, cont=.$container)

    glabel(paste("Read",basename(.$filename),"as",popchar(fileType),collapse=" "),
           cont=g)
    tbl = glayout(cont=g)
    tbl[1,1] <- .$AssignToText
    tbl[1,2] <- (.$varName <- gedit("X", cont=tbl))
    .$varName[] <- ls(envir=.GlobalEnv)

    fmls = formals(get(.$FUN))
    nfmls = names(fmls)
    n <- length(nfmls)
    ## add extra arguments without thinking too much
    if(n > 1) {
      for(i in 2:n) {
        tbl[i,1] <- nfmls[i]
        tbl[i,2] <- (.$args[[nfmls[i]]] <-
                     gedit(fmls[[i]], cont=tbl,
                           coerce.with = paste("as.",class(fmls[[i]]),sep="", collapse="")
                           ))
        
      }
    }
    
    visible(tbl) <- TRUE

  }

  ## show GUI$show()
  GUI$show()
}
