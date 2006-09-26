
## called on startup
## .First.lib = function(...) {
##   cat("\n\nP M G\n")
##   cat("To restart pmg, use the command:\n")
##   cat("pmg()\n")

##   pmg()
## }

## when we have a namespace, we can do this.
.onLoad <- function(...){
  cat("Loading pmg()\n")
  pmg()
}
