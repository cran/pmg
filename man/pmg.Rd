\name{pmg}
\alias{pmg}
\alias{pmg.add}
\alias{pmg.gw}
\alias{pmg.addMenubar}
\alias{pmg.eval}

\title{A function to start the pmg GUI}
\description{
	The PMG GUI is a simple GUI for R using RGtk2 as the
	graphical toolkit. The GUI is written using the
	\code{gWidgets} interface to a toolkit.

}
\usage{
pmg(cliType="console",  width=850, height=.75*width,guiToolkit="RGtk2")

pmg.add(widget,label)

pmg.gw(lst, label=NULL)

pmg.addMenubar(menulist)

pmg.eval(command, assignto=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cliType}{Where to send output of function called within pmg?
  This can be either "console" to put output into console that called
  pmg, or "GUI" to put output into a widget.}
  \item{width}{Width in pixels of initial window}
  \item{height}{height in pixels of initial window}
  \item{guiToolkit}{Specify toolkit to use with gWidgets}
  \item{widget}{A gWidgets widget to add to the main notebook for
    holding dialogs}
  \item{label}{A string containing a label to put on the tab when adding
    a widget to the main notebook for
    holding dialogs}
  \item{lst}{A value passed to \code{ggenericwidget}. Can be a list, a
    function name or a function}
  \item{menulist}{A list passed to \code{gmenu} for adding to the
    menubar}
  \item{command}{A string containing a command to be parsed and
    evaluated in the global environement}
  \item{assignto}{If non-NULL, a variable name to assign the output
  generated from evaluating the command}
}
\details{
	The user can add to the menubar at start up time by defining a
	list that is called by \code{gmenu}. PMG look for a variable
	\code{pmg.user.menu}. This is a list with named
	components. Each name becomes the menubar entry top level, and
	each component is called by \code{gmenu} to populate the
	menubar entry.


	The functions \code{pmg.add}, \code{pmg.gw}, \code{pmg.addMenubar}, and
	\code{pmg.eval} are used to extend the GUI.

	\describe{
	  \item{pmg.add}{This is used to add a widget to the main
	    notebook containing the dialogs}
	  \item{pmg.gw}{This is used to add a \code{ggenericwidget}
	  instance to the main
	    notebook containing the dialogs. These widgets can be
	    generated from a function name using the values from
	    \code{formals}}
	  \item{pmg.addMenubar}{Used to add  top-level entries to the
	    main menubar}
	  \item{pmg.eval}{Used to send a command, as a string, to the
	  Commands area to be evaluated. Evaluation is done in the
	  global environment.}
	}
      }

      \references{See
	\url{http://www.amstat.org/publications/jse/v16n1/verzani.html}
	for a description of the dynamic dialogs in pmg.
	}
%%\value{}
\author{John Verzani}
\examples{
\dontrun{
## this restarts the GUI if the main window has been closed
pmg()
}
}
\keyword{ interface }% at least one, from doc/KEYWORDS
