\name{pmg-dynamic}
\alias{dModelsDialog}
\alias{dLatticeExplorer}
\alias{dTestsDialog}


\title{"Dynamic" widgets for pmg}
\description{
	We call a widget "dynamic" if it updates itself immediately
	when an event occurs, such as a drag and drop, or a change in
	some value. The dynamic widgets documented here, are meant to
	provide quick, easy (but limited) access to R's modeling
	functions, R's significance tests, and R's lattice functions
}
\usage{
dModelsDialog()
dTestsDialog()
dLatticeExplorer(container = NULL, ...)
}
\arguments{
  \item{container}{A container to attach the object to}
  \item{\dots}{Currently ignored}
}

\details{
	For each "dynamic" widget, the variables can be specified by
	drag and drop, or by editing the widget. The bold-face areas
	of each widget can be edited by clicking on them or by
	dropping values. If the drop value comes from a column of an
	\code{idf} instance, then when that column is edited, the
	dynamic widget is updated. Such variables can not be edited or
	changed. Other variables may, such as writing powers, or
	applying functions.

	The "dynamic" widgets are meant for easy exploration, but not
	for saving of actions. 

	The dModelsDialog shows an interface to \code{lm}, \code{aov},
	and \code{rlm}. The user can only specifiy formulas of the
	type \code{y ~ 1 + x1 + x2 + ... + xn}. Dropping a value on
	"response" changes the response. Dropping a value on the right
	side of the \code{~} adds the term (using \code{+}). If the
	terms are edited by clicking, the values are split on the +
	sign. 

	For each model fit, a drop list allows one to generate several
	of the diagnostic plots.

	The \code{dTestsDialog} offers an interface to most of the
	tests in the \code{stats} package of class \code{ctests}. (The
	\code{chisq.test} is not implemented yet.) Not only can
	variables be dropped, but one can also change, as appropriate,
	the choice of the null, the alternative, etc. Again, the
	bold-face terms may be edited by clicking on them.

	The \code{ilatticeexplorer} function creates a dynamic
	graphing widget based on  \code{lattice} graphics. Up to three
	variables (only 2 for univariate graphs) may be dropped on the
	widget. The order is for univariate graphs: \code{~x} then
	\code{~x | y}. And for bivariate graphs \code{x}, \code{x ~
	y}, \code{x ~ y | z}. The panel functions add to the plots of
	dots by, typically, incorporating some trend line.

}
\value{
	Although there are methods for \code{dModelsDialog}, these
	widgets aren't meant to be interacted with from the command line.
	}
\author{John Verzani}
\note{
	Some of the usability was inspired by the Fathom software.
}
\examples{
\dontrun{
dTestsDialog()
}
}
\keyword{interface }% at least one, from doc/KEYWORDS
