\name{pmgRepeatTrials}
\alias{pmgRepeatTrials}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A function to simplify simulations }
\description{
  A simple function to repeat an expression several times as an aid to
  simplifying simulations.
}
\usage{
pmgRepeatTrials(expr, n = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{An R expression, such as \code{rnorm(1)} or
  \code{\{x <- rnorm(10); t.test(x)\$p.value\}} that will be repeated \code{n} times.}
  \item{n}{Number of times to repeat the expressions. The default is 10.}
}
\details{
  This functions aids in doing simulations. Rather than explicitly write
  a \code{for} loop or use \code{sapply} this function will call
  \code{sapply} on the expression.

  A GUI for this appears in pmg under the Simluation tab. The "quick
  action" will call the function on the results of the simulation.
}
\value{
  The output of a \code{sapply} call can be a vector, matrix, ... If it
  is a vector, it is transposed/
}
\author{John Verzani}
\note{This function and GUI was suggested by Daniel Kaplan at useR!2007}
\examples{
  res <- pmgRepeatTrials(rnorm(1))
  hist(res)

  g = data.frame(
   father = c(78.5, 78.5, 77.5, 76.0, 75.5),
   mother = c(67.0, 68.0, 66.0, 65.5, 62.0),
   sex    = c("M", "M",  "F",  "F",  "M"),
   nkids  = c(4,     4,    1,    2,    5)
  )
  res <- pmgRepeatTrials(coef(lm(father~ sex + sample(nkids),data=g)),100)
  print(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ datagen }

