\name{xtsubset}
\alias{xtsubset}
\title{Subset a data frame while preserving its \code{\link{xtdata}} attribute}
\usage{
  xtsubset(data, formula, select = names(data),
    drop = FALSE)
}
\arguments{
  \item{data}{a data frame with the \code{\link{xtdata}}
  attribute}

  \item{formula}{a logical formula to subset to.}

  \item{select}{the names of the variables to keep.}

  \item{drop}{passed on to \code{[} indexing operator.}

  \item{...}{other methods passed to \code{\link{subset}}}
}
\value{
  a data frame
}
\description{
  A wrapper of the \code{\link{subset}} function that
  preserves the \code{\link{xtdata}} attribute.
}
\details{
  The method is explained at
  \url{https://github.com/hadley/devtools/wiki/Computing-on-the-language#non-standard-evaluation-in-subset}.
}
\examples{
# Load QOG demo datasets.
data(qog.demo)
# Subset to two countries.
QOG = xtsubset(qog.ts.demo, cname \%in\% c("China", "India"))
if(require(ggplot2)) {
  # Plot log-population curves.
  qplot(data = QOG, y = unna_pop, x = year, colour = cname, geom = "step") +
    scale_colour_brewer(palette = "Set1") +
    scale_y_log10()
}
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\seealso{
  \code{\link{xtdata}}, \code{\link{subset}}
}
\keyword{xt}

