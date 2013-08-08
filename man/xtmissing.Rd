\name{xtmissing}
\alias{xtmissing}
\title{Plot availability of panel variables}
\usage{
  xtmissing(data = NULL, variable)
}
\arguments{
  \item{data}{a dataset with the \code{xtdata} attribute.}

  \item{variable}{the variable to plot.}
}
\value{
  a \code{ggplot2} object
}
\description{
  Function to plot data availability in \code{xtdata}
  datasets. Requires the \code{ggplot2} package.
}
\examples{
if(require(ggplot2)) {
  # Load QOG demo datasets.
  data(qog.demo)
  xtmissing(qog.ts.demo, "chga_hinst") +
    ggtitle("Country-year availability of regime type")
  xtmissing(qog.ts.demo, "bl_asy15f") +
    ggtitle("Country-year availability of female education")
}
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}

