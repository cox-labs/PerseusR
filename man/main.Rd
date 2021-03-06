% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrixData.R
\name{main}
\alias{main}
\title{Get main columns}
\usage{
main(mdata)
}
\arguments{
\item{mdata}{matrixData}
}
\description{
Gets the main columns (main matrix) of a \code{\link[PerseusR]{matrixData}}
object as a data.frame object
}
\examples{
mdata <- matrixData(main=data.frame(a=1:3, b=6:8),
annotCols=data.frame(c=c('a','b','c')),
annotRows=data.frame(x=factor(c('1','1'))))
main(mdata)
}
\seealso{
Other matrixData basic functions: \code{\link{annotCols<-}},
  \code{\link{annotCols}}, \code{\link{annotRows<-}},
  \code{\link{annotRows}}, \code{\link{description<-}},
  \code{\link{description}}, \code{\link{imputeData<-}},
  \code{\link{imputeData}}, \code{\link{main<-}},
  \code{\link{matrixData-class}}, \code{\link{matrixData}},
  \code{\link{names,matrixData-method}},
  \code{\link{qualityData<-}}, \code{\link{qualityData}}
}
\concept{matrixData basic functions}
