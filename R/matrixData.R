checkMatrixData <- function(object) {
  errors <- character()

  numCols <- sapply(object@main, is.numeric)
  if (!all(numCols)) {
    msg <- paste('Main columns should be numeric: Columns',
                 paste(names(which(!numCols)), sep=','), 'are not numeric')
    errors <- c(errors, msg)
  }

  if (ncol(object@annotRows) > 0) {
    catAnnotRows <- sapply(object@annotRows, is.factor)
    if (!all(catAnnotRows)) {
      msg <- paste('Annotation rows should be factors: Rows',
                   paste(names(which(!catAnnotRows)), sep=','), 'are not factors')
      errors <- c(errors, msg)
    }

    nColMain <- ncol(object@main)
    nColAnnotRows <- nrow(object@annotRows)
    if (nColMain != nColAnnotRows) {
      msg <- paste('Size of annotation rows not matching:',
                   nColMain, 'main columns, but',
                   nColAnnotRows, 'annotations')
      errors <- c(errors, msg)
    }
  }

  nMain <- nrow(object@main)
  nAnnot <- nrow(object@annotCols)
  if (nAnnot > 0 && nMain > 0 && nMain != nAnnot) {
    msg <- paste('Number of rows not matching:',
                 nMain, 'rows in main data, but',
                 nAnnot, 'rows in annotation columns.')
    errors <- c(errors, msg)
  }

  if (length(object@description) != length(names(object))) {
    msg <- paste('Discriptions do not fit columns, found',
                 length(object@description), 'expected', length(names(object)))
  }

  if (length(errors) == 0) TRUE else errors
}

#' MatrixData class
#' @export
setClass("matrixData",
         slots = c(main="data.frame",
                   annotCols="data.frame",
                   annotRows="data.frame",
                   description="character"),
         validity = checkMatrixData)
#' matrixData constructor
#' @export
matrixData <- function(...) {
  new("matrixData", ...)
}

getNames <- function(x) {c(colnames(x@main), colnames(x@annotCols))}
setMethod("names", "matrixData", getNames)
#' Column names of main and annotation columns
#' @export
names.matrixData <- getNames

#' Get main columns
#' @export
main <- function(mdata) {
  mdata@main
}

#' Set main columns
#' @export
`main<-` <- function(mdata, value) {
  mdata@main <- value
  validObject(mdata)
  return(mdata)
}

#' Get annotation columns
#' @export
annotCols <- function(mdata) {
  mdata@annotCols
}

#' Set annotation columns
#' @export
`annotCols<-` <- function(mdata, value) {
  mdata@annotCols <- value
  validObject(mdata)
  return(mdata)
}

#' Get annotation rows
#' @export
annotRows <- function(mdata) {
  mdata@annotRows
}

#' Set annotation rows
#' @export
`annotRows<-` <- function(mdata, value) {
  mdata@annotRows <- value
  validObject(mdata)
  return(mdata)
}

#' Get column description
#' @export
description <- function(mdata) {
  mdata@description
}

#' Set column description
#' @export
`description<-` <- function(mdata, value) {
  mdata@description <- value
  validObject(mdata)
  return(mdata)
}

setMethod("Ops", signature(e1="matrixData", e2="matrixData"),
          function(e1, e2) {
            e1@main <- callGeneric(main(e1), main(e2))
            validObject(e1)
            return(e1)
          }
)
setMethod("Ops", signature(e1="matrixData", e2="numeric"),
          function(e1, e2) {
            e1@main <- callGeneric(main(e1), e2)
            validObject(e1)
            return(e1)
          }
)
setMethod("Ops", signature(e1="numeric", e2="matrixData"),
          function(e1, e2) {
            e1@main <- callGeneric(e1, main(e2))
            validObject(e1)
            return(e1)
          }
)
