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

  nDescr <- length(object@description)
  if (nDescr > 0 && nDescr != length(names(object))) {
    msg <- paste('Descriptions do not fit columns, found',
                 nDescr, 'expected', length(names(object)))
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors
}


#' MatrixData
#' @slot main Main expression \code{data.frame}.
#' @slot annotCols Annotation Columns \code{data.frame}.
#' @slot annotRows Annotation Rows \code{data.frame}.
#' @slot description Column descriptions.
#'
#' @name matrixData-class
#' @rdname matrixData-class
#' @family matrixData basic functions
#'
#' @export
setClass("matrixData",
         slots = c(main="data.frame",
                   annotCols="data.frame",
                   annotRows="data.frame",
                   description="character"),
         validity = checkMatrixData)

#' matrixData constructor
#' @param ... \code{main}, \code{annotCols}, \code{annotRows}, \code{description}
#' @inherit matrixData-class
#' @family matrixData basic functions
#' @export
matrixData <- function(...) {
  methods::new("matrixData", ...)
}

getNames <- function(x) {c(colnames(x@main), colnames(x@annotCols))}
#TODO: check if it would be better to have a list returned with one element
#having the col names and the other the row names


#' Get names
#'
#' Get the column names of main and annotation columns.
#'
#' @param x matrixData
#' @family matrixData basic functions
#' @export
#' @docType methods
#' @rdname matrixData-methods
setMethod("names", "matrixData", getNames)

#' Column names of main and annotation columns
#' @param x matrixData
#' @export
names.matrixData <- getNames

#' Get main columns
#'
#' Gets the main collumns (main matrix) of a \code{\link[matrixData]{PerseusR}}
#' object as a data.frame object
#'
#' @param mdata matrixData
#' @family matrixData basic functions
#' @export
main <- function(mdata) {
  mdata@main
}

#' Set main columns
#' @param mdata matrixData
#' @param value value
#' @family matrixData basic functions
#' @export
`main<-` <- function(mdata, value) {
  mdata@main <- value
  methods::validObject(mdata)
  return(mdata)
}

#' Get annotation columns
#' @param mdata matrixData
#' @family matrixData basic functions
#' @export
annotCols <- function(mdata) {
  mdata@annotCols
}

#' Set annotation columns
#' @param mdata matrixData
#' @param value value
#' @family matrixData basic functions
#' @export
`annotCols<-` <- function(mdata, value) {
  mdata@annotCols <- value
  methods::validObject(mdata)
  return(mdata)
}

#' Get annotation rows
#' @param mdata matrixData
#' @family matrixData basic functions
#' @export
annotRows <- function(mdata) {
  mdata@annotRows
}

#' Set annotation rows
#' @param mdata matrixData
#' @param value value
#' @family matrixData basic functions
#' @export
`annotRows<-` <- function(mdata, value) {
  mdata@annotRows <- value
  methods::validObject(mdata)
  return(mdata)
}

#' Get column description
#' @param mdata matrixData
#' @family matrixData basic functions
#' @export
description <- function(mdata) {
  mdata@description
}

#' Set column description
#' @param mdata matrixData
#' @param value value
#' @family matrixData basic functions
#' @export
`description<-` <- function(mdata, value) {
  mdata@description <- value
  methods::validObject(mdata)
  return(mdata)
}

setMethod("Ops", signature(e1="matrixData", e2="matrixData"),
          function(e1, e2) {
            e1@main <- methods::callGeneric(main(e1), main(e2))
            methods::validObject(e1)
            return(e1)
          }
)
setMethod("Ops", signature(e1="matrixData", e2="numeric"),
          function(e1, e2) {
            e1@main <- methods::callGeneric(main(e1), e2)
            methods::validObject(e1)
            return(e1)
          }
)
setMethod("Ops", signature(e1="numeric", e2="matrixData"),
          function(e1, e2) {
            e1@main <- methods::callGeneric(e1, main(e2))
            methods::validObject(e1)
            return(e1)
          }
)
