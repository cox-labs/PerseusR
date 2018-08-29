#' Check perseus compatibility of an object
#'
#' @title MatrixDataCheck: a function to check the validity of an object as a perseus data frame
#'
#' @param object object to chech consistency with perseus data frames
#' @param ... additional arguments passed to the respective method
#' @param main Main Data frame
#' @param annotationRows Rows containing annotation information
#' @param annotationCols Collumns containing annotation information
#' @param descriptions Descriptions of all the columns
#' @param imputeData Is imputed or not
#' @param qualityData quality number
#' @param all_colnames The colnames to be used
#'
#'
#' @return a logical indicating the validity of the object
#' (or series of objects) as a perseus DF or the string of errors
#'
#' @rdname MatrixDataCheck
#'
#' @export
#'
#' @examples
#'
#' require(PerseusR)
#'
#' mat <- matrixData(
#'     main=data.frame(a=1:3, b=6:8),
#'     annotCols=data.frame(c=c('a','b','c')),
#'     annotRows=data.frame(x=factor(c('1','1'))))
#'
#' MatrixDataCheck(mat)
#'
#'
MatrixDataCheck <- function(object, ...) {
  UseMethod("MatrixDataCheck", object)
}


#' @rdname MatrixDataCheck
#' @method MatrixDataCheck default
#'
#' @export
MatrixDataCheck.default <- function(object = NULL,  main,
                                    annotationRows, annotationCols,
                                    descriptions, imputeData, qualityData,
                                    all_colnames, ...) {
  errors <- character()

  # We could consider using a numeric matrix instead of
  # a df as the main matrix (since by default is a single
  # class )

  numCols <- sapply(main, is.numeric)
  if (!all(numCols)) {
    msg <- paste('Main columns should be numeric: Columns',
                 paste(names(which(!numCols)), sep = ','),
                 'are not numeric')
    errors <- c(errors, msg)
  }

  if (ncol(annotationRows) > 0) {
    catAnnotRows <- sapply(annotationRows, is.factor)
    numAnnotRows <- sapply(annotationRows, is.numeric)
    if (!all(catAnnotRows | numAnnotRows)) {
      msg <- paste('Annotation rows should be factors or numeric: Rows',
                   paste(names(which(!(catAnnotRows | numAnnotRows))), sep = ','),
                   'are not factors')
      errors <- c(errors, msg)
    }

    nColMain <- ncol(main)
    nColAnnotRows <- nrow(annotationRows)
    if (nColMain != nColAnnotRows) {
      msg <- paste('Size of annotation rows not matching:',
                   nColMain, 'main columns, but',
                   nColAnnotRows, 'annotations')
      errors <- c(errors, msg)
    }
  }

  nMain <- nrow(main)
  nAnnot <- nrow(annotationCols)

  if (nAnnot > 0 && nMain > 0 && nMain != nAnnot) {
    msg <- paste('Number of rows not matching:',
                 nMain, 'rows in main data, but',
                 nAnnot, 'rows in annotation columns.')
    errors <- c(errors, msg)
  }

  nDescr <- length(descriptions)
  if (nDescr > 0 && nDescr != length(all_colnames)) {
    msg <- paste('Descriptions do not fit columns, found',
                 nDescr, 'expected', length(all_colnames))
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else stop(errors)
}

#' @return \code{NULL}
#'
#' @inheritParams MatrixDataCheck.default
#'
#' @rdname MatrixDataCheck
#' @method MatrixDataCheck matrixData
#'
#' @export
MatrixDataCheck.matrixData <- function(object) {
  mainDF <- object@main
  annotationRows <- object@annotRows
  annotationCols <- object@annotCols
  descriptions <- object@description
  imputeData <- object@imputeData
  qualityData <- object@qualityData
  all_colnames <- c(colnames(mainDF), colnames(annotationCols))

  ret <- MatrixDataCheck.default(main = mainDF,
                         annotationRows = annotationRows,
                         annotationCols = annotationCols,
                         descriptions = descriptions,
                         imputeData = imputeData,
                         qualityData = qualityData,
                         all_colnames = all_colnames)
  return(ret)
}


#' @return \code{NULL}
#'
#' @inheritParams MatrixDataCheck.default
#'
#' @rdname MatrixDataCheck
#' @method MatrixDataCheck list
#'
#' @export
MatrixDataCheck.list <- function(object, ...) {
  stopifnot(is.list(object))
  stopifnot(sum(c('main', 'annotCols') %in% names(object)) > 0)

  slots <- c('main', 'annotRows',
             'annotCols', 'descriptions', 'imputeData',
             'qualityData')
  defaults <- c(
    replicate(3, quote(data.frame())),
    quote(character(
      length = ncol(object$main) + ncol(object$annotationCols))))

  for (element in seq_along(slots)) {
    object[[slots[element]]] <- tryCatch(
      object[[slots[element]]],
      error = function(...) eval(defaults[[element]]) )
  }
  all_colnames <- c(colnames(object$main), colnames(object$annotationCols))
  write.csv(object$imputeData, file='C:\\Users\\shyu\\Documents\\QQQ.txt')
  ret <- MatrixDataCheck.default(main = object$main,
                                 annotationRows = object$annotRows,
                                 annotationCols = object$annotCols,
                                 descriptions = object$descriptions,
                                 imputeData = object$imputeData,
                                 qualityData = object$qualityData,
                                 all_colnames = all_colnames)
  if (is.logical(ret) & ret) {
    return(ret)
  } else {
    stop(ret)
  }
}


#' @return \code{NULL}
#'
#' @inheritParams MatrixDataCheck.default
#'
#' @rdname MatrixDataCheck
#' @method MatrixDataCheck ExpressionSet
#'
#' @export
MatrixDataCheck.ExpressionSet <- function(object, ...) {
  if (!requireNamespace("Biobase", quietly = TRUE)) {
    stop('This function requires the Biobase package, please install it in the bioconductor repository')
  }

  mainDF <- data.frame(Biobase::exprs(object))
  annotationRows <- methods::as(object@phenoData, 'data.frame')
  descriptions <- Biobase::annotation(object)
  annotationCols <- methods::as(object@featureData, 'data.frame')
  all_colnames <- c(colnames(mainDF), colnames(annotationCols))

  ret <- MatrixDataCheck.default(mainDF,
                                 annotationRows,
                                 annotationCols,
                                 descriptions,
                                 all_colnames)
  if (is.logical(ret) & ret) {
    return(ret)
  } else {
    stop(ret)
  }
}


#' MatrixData
#' @slot main Main expression \code{data.frame}.
#' @slot annotCols Annotation Columns \code{data.frame}.
#' @slot annotRows Annotation Rows \code{data.frame}.
#' @slot description Column descriptions.
#' @slot imputeData Isimputed or not \code{data.frame}.
#' @slot qualityData quality number \code{data.frame}.
#'
#' @name matrixData-class
#' @rdname matrixData-class
#' @family matrixData basic functions
#'
#' @export
setClass("matrixData",
         slots = c(main = "data.frame",
                   annotCols = "data.frame",
                   annotRows = "data.frame",
                   description = "character",
                   imputeData = "data.frame",
                   qualityData = "data.frame"),
         validity = MatrixDataCheck.matrixData)

#' matrixData constructor
#' @param ... \code{main}, \code{annotCols}, \code{annotRows}, \code{description}
#' @inherit matrixData-class
#' @family matrixData basic functions
#' @export
matrixData <- function(...) {
  methods::new("matrixData", ...)
}

#' matrixData initializer
#' @description Initializes the annotCols data frame to have the
#' same number of rows as the main data. This might not be the
#' cleanest solution.
setMethod(initialize, "matrixData", function(.Object, ...) {
  args <- list(...)
  if ("main" %in% names(args) && !("annotCols" %in% names(args))) {
    main <- args[['main']]
    args[["annotCols"]] <- data.frame(matrix(nrow=nrow(main), ncol=0))
  }

  args[['.Object']] <- .Object
  do.call(callNextMethod, args)
})


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
#' Gets the main collumns (main matrix) of a \code{\link[PerseusR]{matrixData}}
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
  write.csv(mdata@main, file='C:\\Users\\shyu\\Documents\\NNN.txt')
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

#' Get column imputation
#' @param mdata matrixData
#' @family matrixData basic functions
#' @export
imputeData <- function(mdata) {
  mdata@imputeData
}

#' Set column imputation
#' @param mdata matrixData
#' @param value value
#' @family matrixData basic functions
#' @export
`imputeData<-` <- function(mdata, value) {
  mdata@imputeData <- value
  methods::validObject(mdata)
  return(mdata)
}

#' Get column quality
#' @param mdata matrixData
#' @family matrixData basic functions
#' @export
qualityData <- function(mdata) {
  mdata@qualityData
}

#' Set column imputation
#' @param mdata matrixData
#' @param value value
#' @family matrixData basic functions
#' @export
`qualityData<-` <- function(mdata, value) {
  mdata@qualityData <- value
  methods::validObject(mdata)
  return(mdata)
}

setMethod("Ops", signature(e1 = "matrixData", e2 = "matrixData"),
          function(e1, e2) {
            e1@main <- methods::callGeneric(main(e1), main(e2))
            methods::validObject(e1)
            return(e1)
          }
)
setMethod("Ops", signature(e1 = "matrixData", e2 = "numeric"),
          function(e1, e2) {
            e1@main <- methods::callGeneric(main(e1), e2)
            methods::validObject(e1)
            return(e1)
          }
)
setMethod("Ops", signature(e1 = "numeric", e2 = "matrixData"),
          function(e1, e2) {
            e1@main <- methods::callGeneric(e1, main(e2))
            methods::validObject(e1)
            return(e1)
          }
)
