#' @title Converts a MatrixData ExpressionSet
#'
#' Converts a MatrixData ExpressionSet
#'
#' function fo convert a \code{\link[matrixData]{PerseusR}} \code{\link[ExpressionSet]{Biobase}}
#'
#' @param mdata a \code{\link[matrixData]{PerseusR}} object
#'
#' @return an ExpressionSet object
#' @importClassesFrom Biobase AnnotatedDataFrame ExpressionSet
#' @inheritParams main
#' @export
#' @aliases as.ExpressionSet.matrixData as.ExpressionSet.matrixData_
#'
#' @examples
#' mD <- matrixData(
#' main=data.frame(a=1:3, b=6:8),
#' annotCols=data.frame(b=c('a','b','c')),
#' annotRows=data.frame(x=factor(c('1','1'))))
#'
#' eSet <- as(mD, "ExpressionSet")
#' print(eSet)
as.ExpressionSet.matrixData_ <- function(mdata) {

  if (!requireNamespace("Biobase", quietly = TRUE)) {
    stop('This function requires the Biobase package, please install it in the bioconductor repository')
  }

  # Expression Data matrix
  mainData <- data.matrix(main(mdata))
  colNames <- colnames(mainData)
  rowNames <- annotCols(mdata)[['Name']]

  descriptions <- PerseusR::description(mdata)
  is_main <- rep(F, length(descriptions))
  is_main[seq_len(ncol(mainData))] <- T
  main_descriptions <- descriptions[is_main]
  annot_descriptions <- descriptions[!is_main]

  # Make main matrix

  rownames(mainData) <- rowNames

  # Pheno Data collects annotations on the colluns (samples/runs/conditions)
  # (which means it is actually a row)
  phenoData <- annotRows(mdata)
  rownames(phenoData) <- colNames
  #phenoData$main_descriptions <- main_descriptions

  phenoData <- new(
    'AnnotatedDataFrame',
    phenoData)

  # Feature Data collects annotations on the rows (peptides/spectra/proteins ...)
  # (which means it is actually a collumn)
  featureData <- annotCols(mdata)
  rownames(featureData) <- rowNames
  #annot_descriptions <- data.frame(labelDescription = annot_descriptions)
  #rownames(annot_descriptions) <- names(annotCols(mdata))

  featureData <- new(
    'AnnotatedDataFrame',
    featureData)#,
    #varMetadata = data.frame(labelDescription = annot_descriptions))

  eSet <- Biobase::ExpressionSet(
    assayData = mainData,
    phenoData = phenoData,
    annotation = PerseusR::description(mdata),
    featureData = featureData)#,
    #description = PerseusR::description(mdata))
  return(eSet)

}


#' @importClassesFrom Biobase ExpressionSet
setAs("matrixData", "ExpressionSet",
      function(from) as.ExpressionSet.matrixData_(from))


#' @title Converts a MatrixData ExpressionSet
#' @aliases as.ExpressionSet.matrixData as.ExpressionSet.matrixData_
#' @export
#'
as.ExpressionSet.matrixData <- function(x) {
  as(x, "ExpressionSet")
}


#' Converts a ExpressionSet MatrixData
#'
#' function fo convert an \code{\link[ExpressionSet]{Biobase}} object into a
#' \code{\link[matrixData]{PerseusR}}
#'
#' @param ExpressionSet an \code{\link[ExpressionSet]{Biobase}}
#'
#' @return a \code{\link[matrixData]{PerseusR}} object
#' @aliases as.matrixData.ExpressionSet_ as.matrixData.ExpressionSet
#' @export
#' @importFrom Biobase exprs
#'
#' @examples
#'
#' eSet <- eSet <- Biobase::ExpressionSet(matrix(1:10, ncol = 2))
#' mD <- as(eSet, "matrixData")
#' print(mD)
as.matrixData.ExpressionSet_ <- function(ExpressionSet) {

  if (!requireNamespace("Biobase", quietly = TRUE)) {
    stop('This function requires the Biobase package, please install it in the bioconductor repository')
  }

  mdata <- matrixData(
    main=data.frame(Biobase::exprs(ExpressionSet)),
    annotRows=as(ExpressionSet@phenoData, 'data.frame'),
    description=Biobase::annotation(ExpressionSet),
    annotCols=as(ExpressionSet@featureData, 'data.frame'))

  return(mdata)
}

#' @importClassesFrom Biobase ExpressionSet
setAs("ExpressionSet", "matrixData",
      function(from) as.matrixData.ExpressionSet_(from))


#' @title Converts a MatrixData ExpressionSet
#' @aliases as.ExpressionSet.matrixData as.ExpressionSet.matrixData_
#' @export
#'
as.matrixData.ExpressionSet <- function(x) {
  as(x, "matrixData")
}
