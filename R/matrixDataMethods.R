#' @importFrom Biobase ExpressionSet
NULL

#' Coerces a MatrixData into an ExpressionSet
#'
#' Coerces a MatrixData object into an ExpressionSet object
#'
#' function to convert a \code{\link[PerseusR]{matrixData}} \code{\link[Biobase]{ExpressionSet}}
#'
#' @param mdata a \code{\link[PerseusR]{matrixData}} object
#'
#' @return returns an \code{\link[Biobase]{ExpressionSet}} object
#' @importFrom methods as new
#' @inheritParams main
#' @export
#'
#' @examples
#'
#'\donttest{
#' mD <- matrixData(
#' main=data.frame(a=1:3, b=6:8),
#' annotCols=data.frame(b=c('a','b','c')),
#' annotRows=data.frame(x=factor(c('1','1'))))
#'
#' eSet <- as(mD, "ExpressionSet")
#' print(eSet)
#' }
as.ExpressionSet.matrixData <- function(mdata) {

  if (!requireNamespace("Biobase", quietly = TRUE)) {
    stop('This function requires the Biobase package, please install it in the bioconductor repository')
  }

  # Expression Data matrix
  mainData <- data.matrix(main(mdata))
  colNames <- colnames(mainData)
  rowNames <- annotCols(mdata)[['Name']]

  # Make main matrix

  rownames(mainData) <- rowNames

  # Pheno Data collects annotations on the colluns (samples/runs/conditions)
  # (which means it is actually a row)
  phenoData <- annotRows(mdata)
  rownames(phenoData) <- colNames

  phenoData <- methods::new(
    'AnnotatedDataFrame',
    phenoData)

  # Feature Data collects annotations on the rows (peptides/spectra/proteins ...)
  # (which means it is actually a collumn)
  featureData <- annotCols(mdata)
  rownames(featureData) <- rowNames

  featureData <- methods::new(
    'AnnotatedDataFrame',
    featureData)

  eSet <- Biobase::ExpressionSet(
    assayData = mainData,
    phenoData = phenoData,
    annotation = PerseusR::description(mdata),
    featureData = featureData)
  return(eSet)

}


setAs("matrixData", "ExpressionSet",
      function(from) as.ExpressionSet.matrixData(from))


#' Coerces an ExpressionSet into a MatrixData
#'
#' function to convert an \code{\link[Biobase]{ExpressionSet}} object into a
#' \code{\link[PerseusR]{matrixData}}
#'
#' @param ExpressionSet an \code{\link[Biobase]{ExpressionSet}} object
#' @inheritParams main
#'
#' @return returns a \code{\link[PerseusR]{matrixData}} object
#' @importFrom methods as new
#' @export
#'
#' @examples
#'
#'\donttest{
#' eSet <- eSet <- Biobase::ExpressionSet(matrix(1:10, ncol = 2))
#' mD <- as(eSet, "matrixData")
#' print(mD)
#' }
as.matrixData.ExpressionSet <- function(ExpressionSet) {

  if (!requireNamespace("Biobase", quietly = TRUE)) {
    stop('This function requires the Biobase package, please install it in the bioconductor repository')
  }

  mdata <- matrixData(
    main=data.frame(Biobase::exprs(ExpressionSet)),
    annotRows=methods::as(ExpressionSet@phenoData, 'data.frame'),
    description=Biobase::annotation(ExpressionSet),
    annotCols=methods::as(ExpressionSet@featureData, 'data.frame'))

  return(mdata)
}

setAs("ExpressionSet", "matrixData",
      function(from) as.matrixData.ExpressionSet(from))
