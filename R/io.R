library(plyr)

#' Read Perseus matrix files
#'
#' Read the custom Perseus matrix file format *.txt into R.
#' @note Limitations to column names in R still apply. Column names valid
#' in Perseus, such as 'Column 1' will be changed to 'Column.1'
#' @export
#' @param inFile path to input file
#' @return DataFrame with additional 'annotationRows' attribute
#' @seealso \code{\link{write.perseus}}
#' @examples
#' testFile <- system.file('extdata', 'matrix1.txt', package='PerseusR')
#' df <- read.perseus(testFile)
#' annotationRows <- attributes(df)$annotationRows
read.perseus <- function(inFile) {
  con <- file(inFile, open='r')
  columns <- strsplit(readLines(con, n=1), '\t')[[1]]
  nCols <- length(columns)
  annotationRows <- list()
  while(startsWith(oneLine <- readLines(con, n=1), '#!')) {
    name <- strsplit(substring(oneLine, 4), '}')[[1]][1]
    rowStr <- substring(oneLine, nchar(name) + 5)
    rowValues <- strsplit(rowStr, '\t')[[1]]
    paddedRowValues <- c(rowValues, rep('', nCols - length(rowValues)))
    annotationRows[[name]] <- paddedRowValues
  }
  close(con)
  colClasses <- map_perseus_types(annotationRows$Type)
  df <- utils::read.table(inFile, header = TRUE, sep = '\t', comment.char = '#',
                          colClasses = colClasses)
  attr(df, "annotationRows") <- annotationRows
  return(df)
}

typeMap <- list(Perseus=c('E', 'N', 'C', 'T'),
                R=c('numeric', 'numeric', 'factor', 'character'))

map_perseus_types <- function(typeAnnotation) {
  plyr::mapvalues(typeAnnotation, from=typeMap$Perseus, to=typeMap$R, warn_missing=FALSE)
}

#' Infer Perseus type annotation row from DataFrame column classes
#'
#' @param df The data.frame
#' @param typeMap A list with elements 'Perseus' and 'R'. The ordering determines the mapping
#' @return A vector with perseus type annotations
#' @seealso Based on \code{\link{mapvalues}}
#' @export
infer_perseus_types <- function(df, typeMap=typeMap) {
  colClasses <- as.vector(sapply(df, class))
  plyr::mapvalues(colClasses, from=typeMap$R, to=typeMap$Perseus)
}

#' Write annotated DataFrame in Perseus matrix format
#'
#' Write a DataFrame to file in the custom Perseus matrix file format.
#' The DataFrame should have a 'annotationRows' attribute set.
#'
#' @param df The DataFrame
#' @param outFile Path to output file
#' @seealso \code{\link{read.perseus}}
#' @export
write.perseus <- function(df, outFile) {
  con <- file(outFile, open='w')
  columns <- colnames(df)
  writeLines(paste0(columns, collapse='\t'), con)
  annotationRows <- attributes(df)$annotationRows
  for (name in names(annotationRows)) {
    values <- paste0(annotationRows[[name]], collapse='\t')
    writeLines(sprintf('#!{%s}%s', name, values), con)
  }
  utils::write.table(df, con, sep='\t', quote=FALSE,
                     row.names=FALSE, col.names=FALSE,
                     na='NaN')
  close(con)
}
