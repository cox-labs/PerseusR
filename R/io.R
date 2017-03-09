library(plyr)

#' Create annotation rows
#'
#' Create the annotation rows data.frame from the list
#' of comment rows parsed from the input file and the main columns indicator
#' @param commentRows list of comment rows
#' @param isMain logical array indicating all main columns
#' @seealso used by \code{\link{read.perseus}}
create_annotRows <- function(commentRows, isMain) {
  annotRows <- list()
  for(name in names(commentRows)) {
    if (startsWith(name, 'C:')) {
      annotRows[[substring(name, 3)]] <- factor(commentRows[[name]][isMain])
    }
    else {
      warning("Found unrecognized annotation row: ", name)
    }
  }
  return(as.data.frame(annotRows))
}

#' Read Perseus matrix files
#'
#' Read the custom Perseus matrix file format *.txt into R.
#' @note Limitations to column names in R still apply. Column names valid
#' in Perseus, such as 'Column 1' will be changed to 'Column.1'
#' @export
#' @param con A \code{\link{connection}} object or the path to input file
#' @return DataFrame with additional 'annotationRows' attribute
#' @seealso \code{\link{write.perseus}}
#' @note If the provided connection \code{con} is a character string, it will assumed
#' to be a file path. A \code{\link{connection}} which is not seekable (see \code{\link{isSeekable}})
#' will be written to a temporary file. Any connection will be closed when \code{read.perseus} exits.
#' @examples
#' testFile <- system.file('extdata', 'matrix1.txt', package='PerseusR')
#' mdata <- read.perseus(con=testFile)
read.perseus <- function(con) {
  if (is.character(con)) {
    con <- file(con, open='r')
  }
  else if (!isSeekable(con)) {
    fileCon <- file()
    writeLines(readLines(con), fileCon)
    close(con)
    con <- fileCon
  }
  columns <- strsplit(readLines(con, n=1), '\t')[[1]]
  nCols <- length(columns)
  commentRows <- list()
  while(startsWith(oneLine <- readLines(con, n=1), '#!')) {
    name <- strsplit(substring(oneLine, 4), '}')[[1]][1]
    rowStr <- substring(oneLine, nchar(name) + 5)
    rowValues <- strsplit(rowStr, '\t')[[1]]
    commentRows[[name]] <- rowValues
  }
  types <- commentRows$Type
  descr <- commentRows$Description
  commentRows[c('Type', 'Description')] <- NULL
  typeMap <- list(Perseus=c('E', 'N', 'C', 'T'),
                    R=c('numeric', 'numeric', 'factor', 'character'))
  colClasses <- map_perseus_types(types, typeMap)
  seek(con, 0)
  df <- utils::read.table(con, header = TRUE, sep = '\t', comment.char = '#',
                          colClasses = colClasses)
  close(con)
  isMain <- types == 'E'
  main <- df[isMain]
  annotCols <- df[!isMain]
  annotRows <- create_annotRows(commentRows, isMain)
  if (is.null(descr)) {
    descr <- character(0)
  }
  return(matrixData(main = main, annotCols = annotCols,
                    annotRows = annotRows, description = descr))
}


map_perseus_types <- function(typeAnnotation, typeMap) {
  plyr::mapvalues(typeAnnotation, from=typeMap$Perseus, to=typeMap$R, warn_missing=FALSE)
}

#' Infer Perseus type annotation row from DataFrame column classes
#'
#' @param df The data.frame
#' @param typeMap A list with elements 'Perseus' and 'R'. The ordering determines the mapping
#' @return A vector with perseus type annotations
#' @seealso Based on \code{\link{mapvalues}}
infer_perseus_annotation_types <- function(df, typeMap) {
  colClasses <- as.vector(sapply(df, class))
  if (length(colClasses) == 0) return(colClasses)
  plyr::mapvalues(colClasses, from=typeMap$R, to=typeMap$Perseus, warn_missing=FALSE)
}

#' Write matrixData in Perseus matrix format
#'
#' Write a matrixData to file in the custom Perseus matrix file format.
#'
#' @param mdata The matrixData
#' @param con A \code{\link{connection}} object or the path to output file
#' @seealso \code{\link{read.perseus}} \code{\link{matrixData}}
#' @export
write.perseus <- function(mdata, con) {
  typeMap <- list(Perseus=c('N', 'C', 'T'),
                  R=c('numeric', 'factor', 'character'))
  closeAtEnd <- FALSE
  if (is.character(con)) {
    con <- file(con, open='w')
    closeAtEnd <- TRUE
  }
  columns <- names(mdata)
  writeLines(paste0(columns, collapse='\t'), con)
  descr <- description(mdata)
  if (length(descr) != 0) {
    descr[1] <- paste0('#!{Description}', descr[1])
    writeLines(paste0(descr, collapse='\t'), con)
  }
  type <- c(rep('E', ncol(main(mdata))), infer_perseus_annotation_types(annotCols(mdata), typeMap))
  type[1] <- paste0('#!{Type}', type[1])
  writeLines(paste0(type, collapse = '\t'), con)
  annotationRows <- as.list(annotRows(mdata))
  for (name in names(annotationRows)) {
    values <- paste0(annotationRows[[name]], collapse='\t')
    writeLines(sprintf('#!{C:%s}%s', name, values), con)
  }
  main <- main(mdata)
  annotCols <- annotCols(mdata)
  df <- main
  if (nrow(annotCols) != 0) {
    if (nrow(main) == 0) {
      df <- annotCols
    }
    else {
      df <- cbind(main, annotCols)
    }
  }
  utils::write.table(df, con, sep='\t', quote=FALSE,
                     row.names=FALSE, col.names=FALSE,
                     na='NaN')
  if (closeAtEnd) close(con)
  return()
}
