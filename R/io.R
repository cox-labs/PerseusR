library(plyr)

#' Create annotation rows
#'
#' Create the annotation rows data.frame from the list
#' of comment rows parsed from the input file and the main columns indicator
create_annotRows <- function(commentRows, isMain) {
  annotRows <- list()
  for(name in names(commentRows)) {
    if (startsWith(name, 'C:')) {
      annotRows[[substring(name, 3)]] <- factor(commentRows[[name]][isMain])
    }
    else {
      warning("Found unrecognized annotation rowi: ", name)
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
#' @param inFile path to input file
#' @return DataFrame with additional 'annotationRows' attribute
#' @seealso \code{\link{write.perseus}}
#' @examples
#' testFile <- system.file('extdata', 'matrix1.txt', package='PerseusR')
#' df <- read.perseus(testFile)
read.perseus <- function(inFile) {
  con <- file(inFile, open='r')
  columns <- strsplit(readLines(con, n=1), '\t')[[1]]
  nCols <- length(columns)
  commentRows <- list()
  while(startsWith(oneLine <- readLines(con, n=1), '#!')) {
    name <- strsplit(substring(oneLine, 4), '}')[[1]][1]
    rowStr <- substring(oneLine, nchar(name) + 5)
    rowValues <- strsplit(rowStr, '\t')[[1]]
    commentRows[[name]] <- rowValues
  }
  close(con)
  types <- commentRows$Type
  description <- commentRows$Description
  commentRows[c('Type', 'Description')] <- NULL
  typeMap <- list(Perseus=c('E', 'N', 'C', 'T'),
                    R=c('numeric', 'numeric', 'factor', 'character'))
  colClasses <- map_perseus_types(types, typeMap)
  df <- utils::read.table(inFile, header = TRUE, sep = '\t', comment.char = '#',
                          colClasses = colClasses)
  isMain <- types == 'E'
  main <- df[isMain]
  annotCols <- df[!isMain]
  annotRows <- create_annotRows(commentRows, isMain)
  return(matrixData(main = main, annotCols = annotCols,
                    annotRows = annotRows, description = description))
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

#' Write annotated DataFrame in Perseus matrix format
#'
#' Write a DataFrame to file in the custom Perseus matrix file format.
#' The DataFrame should have a 'annotationRows' attribute set.
#'
#' @param df The DataFrame
#' @param outFile Path to output file
#' @seealso \code{\link{read.perseus}}
#' @export
write.perseus <- function(mdata, outFile) {
  typeMap <- list(Perseus=c('N', 'C', 'T'),
                  R=c('numeric', 'factor', 'character'))
  con <- file(outFile, open='w')
  columns <- names(mdata)
  writeLines(paste0(columns, collapse='\t'), con)
  description <- description(mdata)
  if (length(description) != 0) {
    description[1] <- paste0('#!{Description}', description[1])
    writeLines(paste0(description, collapse='\t'), con)
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
  close(con)
}
