## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(PerseusR)

dataFolder <- system.file('extdata', package = 'PerseusR')
dataFiles <- list.files(dataFolder, 
                        pattern = "matrix[[:digit:]]*.txt",
                        full.names=TRUE)
dataFile <- dataFiles[[1]]

default_output <- read.perseus.default(dataFile)
class(default_output)
print(default_output)

## ------------------------------------------------------------------------
dm_out <- read.perseus.as.matrixData(dataFile)
class(dm_out)
print(dm_out)

## ------------------------------------------------------------------------
main(dm_out)
annotRows(dm_out)
annotCols(dm_out)
PerseusR::description(dm_out) # Biobase has a descr function as well...

## ------------------------------------------------------------------------
require(Biobase)
eSet_out <- read.perseus.as.ExpressionSet(dataFile)
class(eSet_out)
print(eSet_out)

eSet_out@annotation

## ----error=FALSE---------------------------------------------------------

df <- matrixData(
    main = data.frame(a = 1:3, b = 6:8),
    annotCols = data.frame(c = c('a','b','c')),
    annotRows = data.frame(x = factor(c('1','1'))))

MatrixDataCheck(df)

my_error <- try({
  matrixData(
    main = data.frame(a = 1:3, b = 6:8, c = 1:3),
    annotCols = data.frame(c = c('a','b','c')),
    annotRows = data.frame(x = factor(c('1','1'))))
})

print(my_error)


## ---- message=FALSE, warning=FALSE---------------------------------------
my_list <- list(main = data.frame(a = 1:3, b = 6:8),
    annotCols = data.frame(c = c('a','b','c')),
    annotRows = data.frame(x = factor(c('1','1'))))

MatrixDataCheck(my_list)

my_list <- list(main = data.frame(a = 1:3, b = 6:8, c = 1:3),
    annotCols = data.frame(c = c('a','b','c')),
    annotRows = data.frame(x = factor(c('1','1'))))

my_error <- try({
  MatrixDataCheck(my_list)
})
print(my_error)


## ------------------------------------------------------------------------
eSet <- Biobase::ExpressionSet(matrix(1:10, ncol = 2))

MatrixDataCheck(eSet)

## ------------------------------------------------------------------------
# Here you can use any tipe of conection, similar to th base write... functions
# The usage should be fairly similar to the write.table function

tmp.file <- tempfile(fileext="txt")
write.perseus(dm_out, tmp.file)

# which would output somethint like this

cat(readLines(tmp.file), sep = '\n')

## ------------------------------------------------------------------------

my_df <- data.frame(Con1 = 1:3, Con2 = 4:6, An1 = letters[1:3])

tmp.file <- tempfile(fileext="txt")
write.perseus(my_df, con = tmp.file)

cat(readLines(tmp.file), sep = '\n')

## ------------------------------------------------------------------------
my_matrix <- matrix(1:10, ncol = 2, 
                    dimnames = list(letters[11:15], letters[1:2]))
tmp.file <- tempfile(fileext="txt")
write.perseus(my_matrix, con = tmp.file)

cat(readLines(tmp.file), sep = '\n')

## ------------------------------------------------------------------------

my_list <- list(main = data.frame(A = 1:5, B = 6:10),
                annotRows = data.frame(is_control = c(TRUE, FALSE)),
                annotCols = data.frame(Names = letters[1:5]),
                descr = c('something',
                          'something else',
                          'yet another thing'))
tmp.file <- tempfile(fileext="txt")
write.perseus(my_list, con = tmp.file)
cat(readLines(tmp.file), sep = '\n')



## ------------------------------------------------------------------------
eSet <- Biobase::ExpressionSet(matrix(1:10, ncol = 2))
tmp.file <- tempfile(fileext="txt")
write.perseus(eSet, con = tmp.file)
cat(readLines(tmp.file), sep = '\n')


## ------------------------------------------------------------------------
my_matrix <- matrix(1:10, ncol = 2, 
                    dimnames = list(letters[11:15], letters[1:2]))
my_annotations_rows <- data.frame(My_Names = letters[1:2])
my_annotations_cols <- data.frame(My_Genes = letters[11:15])
tmp.file <- tempfile(fileext="txt")
write.perseus(my_matrix, con = tmp.file, 
              annotCols = my_annotations_cols,
              annotRows = my_annotations_rows)

cat(readLines(tmp.file), sep = '\n')

## ------------------------------------------------------------------------
my_matrix <- matrix(1:10, ncol = 2, 
                    dimnames = list(letters[11:15], letters[1:2]))
tmp.file <- tempfile(fileext="txt")
write.perseus(my_matrix, con = tmp.file, 
              annotCols = NULL)

cat(readLines(tmp.file), sep = '\n')

