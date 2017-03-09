library(PerseusR)
context('Input/output')

dataFolder <- system.file('extdata', package = 'PerseusR')
dataFiles <- list.files(dataFolder, pattern = "matrix[[:digit:]]*.txt", full.names=TRUE)
test_that('all the example files are read without error', {
  lapply(dataFiles, read.perseus)
})

test_that('reading and writing out immediately preserves the exact file content', {
  roundtrip <- function(fileName) {
    df <- read.perseus(fileName)
    fileName2 <- paste0(fileName, '.tmp')
    write.perseus(df, fileName2)
    original <- readLines(fileName)
    written <- readLines(fileName2)
    expect_equal(written, original, info=paste('file content of', fileName))
    df2 <- read.perseus(fileName2)
    expect_equal(df2, df, info=paste('read written for', fileName))
    file.remove(fileName2)
  }
  lapply(dataFiles, roundtrip)
})

test_that('reading small example with categorical row works', {
  con <- textConnection("a\tb\n#!{Type}E\tE\n#!{C:site}s1\ts2\n")
  df <- read.perseus(con)
})

test_that('reading and writing from a connection is possible', {
  roundtrip <- function(fileName) {
    df <- read.perseus(fileName)
    con1 <- textConnection('writeOut', 'w')
    write.perseus(df, con1)
    close(con1)
    con2 <- textConnection(writeOut, 'r')
    df2 <- read.perseus(con2)
    expect_equal(df, df2)
  }
  lapply(dataFiles, roundtrip)
})

test_that('writing matrixData without main/annotCols/annotRows/description works', {
  df <- matrixData(
    main=data.frame(a=1:3, b=6:8),
    annotCols=data.frame(b=c('a','b','c')),
    annotRows=data.frame(x=factor(c('1','1'))),
    description=c('a','a','b'))
  con <- textConnection('df1', 'w')
  write.perseus(df, con)
  close(con)
  expect_match(df1[2], "#!\\{Description\\}")
  expect_equal(7, length(df1))

  df <- matrixData(
    main=data.frame(a=1:3, b=6:8),
    annotCols=data.frame(b=c('a','b','c')))
  con <- textConnection('df2', 'w')
  write.perseus(df, con)
  close(con)
  expect_match(df2[2], "#!\\{Type\\}")
  expect_equal(5, length(df2))

  df <- matrixData(
    main=data.frame(a=1:3, b=6:8))
  con <- textConnection('df3', 'w')
  write.perseus(df, con)
  close(con)
  expect_equal(5, length(df3))
})
