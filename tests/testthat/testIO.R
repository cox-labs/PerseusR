library(PerseusR)
context('Input/output')

dataFolder <- system.file('extdata', package='PerseusR')
dataFiles <- list.files(dataFolder, full.names=TRUE)
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
