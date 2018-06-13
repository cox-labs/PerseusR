library(PerseusR)
context('MatrixData')

test_that('you can construct a valid matrixData', {
  df <- matrixData(
    main=data.frame(a=1:3, b=6:8),
    annotCols=data.frame(c=c('a','b','c')),
    annotRows=data.frame(x=factor(c('1','1'))))
  expect_is(df, 'matrixData')

  df <- matrixData(
    main=data.frame(a=1:3, b=6:8),
    annotCols=data.frame(c=c('a','b','c')))
  expect_is(df, 'matrixData')

  df <- matrixData(
    main=data.frame(a=1:3, b=6:8))
  expect_is(df, 'matrixData')
})

test_that('you cannot construct an invalid matrixData', {
  expect_error(
    matrixData(main=data.frame(a=c('a','b','c'))),
    'should be numeric'
  )

  expect_error(
    matrixData(main=data.frame(a=1:3), annotCols=data.frame(b=1:2)),
    'Number of rows not matching'
  )

  expect_error(
    matrixData(
      main=data.frame(a=1:3, b=6:8),
      annotCols=data.frame(b=c('a','b','c')),
      annotRows=data.frame(x=factor(c('1')))),
    'Size of annotation rows not matching'
  )
})

test_that('the column names span main columns and annotation columns',{
  df <- matrixData(
    main=data.frame(a=1:3, b=6:8),
    annotCols=data.frame(b=c('a','b','c')),
    annotRows=data.frame(x=factor(c('1','1'))))

  cn <- names(df)
  expect_equal(cn, c('a','b','b'))
})

test_that('you can add simple annotation columns to an existing matrix data', {
  df <- matrixData(main=data.frame(a=1:3, b=6:8))
  annotCols(df)['test'] <- 'a'
  expect_equal(annotCols(df)[['test']], rep('a', 3))
})

test_that('you should be able to add new columns regardless of the existance of descriptions', {
  df <- matrixData(main=data.frame(a=1:3, b=6:8), description=c('a','b'))
  warning("TODO: test uses workaround!")
  description(df) <- character(0) # TODO: remove this line, test should still pass, then remove warning
  main(df)['c'] <- 9:11
  expect_equal(main(df)[['c']], 9:11)
})
