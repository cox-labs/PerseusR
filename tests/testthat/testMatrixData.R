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
