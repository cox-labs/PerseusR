context("testCohersionMethods")

test_that("can coherse matrixData to ExpressionSet", {
  mD <- matrixData(
    main=data.frame(a=1:3, b=6:8),
    annotCols=data.frame(b=c('a','b','c')),
    annotRows=data.frame(x=factor(c('1','1'))))
  expect_s4_class(as(mD, "ExpressionSet"), "ExpressionSet")
})

test_that("can ExpressionSet to matrixData", {
  eSet <- Biobase::ExpressionSet(matrix(1:10, ncol = 2))
  expect_s4_class(as(eSet, "matrixData"), "matrixData")
})


# TODO: check if the fact that the rownames are different is relevant
test_that("matrixData coherced back and forth to Expression set is identical", {
  mD <- matrixData(
    main=data.frame(a=1:3, b=6:8),
    annotCols=data.frame(b=c('a','b','c')),
    annotRows=data.frame(x=factor(c('1','1'))))

  eSet2 <- as(mD, "ExpressionSet")
  mD2 <- as(eSet2, "matrixData")
  expect_s4_class(mD2, "matrixData")
  expect_true(all(main(mD) == main(mD2)))
})


