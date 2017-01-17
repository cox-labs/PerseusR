library(PerseusR)
context('Parameters')

paramFile <- system.file('extdata/parameters1.xml', package = 'PerseusR')
test_that('you can read the parameters file', {
  parameters <- parseParameters(paramFile)
  expect_equal("cor", singleChoiceParamValue(parameters, "Correlation function"))
  expect_equal(5, intParamValue(parameters, "Power"))
})
