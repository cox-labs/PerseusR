#' Parse parameters
#'
#' Parse parameters from the parameters xml file.
#' @param paramFile Parameters xml file
#' @examples
#' write('<IntParam Name="test_int">\n<Value>2</Value>\n</IntParam>', file='tmp.xml')
#' parameters <- parseParameters("tmp.xml")
#' @export
parseParameters <- function(paramFile) {
	return(XML::xmlParse(paramFile))
}

#' Single choice value
#'
#' Extract the value selected in a \code{SingleChoiceParam}.
#' @param parameters The parameters object (see \code{\link{parseParameters}})
#' @param name The name of the parameter
#' @examples
#' write('<SingleChoiceParam Name="test_single">\n<Value>1</Value>\n
#' <Values>\n<Item>A</Item>\n<Item>B</Item>\n</Values>\n</SingleChoiceParam>', file='tmp.xml')
#' parameters <- parseParameters("tmp.xml")
#' singleChoiceParamValue(parameters, "test_single")
#' @export
#' @return The string representing the value
singleChoiceParamValue <- function(parameters, name) {
  value <- as.numeric(XML::xmlValue(parameters[[sprintf("//*[@Name='%s']/Value", name)]]))
  return(XML::xmlValue(parameters[sprintf("//*[@Name='%s']/Values/Item", name)][[value + 1]]))
}

#' Int parameter value
#'
#' Extract the value chosen in an \code{IntParam}
#' @param parameters The parameters object (see \code{\link{parseParameters}})
#' @param name The name of the parameter
#' @examples
#' write('<IntParam Name="test_int">\n<Value>2</Value>\n</IntParam>', file='tmp.xml')
#' parameters <- parseParameters("tmp.xml")
#' intParamValue(parameters, "test_int")
#' @export
#' @return The selected number
intParamValue <- function(parameters, name) {
  as.numeric(XML::xmlValue(parameters[[sprintf("//*[@Name='%s']/Value", name)]]))
}

#' Bool parameter value
#'
#' Extract the value chosen in an \code{BoolParam}
#' @param parameters The parameters object (see \code{\link{parseParameters}})
#' @param name The name of the parameter
#' @examples
#' write('<BoolParam Name="test_bool">\n<Value>false</Value>\n</BoolParam>', file='tmp.xml')
#' parameters <- parseParameters("tmp.xml")
#' boolParamValue(parameters, "test_bool")
#' @export
#' @return The selected boolean
boolParamValue <- function(parameters, name) {
  as.logical(XML::xmlValue(parameters[[sprintf("//*[@Name='%s']/Value", name)]]))
}

#' Single choice index
#'
#' Extract the index chosen in an \code{BoolParam}
#' @param parameters The parameters object (see \code{\link{parseParameters}})
#' @param name The name of the parameter
#' @examples
#' write('<SingleChoiceParam Name="test_single">\n<Value>1</Value>\n
#' <Values>\n<Item>A</Item>\n<Item>B</Item>\n</Values>\n</SingleChoiceParam>', file='tmp.xml')
#' parameters <- parseParameters("tmp.xml")
#' singleChoiceParamInd(parameters, "test_single")
#' @export
#' @return The selected index
singleChoiceParamInd <- function(parameters, name) {
  as.numeric(XML::xmlValue(parameters[[sprintf("//*[@Name='%s']/Value", name)]]))
}
