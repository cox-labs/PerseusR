#' Parse parameters
#'
#' Parse parameters from the parameters xml file.
#' @param paramFile Parameters xml file
#' @export
parseParameters <- function(paramFile) {
	return(XML::xmlParse(paramFile))
}

#' Single choice value
#'
#' Extract the value selected in a \code{SingleChoiceParam}.
#' @param parameters The parameters object (see \code{\link{parseParameters}})
#' @param name The name of the parameter
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
#' @export
#' @return The selected number
boolParamValue <- function(parameters, name) {
  value <- as.logical(XML::xmlValue(parameters[[sprintf("//*[@Name='%s']/Value", name)]]))
  return(value)
}
