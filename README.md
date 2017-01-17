#PerseusR
[![Travis-CI Build Status](https://travis-ci.org/jdrudolph/PerseusR.svg?branch=master)](https://travis-ci.org/jdrudolph/PerseusR)

Convenience functions for interop between Perseus and R.

# Installation

Make sure to have the `devtools` package installed

```{R}
library(devtools)
install_github('jdrudolph/perseusr')
```

# Usage

`PerseusR` provides two functions for reading and writing files from/to Perseus.
You can use them to write simple scripts which can be used as
`MatrixProcessing` activities in Perseus. Additionally you can parse Perseus
parameters and extract their values.

```{R}
# if applicable: read command-line arguments
args = commandArgs(trailingOnly=TRUE)
if (length(args) != 3) {
	stop("Should provide two arguments: inFile outFile", call.=FALSE)
}
paramFile <- args[1]
inFile <- args[2]
outFile <- args[3]

library(PerseusR)
# extract parameters
parameters <- parseParameters(paramFile)
networkType <- singleChoiceParamValue(parameters, "Network type")
corFnc <- singleChoiceParamValue(parameters, "Correlation function")
power <- intParamValue(parameters, "Power")
# read data
mdata <- read.perseus(inFile)

# run any kind of analysis
library(WGCNA)
net <- blockwiseModules(t(main(mdata)), power = power, corFnc = corFnc, networkType = networkType)
c1 <- net$dendrograms[[1]]
df <- as.data.frame(cbind(c1$merge, c1$height))
colnames(df) <- c('left', 'right', 'distance')

# save results to matrixData and write to file
outMdata <- matrixData(main=df)
write.perseus(outMdata, outFile)
```
