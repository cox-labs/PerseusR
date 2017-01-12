#PerseusR

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
`MatrixProcessing` activities in Perseus.

```{R}
args = commandArgs(trailingOnly=TRUE)

if (length(args) != 2) {
        stop("Should provide two arguments: inFile outFile", call.=FALSE)
}

inFile <- args[1]
outFile <- args[2]

library(PerseusR)

print(paste('reading from', inFile))
df <- read.perseus(inFile)

print(paste('writing to', outFile))
write.perseus(df, outFile)
```

See `/examples` folder for more examples.
