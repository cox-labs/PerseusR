# PerseusR

[![Travis-CI Build Status](https://travis-ci.org/jdrudolph/PerseusR.svg?branch=master)](https://travis-ci.org/jdrudolph/PerseusR)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/PerseusR)](https://cran.r-project.org/package=PerseusR)


Convenience functions for interop between Perseus and R.

Designed to work with the [PluginInterop](https://github.com/jdrudolph/PluginInterop) plugin
for the Perseus framework.

## Citation

If you use `PerseusR` in your projects, please cite

Rudolph, J D and Cox, J 2018, *A network module for the Perseus software for computational proteomics facilitates proteome interaction graph analysis* [doi:10.1101/447268](https://doi.org/10.1101/447268)

# Installation

Make sure to have `R >= 3.5.0` installed. Paste the following lines
into an running `R` session. You can skip the comment lines starting with `#`.

```{R}
# installing BioConductor dependencies first
install.packages('BiocManager')
BiocManager::install('Biobase')

# installing PerseusR
install.packages('PerseusR')
```

# Usage

`PerseusR` provides two functions for reading and writing files from/to Perseus.
You can use them to write simple scripts which can be used as
`MatrixProcessing` activities in Perseus. Additionally you can parse Perseus
parameters and extract their values.

an example R script that could be called though the Perseus plugin:

```{R}
# Parse command line arguments passed in from Perseus,
# including input file and output file paths.
args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
	stop("Do not provide additional arguments!", call.=FALSE)
}
inFile <- args[1]
outFile <- args[2]


# Use PerseusR to read and write the data in Perseus text format.
library(PerseusR)
mdata <- read.perseus(inFile)

# The mdata object can be easily deconstructed into a number of different
# data frames. Check reference manual or help() for full list.
mainMatrix <- main(mdata)

# Run any kind of analysis on the extracted data.
df <- mainMatrix + 1

# Create a matrixData object which can be conveniently written to file
# in the Perseus txt format.
outMdata <- matrixData(main=df)
write.perseus(outMdata, outFile)
```

# Licensing and contributions
`PerseusR` is licensed under the MIT license. Contributions are welcome.
