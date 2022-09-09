# PerseusR

[![Travis-CI Build Status](https://travis-ci.org/cox-labs/PerseusR.svg?branch=master)](https://travis-ci.org/cox-labs/PerseusR)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/PerseusR)](https://cran.r-project.org/package=PerseusR)


This repository contains the source code of the `PerseusR` software package.
`PerseusR` contains convenience functions which allow for faster and easier development
of plugins for [Perseus](https://maxquant.org/perseus) in the R programming language.
This page contains developer information on `PerseusR`, for high-level information please
refer to the manuscript listed below.

`PerseusR` was designed to work in conjunction with the [PluginInterop](https://github.com/cox-labs/PluginInterop)
plugin, but can also be used stand-alone.

## Citation

If you use `PerseusR` in your projects, please cite

Rudolph, J D and Cox, J 2018, *A network module for the Perseus software for computational proteomics facilitates proteome interaction graph analysis* [doi:10.1101/447268](https://doi.org/10.1101/447268)

# Installation

Make sure to have `R >= 3.5.0` installed. Paste the following lines
into an running `R` session. You can skip the comment lines starting with `#`.

```{R}
install.packages(“BiocManager”)
BiocManager::install(“Biobase”)

# installing devtools first
install.packages("devtools")

# install PerseusR via devtools
library(devtools)
install_github("cox-labs/PerseusR")
```

# Developing plugins

Perseus provides activities to call R scripts from within the workflow via
[PluginInterop](https://github.com/cox-labs/PluginInterop), e.g. `Matrix => R`.
Developing a plugin therefore translates to writing an R script that follows
a small set of conventions. By adhering to these conventions, Perseus will be
able to successfully communicate with R and transfer inputs and results between
the programs. `PerseusR` provides the neccessary functions to make plugin development
in R easy and straight forward.

This example R script adds 1 to all main columns in the matrix. While its functionality
is very simple. It can serve as a starting point for more extensive scripts.

```{R}
# All plugins can be split into 3 parts
# 1. Reading the command line arguments provided by Perseus and parsing the data.
# 2. Perform the desired functionality on the data.
# 3. Write the results to the expected locations in the Perseus formats.

# 1. Parse command line arguments passed in from Perseus,
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

# 2. Run any kind of analysis on the extracted data.
df <- mainMatrix + 1

# 3. Create a matrixData object which can be conveniently written to file
# in the Perseus txt format.
outMdata <- matrixData(main=df)
write.perseus(outMdata, outFile)
```

# Licensing and contributions
`PerseusR` is licensed under the MIT license. Contributions are welcome.
