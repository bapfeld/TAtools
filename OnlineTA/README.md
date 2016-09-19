This is an R package with a series of functions useful for University of Texas
at Austin online course TAs. Please read all documentation carefully and proceed with caution!

Important note about working with copies of the Canvas gradebook:
These functions are written for dataframes where the spaces have been preserved in column names. The preferred import method `rio::import("FILENAME")` will give the correct formatting.[^1] Alternatively, you can use `read.csv("FILENAME", header = T, sep = ",", check.names = FALSE)`.   

To install this package, run `devtools::install_github("bapfeld/TAtools", subdir="R-Package"))`.  Alternatively, navigate up one level in this repo, download the most recent package source file and install using `install.packages("~/path/to/file", repos = NULL, type = "source")`.

[^1]: If you do not have rio installed already, install using `install.packages("rio")`.
