This is an R package with a series of functions useful for University of Texas
at Austin online course TAs. Please read all documentation carefully and proceed with caution!

It is recommended that you preserve spaces in the dataframe column names using either of the following methods. The preferred import method `rio::import("FILENAME")` or the alternative `read.csv("FILENAME", header = T, sep = ",", check.names = FALSE)`. However, the functions should function correctly even if you have periods instead of spaces.   

To install this package, run `devtools::install_github("bapfeld/TAtools", subdir="R-Package"))`.  Alternatively, navigate up one level in this repo, download the most recent package source file and install using `install.packages("~/path/to/file", repos = NULL, type = "source")`.

[^1]: If you do not have rio installed already, install using `install.packages("rio")`.
