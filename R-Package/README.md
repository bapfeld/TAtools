This is an R package with a series of functions useful for University of Texas
at Austin online course TAs. Please read all documentation carefully and proceed with caution!

Important note about working with copies of the Canvas gradebook:
When R imports your gradebook csv file, it will replace any spaces in variable names with a period. You can avoid this by setting check.name = F as an option in the read.csv command. This may solve a number of read/write issues if you try to upload any changes you make to Canvas. HOWEVER, the functions in this package are not built to accommodate this. Particularly, several functions refer to the "SIS.USER.ID" variable in a gradebook data frame. If you do not have this variable, the functions will not work. 

To install this package, run devtools::install_github("bapfeld/TAtools", subdir="R-Package")).  Alternatively, navigate up one level in this repo, download the most recent package source file and install using install.packages("~/path/to/file", repos = NULL, type = "source").
