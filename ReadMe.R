# BASIC EXAMPLE
library(ReadMe)

# functions inside the package
ls("package:ReadMe")

# change directory
oldwd <- getwd()
list.files()
setwd(system.file("demofiles/clintonposts", package="ReadMe"))

# translate a set of texts stored in a single folder into a list
output <- undergrad(sep = ',') # default threshold = 0.01

# list the elements
length(output)
str(output)

# remove columns with variance 0
preprocess <- preprocess(output)

# compute the proportion of text documents within each of 
# the user-specified categories
results <- readme(undergradlist=preprocess)
str(results)

# restore old working directory
setwd(oldwd)

