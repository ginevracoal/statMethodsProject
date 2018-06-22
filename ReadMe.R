# BASIC EXAMPLE
library(ReadMe)

# functions inside the package
ls("package:ReadMe")

# change directory
oldwd <- getwd()
list.files()
setwd(system.file("demofiles/clintonposts", package="ReadMe"))

# UNDERGRAD FUNCTION

# translate a set of texts stored in a single folder into a list
output <- undergrad(sep = ',') 
# the default threshold is 0.01, it only works as an upper bound 
# (extremely frequent words)

# list the elements
length(output)
str(output)


# PREPROCESS FUNCTION

# remove columns with variance 0
preprocess <- preprocess(output)

length(preprocess)
str(preprocess)

# we can notice that the function preprocess is removing 3 columns
length(output$trainingset)
length(output$testset)

length(preprocess$trainingset)
length(preprocess$testset)

# why?
# it computes variances for all columns representing a single word
v2 <- apply(as.matrix(undergrad.results$trainingset[, 4:dim(undergrad.results$trainingset)[2]]), 
      2, sd)
v2

# and removes all words with 0 variance, meaning words not 
# comparing in any document
rmc2 <- 3 + which(v2 == 0)
rmc2

# compute the proportion of text documents within each of 
# the user-specified categories
results <- readme(undergradlist=preprocess)
str(results)

# restore old working directory
setwd(oldwd)

