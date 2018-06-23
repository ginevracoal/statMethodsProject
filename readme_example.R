# BASIC EXAMPLE
library(ReadMe)

# functions inside the package
ls("package:ReadMe")

# change directory
oldwd <- getwd()
setwd(system.file("demofiles/clintonposts", package="ReadMe"))
getwd()
list.files()

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
v2 <- apply(as.matrix(preprocess$trainingset[, 4:dim(preprocess$trainingset)[2]]), 
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

library(tidyverse)
valori_fit = results$est.CSMF
valori_veri = results$true.CSMF
#converto in dataframe
valori_fit = as_tibble(as.list(valori_fit))
valori_veri = as_tibble(as.list(valori_veri))
valori_plot=gather(valori_fit,type)
valori_plot_veri=gather(valori_veri,type)
#typeof(results$est.CSMF)
valori_plot_veri
valori_plot
valori_tot=inner_join(valori_plot,valori_plot_veri,by="type")
ggplot(data=valori_tot,aes(x=value.x,y=value.y))+
  geom_point()+
  ggtitle(" Computed vs fitted values")+
  geom_smooth(method=lm)+
  xlab("Real value")+
  ylab("Computed value")+
  theme_minimal()

valori_tot
  
ggplot(data=valori_tot,aes(x=type,y=value.x))+
  geom_col()

ggplot(data=valori_tot,aes(x=type,y=value.y))+
  geom_col()




# restore old working directory
setwd(oldwd)



