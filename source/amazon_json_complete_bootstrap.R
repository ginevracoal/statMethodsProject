#json

#set right environment, run only once!
oldwd <- getwd()
setwd("/home/doma/0dssc/smds/statMethodsProject/input_readme/")

#from here rerun how much you want
set.seed(1)
library(ReadMe)
library(rjson)
json_file <- "../dataset/Musical_Instruments_5.json"
json_data <- json_data <- rjson::fromJSON(file=json_file,simplify = FALSE,method = "R")

library("jsonlite")
json_data = stream_in(file("../dataset/Musical_Instruments_5.json",open="r"))

library(tidyverse)
reviews=json_data %>%
  select(reviewText,overall,reviewTime) %>%
  add_column(SentenceId=1:length(json_data$reviewText))

n <- 2000
#n = length(json_data$reviewerID)
# create text files with proper filenames (numbers)
unlink("../input_readme/*")
data=reviews
apply(data[1:n,], 1, 
      function(x) write.table(data.frame(x[1]), 
                              file = gsub(" ","", paste("../input_readme/",x[4],sep="")), row.names = FALSE, col.names = FALSE))



# create control file with same naming conventions as rotten tomatoes
training_perc=0.5
data1 <- data[1:n,] %>% 
  rename(Sentiment=overall)%>%
  select(SentenceId, Sentiment) %>% 
  rename(filename = SentenceId) %>% 
  rename(truth = Sentiment) %>% 
  add_column(trainingset = rbinom(n, 1, training_perc))

head(data1)

write.csv(data1, file = "../input_readme/control.txt", row.names = FALSE)       


#analisi con readme

#step undergrad preprocessing


output <- undergrad(sep = ',') 

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
results <- readme(undergradlist=preprocess,boot.se = TRUE , nboot = 150)
str(results)

################plotting mod
library(tidyverse)
valori_fit = results$est.CSMF
valori_veri = results$true.CSMF
sd_fit = results$CSMF.se
#converto in dataframe
valori_fit = as_tibble(as.list(valori_fit))
valori_veri = as_tibble(as.list(valori_veri))
sd_fit = as_tibble(as.list(sd_fit))
valori_plot=gather(valori_fit,type)
valori_plot_veri=gather(valori_veri,type)
valori_sd=gather(sd_fit,type)
#typeof(results$est.CSMF)
valori_plot_veri
valori_plot
valori_sd
valori_tot=inner_join(valori_plot,valori_plot_veri,by="type",suffix=c(".est",".real"))
valori_tot=inner_join(valori_tot,valori_sd)
valori_tot

ggplot(data=valori_tot,aes(x=value.real,y=value.est,label = value.est))+
  geom_point(aes(color=type))+
  geom_linerange(aes(ymin=value.est-value, ymax=value.est+value,color=type)) +
  ggtitle(" Computed vs fitted values for Amazon music dataset")+
  ggplot2::labs(
  title = "Computed vs fitted values for Amazon music dataset",
    subtitle = paste("total number of elements:",n,"of which training:",100*training_perc,"%"))+
  # geom_smooth(method=lm)+
  xlab("Real value")+
  ylab("Computed value")+
  theme_minimal()+
  geom_abline(slope = 1,intercept = 0)


############################


# restore old working directory
setwd(oldwd)


