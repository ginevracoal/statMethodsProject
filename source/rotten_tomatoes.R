setwd("/home/ginevracoal/MEGA/Università/DSSC/semester_2/statistical_methods_for_data_science/statMethodsProject/source")
source("dataset_import.R")
source("plot.R")
library(ReadMe)
library(dplyr)
library(tidyverse)

set.seed(123)

# get labeled dataset
str(train)

# get only whole sentences
data <- train %>%
  group_by(SentenceId) %>% 
  filter(PhraseId == min(PhraseId)) %>% 
  select(SentenceId, Phrase, Sentiment)
head(data)

# number of sentences
nrow(data)

n <- nrow(data)

# create text files
unlink("../input_readme/*")
apply(data[1:n,], 1, function(x) write.table(data.frame(x[2]),  
                                             file = gsub(" ","",paste("../input_readme/",x[1],".txt",sep="")), row.names = FALSE, col.names = FALSE))

# readapt csv file

data1 <- data[1:n,] %>% 
  mutate(filename = paste(SentenceId,".txt", sep="")) %>% 
  rename(truth = Sentiment) %>% 
  add_column(trainingset = rbinom(n, 1, 0.5)) %>% 
  ungroup() %>% 
  select(filename, truth, trainingset)

# write.csv(data1, file = "../input_readme/control.csv", row.names = FALSE, quote=FALSE)
write.table(data1, file = "../input_readme/control.txt", sep = ',',row.names = FALSE, quote=FALSE)

# ======================================================
# first try

oldwd <- getwd()
setwd("../input_readme/")
list.files()

# undergrad
output <- undergrad(sep = ',')

# remove columns with variance 0
preprocess <- preprocess(output)

# readme
results <- readme(undergradlist=preprocess)
str(results)

################
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
  geom_point(aes(color=type))+
  ggtitle(" Computed vs fitted values")+
  # geom_smooth(method=lm)+
  xlab("Real value")+
  ylab("Computed value")+
  theme_minimal()+
  geom_abline(slope = 1,intercept = 0)


setwd(oldwd)
