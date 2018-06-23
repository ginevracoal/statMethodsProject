source("dataset_import.R")
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

n <- 500

# create text files
apply(data[1:n,], 1, 
      function(x) write.table(data.frame(x[2]), 
                              file = paste("../input_readme/",x[1],".txt",sep=""), row.names = FALSE, col.names = FALSE))

# readapt csv file

data1 <- data[1:n,] %>% 
  select(SentenceId, Sentiment) %>% 
  rename(filename = SentenceId) %>% 
  rename(truth = Sentiment) %>% 
  add_column(trainingset = rbinom(n, 1, 0.5))

head(data1)

write.csv(data1, file = "../input_readme/control.csv", row.names = FALSE)

# ======================================================
# first try

output <- undergrad(control="../input_readme/control.csv", sep=",") 
