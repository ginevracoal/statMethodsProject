# prepare a json dataset for readme
library(jsonlite)
library(dplyr)
library(tidyverse)

json_to_df <- function(filepath){
  # import json
  json_data <- stream_in(file(filepath,open="r"))
  
  # transform into dataframe
  df <- json_data %>%
    select(reviewText,overall,reviewTime) %>%
    add_column(SentenceId=1:length(json_data$reviewText)) %>% 
    mutate(reviewTime = as.Date(reviewTime,format = "%m %d, %Y")) 

  return(df)
}

# export text files based on ID and create control file for Readme package
readme_inputs <- function(df){
  unlink("../input_readme/*")

  apply(df, 1, 
        function(x) write.table(data.frame(x[1]), 
                                file = gsub(" ","", paste("../input_readme/",x[4],sep="")), 
                                row.names = FALSE, col.names = FALSE))

  readme_df <- df %>% 
    rename(Sentiment=overall)%>%
    select(SentenceId, Sentiment) %>% 
    rename(filename = SentenceId) %>% 
    rename(truth = Sentiment) %>% 
    add_column(trainingset = rbinom(n, 1, 0.5)) %>% 
    ungroup()
  
  write.csv(readme_df, file = "../input_readme/control.txt", row.names = FALSE)       
}

